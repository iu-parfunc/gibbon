{-# LANGUAGE OverloadedStrings #-}

module Packed.FirstOrder.Passes.Cursorize4
  (cursorize) where

import Data.Loc
import Data.List as L
import Data.Map as M
import Text.PrettyPrint.GenericPretty

import Packed.FirstOrder.Common    hiding (FunDefs, FunDef(..))
import Packed.FirstOrder.L1.Syntax hiding (Prog(..), FunDef(..), FunDefs)
import Packed.FirstOrder.L2.Syntax as L2
import qualified Packed.FirstOrder.L3.Syntax as L3

import Debug.Trace
--------------------------------------------------------------------------------


-- | Cursor insertion, strategy one.
--
-- Here we go to a "dilated" representation of packed values, where
-- every `Packed T` is represented by a pair, `(Cursor,Cursor)`,
-- i.e. start/end.  At least, this is the LOCAL representation of
-- packed values.  The inter-procedural representation does not
-- change.  When passing a packed value to another function, it is the
-- "start" component of the (start,end) pair which is sent.  Likewise
-- end cursors come back traveling on their own.
--
-- REASONING: Why the dilated convention?  In a word: conditionals.  At the
-- end of each function body we need to return the appropriate end cursors.
-- But during the computation, we may need to add an arbitrary amount of
-- extra state to the return type of a conditional.  Thus it's difficult to
-- do this routing of information without changing the types of intermediate
-- expressions significantly.  Dilation is the current strategy.
--
-- We proceed with two loops, corresponding to packed and unpacked
-- context.  When the type of the current expression satisfies
-- `hasPacked`, that's when we're in packed context.  And, when in
-- packed context, we return dilated values.



-- keeps a track of all the `Packed Ty loc` variables in L2 IR. Since all packed types go
-- to the dilated (start,end) form, we refer to this environment when processing var references
type TEnv = M.Map Var Ty2

-- |
cursorize :: Prog -> SyM L3.Prog
cursorize Prog{ddefs,fundefs,mainExp} = do
  fns' <- mapM (fd . snd) (M.toList fundefs)
  let fundefs' = M.fromList $ L.map (\f -> (L3.funname f, f)) fns'
      ddefs'   = M.map L3.eraseLocMarkers ddefs

  mainExp' <- case mainExp of
                Nothing -> return Nothing
                Just (e,ty) -> do
                  e' <- case ty of
                          _ | isPackedTy ty  -> fromDi <$> cursorizePackedExp ddefs fundefs M.empty e
                          _ | hasPacked ty   -> error $ "TODO: hasPacked mainExp"
                          _ -> cursorizeExp ddefs fundefs M.empty e
                  return $ Just (e', L3.stripTyLocs ty)

  return $ L3.Prog ddefs' fundefs' mainExp'

  where

    fd :: FunDef -> SyM L3.FunDef
    fd FunDef{funname,funty,funarg,funbod} = do
      let lvars   = locVars funty
          outCurs = L.filter (\(LRM _ _ m) -> m == Output) lvars
          inCurs  = L.filter (\(LRM _ _ m) -> m == Input) lvars

          inT  = arrIn funty
          outT = arrOut funty

          funty' = L3.cursorizeTy funty

      (arg,exp') <-
        case (outCurs,inCurs) of

          -- Eg. intAdd
          ([],[]) ->
            if (hasPacked inT || hasPacked outT)
            then error $ "Cannot process a packed type without cursors"
            else do
              let initEnv = M.singleton funarg (arrIn funty)
              (funarg,) <$> cursorizeExp ddefs fundefs initEnv funbod

          (ocurs,icurs) -> do
            newarg <- gensym (varAppend "tup_arg_" funname)

            let initEnv = M.fromList [(lrmLoc cur, CursorTy) | cur <- (ocurs ++ icurs)]
                initEnv2 = case inT of
                             PackedTy _ _ -> M.insert funarg CursorTy initEnv
                             _ -> M.insert funarg inT initEnv

            -- 1st: Bind output and input cursors for all locations:
            b <- mkLets [ (lrmLoc cur,[], CursorTy, l$ ProjE ix (l$ VarE newarg))
                        | (cur,ix) <- zip (ocurs ++ icurs) [0..]] <$>

                 -- 2nd: Unpack the "real" argument, which is after the prepended output cursors:
                 (case inT of
                   PackedTy _k loc ->
                     -- we'll only have 1 input cursor bound at `l`
                     l <$> LetE (funarg,[], CursorTy, (l$ VarE loc))

                   -- Packed tuples
                   _ ->
                     l <$> LetE (funarg,[], L3.stripTyLocs inT,
                                 l$ ProjE (1 + length ocurs + length icurs) (l$ VarE newarg))

                 ) <$> do

                 -- 3rd: Bind the result of the function body so we can operate on it:
                 Di bod <- cursorizePackedExp ddefs fundefs initEnv2 funbod

                 -- TODO: all the other steps

                 return bod

            return (newarg, b)

      return $ L3.FunDef funname funty' arg exp'

        {-
        case (mInloc,mOutLoc) of
          -- Eg. leftmost
          (Just inLoc, Nothing) ->

            l<$> (LetE (lrmLoc inLoc,[], CursorTy, l$ ProjE 0 (l$ VarE newarg)) <$>
                   (l<$> (LetE (funarg,[],CursorTy, l$ VarE (lrmLoc inLoc)) <$>
                          cursorizeExp ddefs M.empty funbod)))

          -- Eg. buildLeaf
          (Nothing, Just outLoc) ->

            l<$> (LetE (lrmLoc outLoc,[], CursorTy, l$ ProjE 0 (l$ VarE newarg)) <$>
                   (l<$> (LetE (funarg,[],CursorTy, l$ VarE (lrmLoc outLoc)) <$>
                          cursorizePackedExp ddefs M.empty funbod)))
        -}

-- | Cursorize expressions NOT producing `Packed` values
cursorizeExp :: DDefs Ty2 -> NewFuns -> TEnv -> L Exp2 -> SyM (L L3.Exp3)
cursorizeExp ddfs fundefs tenv (L p ex) = L p <$>
  case ex of
    VarE v    -> return $ VarE v
    LitE n    -> return $ LitE n
    LitSymE n -> return $ LitSymE n

    AppE f locs arg ->
      if locs /= []
      then error $ "cursorizeExp: AppE expected empty locs for scalar values. Got: " ++ sdoc locs
      else AppE f [] <$> go arg

    PrimAppE pr args -> PrimAppE pr <$> mapM go args

    LetE (v,locs,ty,rhs) bod
      | isPackedTy ty -> error $ "cursorizeExp: TOOD isPacked LetE"
      | hasPacked ty  -> error $ "cursorizeExp: TOOD hasPacked LetE"
      | otherwise ->
          if locs /= []
          then error $ "cursorizeExp: LetE expected empty locs for scalar values. Got " ++ sdoc locs
          else do
            rhs' <- go rhs
            LetE (v,[],L3.stripTyLocs ty,rhs') <$>
              cursorizeExp ddfs fundefs (M.insert v ty tenv) bod

    IfE a b c  -> IfE <$> go a <*> go b <*> go c

    MkProdE ls -> MkProdE <$> mapM go ls

    ProjE i e  -> ProjE i <$> go e

    -- Eg. leftmost
    CaseE (L _ (VarE  v)) brs ->
      CaseE (l$ VarE $ v) <$>
        mapM (unpackDataCon ddfs fundefs tenv False v) brs

    DataConE _ _ _ -> error $ "cursorizeExp: Should not have encountered DataConE if type is not packed: "++ndoc ex

    TimeIt e ty b -> TimeIt <$> go e <*> pure (L3.stripTyLocs ty) <*> pure b

    -- Eg. leftmost
    Ext ext ->
      case ext of
        -- Since we're returning a scalar value, locs should be empty here...
        -- Also, we don't have to dilate this return value
        RetE locs v ->
          if locs /= []
          then error $ "cursorizeExp: RetE expected empty locs for scalar values. Got" ++ sdoc locs
          else return $ VarE v

        -- All locations are transformed into cursors here. All the location expressions
        -- are expressed in terms of corresponding cursor operations. See `cursorizeLocExp`
        LetLocE loc rhs bod -> do
          let rhs' = cursorizeLocExp rhs
          LetE (loc,[],CursorTy,rhs') <$>
            cursorizeExp ddfs fundefs (M.insert loc CursorTy tenv) bod

        _ -> error $ "TODO: cursorizeExp Ext: " ++ sdoc ext

    oth -> trace ("TODO: cursorizeExp:\n" ++ sdoc oth) (return $ VarE (toVar $ sdoc oth))

  where
    go = cursorizeExp ddfs fundefs tenv


-- Cursorize expressions producing `Packed` values
cursorizePackedExp :: DDefs Ty2 -> NewFuns -> TEnv -> L Exp2 -> SyM (DiExp (L L3.Exp3))
cursorizePackedExp ddfs fundefs tenv (L p ex) =
  case ex of
    -- Here the allocation has already been performed:
    -- To follow the calling convention, we are reponsible for tagging on the end here:
    VarE v -> return $ mkDi (l$ VarE v) [ l$ VarE (toEndV v) ]

    LitE _n    -> error $ "Shouldn't encounter LitE in packed context:" ++ sdoc ex
    LitSymE _n -> error $ "Shouldn't encounter LitSymE in packed context:" ++ sdoc ex

    AppE f locs _arg ->
     case locs of
        -- TODO: Is this actually an error ?
        [] -> error $ "cursorizePackedExp AppE: unexpected number of locations: " ++ sdoc locs
        _  ->
          let fty = L2.getFunTy fundefs f
              inT = arrIn fty
              outT = arrOut fty
          in
          case (inT, outT) of
            (PackedTy _ _, PackedTy _ _) -> do
              -- ASSUMPTION: locs = (start,end)
              -- Since our calling convention expects o/p cursor to be the first input, we
              -- re-order the locations accordingly
              let (start:end:[]) = locs
              return $ Di $ l $ AppE f [] (l$ MkProdE [l$ VarE end, l$ VarE start])

            _ -> error $ "cursorizePackedExp: TODO functions with packed tuples"


    LetE (v,locs,ty,rhs) bod
      | isPackedTy ty -> do
          -- Packed values are dilated i.e represented as (start,end) cursors
          rhs' <- go rhs
          fresh <- gensym "packed_tpl"

          let tenv' = M.union (M.fromList [(fresh, ProdTy [CursorTy, CursorTy]), (v, CursorTy), (toEndV v, CursorTy)])
                              tenv

          -- Bind start/end cursors. We should use Di here...
          prefix <- return $
                      LetE (fresh,[], ProdTy [CursorTy, CursorTy], fromDi rhs') <$> l <$>
                        LetE (v,[], CursorTy, l$ ProjE 0 (l$ VarE fresh)) <$> l <$>
                          LetE (toEndV v,[], CursorTy, l$ ProjE 1 (l $ VarE fresh))

          case locs of
            [] -> dilprefix <$> prefix <$>
                    fromDi <$> cursorizePackedExp ddfs fundefs tenv' bod
            _  ->  dilprefix <$> prefix <$> l <$>
                     LetE (head locs,[], CursorTy, l$ ProjE 0 (l $ VarE fresh)) <$>
                       fromDi <$> cursorizePackedExp ddfs fundefs (M.insert (head locs) CursorTy tenv') bod

      | hasPacked ty  -> error $ "cursorizePackedExp: TOOD hasPacked LetE"

      | otherwise ->
          if locs /= []
          then error $ "cursorizeExp: LetE expected empty locs for scalar values. Got "
                       ++ sdoc locs
          else do
            rhs' <- cursorizeExp ddfs fundefs tenv rhs
            onDi (l <$> LetE (v,[],L3.stripTyLocs ty,rhs')) <$>
              cursorizePackedExp ddfs fundefs (M.insert v ty tenv) bod

    -- A case expression is eventually transformed into a ReadTag + switch statement.
    -- We first retrieve the cursor referred to by the scrutinee, and unpack
    -- the first bound variable 1 byte after that cursor. Thats all we need to do here,
    -- because we've already computed other locations in InferLocations and RouteEnds
    -- ASSUMPTION: scrutinee is always flat
    CaseE (L _ (VarE v)) brs ->
      dilprefix <$>
        CaseE (l$ VarE $ v) <$>
          mapM (unpackDataCon ddfs fundefs tenv True v) brs

    DataConE sloc dcon args -> do
      let
          -- Return (start,end) cursors
          -- The final return value lives at the position of the out cursors:
          go2 d [] = return $ MkProdE [l$ VarE sloc, l$ VarE d]

          go2 _d ((rnd, ty):rst) | isPackedTy ty = do
            d' <- gensym $ toVar "writepackedcur"
            rnd' <- go rnd
            LetE (d',[], CursorTy, projEnds rnd') <$>
              l <$> (go2 d' rst)

          -- (_ty == IntTy) : Int fields are currently our only "scalar" fields
          go2 d ((rnd,_ty):rst) = do
            d' <- gensym $ toVar "writeintcur"
            rnd' <- cursorizeExp ddfs fundefs tenv rnd
            LetE (d',[], CursorTy, l$ Ext $ L3.WriteInt d rnd') <$>
              l <$> (go2 d' rst)

      writetag <- gensym "writetag"
      dilprefix <$>
        (LetE (writetag,[], CursorTy, l$ Ext $ L3.WriteTag dcon sloc)
         <$> l <$> (go2 writetag (zip args (lookupDataCon ddfs dcon))))

    Ext ext ->
      case ext of

        -- All locations are transformed into cursors here. All the location expressions
        -- are expressed in terms of corresponding cursor operations. See `cursorizeLocExp`
        LetLocE loc rhs bod -> do
          let rhs' = cursorizeLocExp rhs
          onDi (l <$> LetE (loc,[],CursorTy,rhs')) <$>
            cursorizePackedExp ddfs fundefs (M.insert loc CursorTy tenv) bod

        -- ASSUMPTION: RetE forms are inserted at the tail position of functions,
        -- and we safely just return ends-witnesses & ends of the dilated expressions
        RetE locs v -> do
          v' <- go (l$ VarE v)
          case locs of
            []    -> return v'
            [loc] -> return $ mkDi (l$ VarE loc) [ projEnds v' ]
            _ -> error $ "cursorizePackedExp: unexpected no of locations in RetE " ++ sdoc locs

        _ -> trace ("TODO: cursorizeExp:\n" ++ sdoc ext) (return $ Di $ l$  VarE (toVar $ sdoc ext))


    oth -> trace ("TODO: cursorizeExp:\n" ++ sdoc oth) (return $ Di $ l$  VarE (toVar $ sdoc oth))

  where go = cursorizePackedExp ddfs fundefs tenv
        dilprefix = Di <$> L p
        toEndV = varAppend "end_"


cursorizeLocExp :: LocExp -> L L3.Exp3
cursorizeLocExp locExp =
  case locExp of
    AfterConstantLE i loc -> l$ Ext $ L3.AddCursor loc i
    AfterVariableLE v loc -> l$ VarE (toVar $ "AfterVariableLE" ++ fromVar v ++ fromVar loc)
    FromEndLE loc -> l$ VarE loc
    StartOfLE r   -> case r of
                       GlobR  -> error $ "cursorizeLocExp: TODO: GlobR should have a var param"
                       VarR v -> l$ VarE v
                       DynR v -> l$ VarE v
    oth -> error $ "cursorizeLocExp: todo " ++ sdoc oth


-- | Take a cursor pointing to the start of the tag, and advance it by 1 byte
-- If the first bound varaible is a scalar (IntTy), read it using the newly returned cursor.
-- Otherwise, just process the body. it'll have the correct instructions to process
-- other bound locations
unpackDataCon :: DDefs Ty2 -> NewFuns -> TEnv -> Bool -> Var -> (DataCon, [(Var, Var)], L Exp2)
              -> SyM (DataCon, [t], L L3.Exp3)
unpackDataCon ddfs fundefs tenv isPacked scrtCur (dcon,vlocs,rhs) =
  (dcon,[],) <$> go scrtCur vlocs tys True tenv

  where -- (vars,locs) = unzip vlocs
        tys  = lookupDataCon ddfs dcon
        toEndV = varAppend "end_"
        processRhs env = if isPacked
                         then fromDi <$> cursorizePackedExp ddfs fundefs env rhs
                         else cursorizeExp ddfs fundefs env rhs

        -- Loop over fields.  Issue reads to get out all Ints:
        -- Otherwise, just bind vars to locations
        -- Strategy: ALLOW unbound witness variables. A later traversal will reorder.
        --
        -- The first bound location requires special handling. We have to bind it to
        -- (scrtCur + 1) by hand. All the other locations are bound (calculated) by RouteEnd2
        -- Ideally we should arrange RE to bind this as well, but this is a quick hack for now
        --
        go :: (Show t) => Var -> [(Var, Var)] -> [UrTy t] -> Bool -> TEnv -> SyM (L L3.Exp3)
        go _c [] [] _isFirst env = processRhs env
        go cur ((v,loc):rst) (ty:rtys) isFirst env =
          case ty of
            IntTy -> do
              tmp <- gensym (toVar "readint_tpl")
              let env' = M.union (M.fromList [(tmp,ProdTy [IntTy, CursorTy]),
                                              (v, CursorTy),
                                              (toEndV v, CursorTy)])
                         env
              l <$>
                LetE (tmp, [], ProdTy [IntTy, CursorTy], l$ Ext $ L3.ReadInt cur) <$> l <$>
                  LetE (v, [], IntTy, l$ ProjE 0 (l$ VarE tmp)) <$> l <$>
                    LetE (toEndV v, [], CursorTy, l$ ProjE 1 (l$ VarE tmp)) <$>
                      if isFirst
                      then l <$>
                             LetE (loc, [], CursorTy, l$ Ext $ L3.AddCursor scrtCur 1) <$>
                               go (toEndV v) rst rtys False env'
                      else go (toEndV v) rst rtys False (M.insert loc CursorTy env')

            _ -> do
              let env' = (M.insert v CursorTy env)
              if isFirst
              then l <$>
                     LetE (loc, [], CursorTy, l$ Ext $ L3.AddCursor scrtCur 1) <$> l <$>
                       LetE (v,[], CursorTy, l$ VarE loc) <$>
                         go (toEndV v) rst rtys False (M.insert loc CursorTy env')
              else
                l <$>
                  LetE (v,[], CursorTy, l$ VarE loc) <$>
                    go (toEndV v) rst rtys False env'

        go _ vls rtys _ _ = error $ "Unexpected numnber of varible, type pairs: " ++ show (vls,rtys)

-- ================================================================================
--                         Dilation Conventions
-- ================================================================================
-- Everything to do with dilation.  It should be possible to change
-- the dilated format by changing only this section.


-- | If an expression `e` returns type `T`, then a dilated version of
-- `e` returns a tuple (T,Cursors), where cursors contains a flat
-- record of end-cursors corresponding exactly to all the components
-- of T which are PackedTy.
--
newtype DiExp ex = Di ex
  deriving (Generic, Show, Read, Eq, Ord)
--type DiExp = Exp

instance (Out ex) => Out (DiExp ex)

onDi :: (ex -> ex) -> DiExp ex -> DiExp ex
onDi f (Di x) = Di (f x)

fromDi :: DiExp ex -> ex
fromDi (Di x) = x


-- | Project the cursor package from a dilated expression, contains pointers
-- to all the ENDs.
projEnds :: DiExp (L L3.Exp3) -> (L L3.Exp3)
projEnds (Di e) = mkProjE 1 e

-- | Project the original value from a dilated expression.
projVal :: DiExp (L L3.Exp3) -> (L L3.Exp3)
projVal (Di e) = mkProjE 0 e


-- | Smart constructor that immediately destroys products if it can:
--   Does NOT avoid single-element tuples.
mkProjE :: Int -> (L L3.Exp3) -> (L L3.Exp3)
mkProjE ix (L _ (MkProdE ls)) = ls !! ix
mkProjE ix e = l$ (ProjE ix e)

-- | Constructor that combines a regular expression with a list of
-- corresponding end cursors.
mkDi :: (L L3.Exp3) -> [(L L3.Exp3)] -> DiExp (L L3.Exp3)
mkDi x []  = Di $ l$ MkProdE [x, l$ MkProdE []]
mkDi x [o] = Di $ l$  MkProdE [x, o]
mkDi x ls  = Di $ l$ MkProdE [x, l$ MkProdE ls]



{-

-- | For non-cursor types, dilation is very simple:
dilateTrivial :: ex -> DiExp ex
dilateTrivial e = Di $ MkProdE [e, MkProdE []]


-}

{-
cursorizeExp :: DDefs Ty2 -> TEnv -> L Exp2 -> SyM (L L3.Exp3)
cursorizeExp ddfs tenv (L p exp) = L p <$>
  case exp of
    -- If the variable had a `Packed _ T`, it's going to be transformed to
    -- (start,end) cursors
    VarE v    -> case M.lookup v tenv of
                   Nothing -> return $ VarE v
                   -- (isPackedTy ty) is always going to be true here. tenv only stores
                   -- variables who had packed types
                   Just ty | isPackedTy ty -> return $ MkProdE [l$ VarE v, l$ VarE (toEndV v)]
                   oth -> error $ "unexpected variable type: " ++ sdoc oth

    LitE n    -> return $ LitE n
    LitSymE v -> return $ LitSymE v

    -- A case expression is eventually transformed into a ReadTag + switch statement.
    -- We first retrieve the cursor referred to by the scrutinee, and unpack
    -- the first bound variable 1 byte after that cursor. Thats all we need to do here,
    -- because we've already computed other locations in InferLocations and RouteEnds
    CaseE (L _ (VarE v)) brs ->
          {- generate a ReadTag here, instead of doing it in Lower ...

          cursorAfterTag <- gensym (toVar "cursor_after_tag")
          traceShow lrm (return __)
          maintain all the same properties as the cursor for v
          let cursorAfterTagLrm = cursorizeLRM "_" lrm
          (LetE (cursorAfterTag,[],
                 CursorTy cursorAfterTagLrm,
                 l$ Ext $ L3.ReadTag loc)) <$>
            (l<$> CaseE (l$ VarE cursorAfterTag)) <$>
                    mapM (unpackDataCon cursorAfterTagLrm) brs

          -}
      CaseE (l$ VarE $ v) <$>
        mapM (unpackDataCon v) brs

    -- Trivial case
    PrimAppE pr args -> PrimAppE pr <$> mapM go args

    -- Here we switch to a convention where functions accept (output,input) cursors
    -- and return end-witnesses. Since we already know the abstract locations to which
    -- these values flow to, the actual fn arguments are unused!
    -- TODO: AUTIT ME
    AppE f locs _args ->
      case locs of
        (iploc:oploc:[]) -> do
          return $ AppE f [] (l$ MkProdE [l$ VarE oploc, l$ VarE iploc])
        _ -> error $ "cursorizing AppE: unexpected number of locations: " ++ show locs

    -- TODO: AUTIT ME
    -- Right now, we return a (Cursor,Cursor) pair i.e (start, end)
    DataConE sloc dcon args -> do
      let
          -- Return (start,end) cursors
          -- The final return value lives at the position of the out cursors:
          go2 d [] = return $ MkProdE [l$ VarE sloc, l$ VarE d]

          go2 _d ((rnd, ty):rst) | isPackedTy ty = do
            d' <- gensym $ toVar "writepackedcur"
            let (L _ (VarE v)) = rnd
            LetE (d',[], CursorTy, l$ VarE (toEndV v)) <$>
              l <$> (go2 d' rst)

          -- (_ty == IntTy) : Int fields are currently our only "scalar" fields
          go2 d ((rnd,_ty):rst) = do
            d' <- gensym $ toVar "writeintcur"
            rnd' <- go rnd
            LetE (d',[], CursorTy, l$ Ext $ L3.WriteInt d rnd') <$>
              l <$> (go2 d' rst)

      writetag <- gensym "writetag"
      (LetE (writetag,[], CursorTy,
             l$ Ext $ L3.WriteTag dcon sloc)
        <$> l <$> (go2 writetag (zip args (lookupDataCon ddfs dcon))))


    -- This is a simple case where the RHS is not packed
    LetE (v,_locs,ty,rhs) bod | not (isPackedTy ty) -> do
      rhs' <- go rhs
      LetE (v,[], L3.stripTyLocs ty,rhs') <$> go bod


    -- Here, we assume a convention that all packed values are changed to be (start,end) cursors.
    LetE (v,locs,ty,rhs) bod | isPackedTy ty -> do
      -- would return a (start,end) cursor tuple
      -- we bind v to the end cursor, and start cursor to the location in locs
      rhs' <- go rhs
      fresh <- gensym "packed_tpl"
      let tenv' = M.insert v ty tenv

      let (PackedTy _ tyLoc) = ty

      -- bind the end-of witness that rhs would return
      prefix <- case locs of
                  [] -> return $ LetE (fresh,[],ProdTy [CursorTy, CursorTy], rhs')

                  _  -> return $ LetE (fresh,[],ProdTy [CursorTy, CursorTy], rhs') <$>
                                   l <$> LetE (head locs,[],CursorTy, l$ ProjE 0 (l$ VarE fresh))

      prefix <$>
        (l <$> LetE (toEndV v,[],CursorTy, l$ ProjE 1 (l$ VarE fresh)) <$>
          (l <$> LetE (v,[],CursorTy, l$ VarE tyLoc) <$>
            cursorizeExp ddfs tenv' bod))


    -- TODO
    -- IfE EXP EXP EXP
    -- MkProdE [EXP]
    -- ProjE Int EXP
    -- TimeIt EXP dec Bool

    -- All locations are transformed into cursors here. All the location expressions
    -- are expressed in terms of corresponding cursor operations. See `cursorizeLocExp`
    Ext (LetLocE loc rhs bod) -> do
      let rhs' = cursorizeLocExp rhs
      LetE (loc,[],CursorTy,rhs') <$>
        go bod

    -- Just convert the implicit location return into a ProdE
    Ext (RetE locs v) ->
      case locs of
        [] -> unLoc <$> go (l$ VarE v)
        -- ASSUMPTION: RetE forms have locs when we're using them to return end-witnesses
        -- So return the end-witness of v' if v was packed
        [loc] -> do
          v' <- case M.lookup v tenv of
                  Nothing -> return $ VarE v
                  Just ty | isPackedTy ty ->  return $ VarE (toEndV v)
                  oth -> error $ "unexpected variable type: " ++ sdoc oth

          return $ MkProdE [l$ VarE loc, l$ v']
        _ -> error $ "cursorize: RetE with more than 1 locs not allowed! " ++ show locs

    Ext (LetRegionE r bod) -> do
      v <- regionToVar r
      LetE (v,[],CursorTy, l$ Ext L3.NewBuffer) <$>
        go bod

    -- Some expressions are not handled yet ...
    oth -> error $ "TODO:\n" ++ sdoc oth

  where
    go = cursorizeExp ddfs tenv

    toEndV = varAppend "end_"

    regionToVar :: Region -> SyM Var
    regionToVar r = case r of
                      GlobR  -> gensym "glob_region"
                      VarR v -> return v
                      DynR v -> return v


    -- | Take a cursor pointing to the start of the tag, and advance it by 1 byte
    -- If the first bound varaible is a scalar (IntTy), read it using the newly returned cursor.
    -- Otherwise, just process the body. it'll have the correct instructions to process
    -- other bound locations
    unpackDataCon :: Var -> (DataCon, [(Var,LocVar)], L Exp2) ->
                     SyM (DataCon, [(Var,())], L L3.Exp3)
    unpackDataCon scrtCur (dcon,vlocs,rhs) =
      let (vars,locs) = unzip vlocs
          tys  = lookupDataCon ddfs dcon
      in
      case tys of
        [] -> (dcon, [],) <$> go rhs
        (ty:_) -> do
          let floc    = head locs            -- location of the first field

          -- TODO: check if we can conditionally add things to the fmap computation
          bod <-
            if ty == IntTy
            then do
              -- the first field is an int, create a let binding for "v" by performing a
              -- readint
              let v = head vars -- name of the first bound variable
              tmp <- gensym (toVar "readint_tpl")
              LetE (floc,[],CursorTy, l$ Ext$ L3.AddCursor scrtCur 1) <$>
                -- the tmp cursor doesn't have a correct type. flrm should be modified
                -- with the location of the next field, if it has any
                l<$> (LetE (tmp,[],ProdTy [IntTy, CursorTy],
                            l$ Ext $ L3.ReadInt scrtCur) <$>
                       (l<$> LetE (v,[],IntTy, l$ ProjE 0 (l$ VarE tmp)) <$>
                         go rhs))
            else do
              LetE (floc,[],CursorTy, l$ Ext$ L3.AddCursor scrtCur 1) <$>
                go rhs

          return (dcon,[],l$ bod)

    -- would this always have a valid LRM ? should this be a Maybe ?
    cursorizeLocExp :: LocExp -> L L3.Exp3
    cursorizeLocExp locExp =
      case locExp of
        AfterConstantLE i loc -> l$ Ext $ L3.AddCursor loc i
        AfterVariableLE v loc -> l$ VarE (toVar $ "AfterVariableLE" ++ fromVar v ++ fromVar loc)
        FromEndLE loc -> l$ VarE loc
        StartOfLE r   -> case r of
                           GlobR  -> error $ "cursorizeLocExp: TODO: GlobR should have a var param"
                           VarR v -> l$ VarE v
                           DynR v -> l$ VarE v
        oth -> error $ "cursorizeLocExp: todo " ++ sdoc oth

-- | Change the location to _
cursorizeLRM :: LRM -> LRM
cursorizeLRM lrm = lrm {lrmLoc = "_"}

-}
