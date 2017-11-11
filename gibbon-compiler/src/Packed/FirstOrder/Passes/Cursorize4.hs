{-# LANGUAGE OverloadedStrings #-}

module Packed.FirstOrder.Passes.Cursorize4
  (cursorize) where

import Control.Monad (forM)
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
-- i.e. start/end.
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
                  case ty of
                    _ | isPackedTy ty  -> Just . (, L3.stripTyLocs ty) <$>
                                          projVal <$> cursorizePackedExp ddefs fundefs M.empty e
                    _ | hasPacked ty   ->  Just . (, L3.stripTyLocs ty) <$>
                                           fromDi <$> cursorizePackedExp ddefs fundefs M.empty e
                    _ -> Just . (,L3.stripTyLocs ty) <$> cursorizeExp ddefs fundefs M.empty e

  return $ L3.Prog ddefs' fundefs' mainExp'

  where

    fd :: FunDef -> SyM L3.FunDef
    fd FunDef{funname,funty,funarg,funbod} = do
      let lvars   = locVars funty
          outCurs = L.filter (\(LRM _ _ m) -> m == Output) lvars
          inCurs  = L.filter (\(LRM _ _ m) -> m == Input) lvars

          inT  = arrIn funty
          outT = arrOut funty

          funty' = L3.cursorizeArrowTy funty

      (arg,exp') <-
        case (outCurs,inCurs) of

          -- Eg. intAdd
          ([],[]) ->
            if (hasPacked inT || hasPacked outT)
            then error $ "Cannot process a packed type without cursors"
            else do
              let initEnv = M.singleton funarg (arrIn funty)
              (funarg,) <$> cursorizeExp ddefs fundefs initEnv funbod

          -- Eg. leftmost
          -- Takes in a packed argument, but returns a scalar value. Only has input/read cursors
          ([],icurs) -> do
            case icurs of
              -- When a function takes in a single packed argument, it's passed directly
              -- i.e not in a tuple. It's type is (f :: CursorTy -> SCALAR)
              [_cur] -> (funarg,) <$>
                          cursorizeExp ddefs fundefs (M.singleton funarg CursorTy) funbod
              _ -> error "fd: Read packed tuples"

          -- Eg. buildLeaf, buildTree, buildTreeSum etc
          -- Start by creating let bindings for all the output cursors.
          -- Since all the cursors are passed before old fn argument, all bindings are just projections starting at 0.
          -- We also have to bind the old fn argument (funarg), which comes after all the output cursors.
          --
          -- Then, if the body is of type Packed, we return the end_write cursor of the packed value.
          -- Otherwise, the return value would be a tuple of (end_write_cur1, end_write_cur2, maybe_scalar) etc
          -- and we return it as is.
          (ocurs, []) -> do
              newarg <- gensym (varAppend "tup_arg_" funname)
              funbod' <- fromDi <$> cursorizePackedExp ddefs fundefs (M.singleton funarg CursorTy) funbod
              bod <- return $
                     -- output cursor bindings
                     mkLets [ (lrmLoc cur,[],CursorTy, l$ ProjE i (l$ VarE newarg))
                            | (cur,i) <- zip ocurs [0..]]
                     -- old fn argument binding + processed fn body
                     (LetE (funarg,[], L3.stripTyLocs (arrIn funty), l$ ProjE (length ocurs) (l$ VarE newarg)) <$> l <$>
                        funbod')
              return (newarg, bod)

          -- Eg. add1
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

                 ) <$>
                 -- 3rd: Bind the result of the function body so we can operate on it:
                 fromDi <$> cursorizePackedExp ddefs fundefs initEnv2 funbod
                 -- TODO: all the other steps

            return (newarg, b)

      return $ L3.FunDef funname funty' arg exp'

-- | Cursorize expressions NOT producing `Packed` values
cursorizeExp :: DDefs Ty2 -> NewFuns -> TEnv -> L Exp2 -> SyM (L L3.Exp3)
cursorizeExp ddfs fundefs tenv (L p ex) = L p <$>
  case ex of
    VarE v    -> return $ VarE v
    LitE n    -> return $ LitE n
    LitSymE n -> return $ LitSymE n

    AppE f locs arg ->
      case locs of
          [] -> AppE f [] <$> go arg
          -- These are read/input locations
          [loc] -> return $ AppE f [] (l$ VarE loc)
          _     -> return $ AppE f [] (l$ MkProdE [l$ VarE x | x <- locs ])

    PrimAppE pr args -> PrimAppE (L3.toL3Prim pr) <$> mapM go args

    -- Same as `cursorizePackedExp`
    LetE bnd bod -> cursorizeLet ddfs fundefs tenv False bnd bod

    IfE a b c  -> IfE <$> go a <*> go b <*> go c

    MkProdE ls -> MkProdE <$> mapM go ls

    ProjE i e  -> ProjE i <$> go e

    -- Eg. leftmost
    CaseE scrt brs -> do
      -- ASSUMPTION: scrt is flat
      let (L _ (VarE  v)) = scrt
      CaseE (l$ VarE $ v) <$>
        mapM (unpackDataCon ddfs fundefs tenv False v) brs

    DataConE _ _ _ -> error $ "cursorizeExp: Should not have encountered DataConE if type is not packed: "++ndoc ex

    TimeIt e ty b -> TimeIt <$> go e <*> pure (L3.stripTyLocs ty) <*> pure b

    -- Eg. leftmost
    Ext ext ->
      case ext of
        RetE locs v ->
          case locs of
              []    -> return (VarE v)
              [loc] -> return $ MkProdE [l$ VarE loc, l$ VarE v]
              _ -> error $ "cursorizeExp: RetE todo "

        -- All locations are transformed into cursors here. All the location expressions
        -- are expressed in terms of corresponding cursor operations. See `cursorizeLocExp`
        LetLocE loc rhs bod -> do
          let rhs' = cursorizeLocExp rhs
          LetE (loc,[],CursorTy,rhs') <$>
            cursorizeExp ddfs fundefs (M.insert loc CursorTy tenv) bod

        -- Exactly same as cursorizePackedExp
        LetRegionE reg bod -> do
          let (v,buf) = regionToBnd reg
          LetE (v,[],CursorTy, l$ Ext buf) <$>
            go bod

        _ -> error $ "TODO: cursorizeExp Ext: " ++ sdoc ext

    MapE{} -> error $ "TODO: cursorizeExp MapE"
    FoldE{} -> error $ "TODO: cursorizeExp FoldE"

  where
    go = cursorizeExp ddfs fundefs tenv


-- Cursorize expressions producing `Packed` values
cursorizePackedExp :: DDefs Ty2 -> NewFuns -> TEnv -> L Exp2 -> SyM (DiExp (L L3.Exp3))
cursorizePackedExp ddfs fundefs tenv (L p ex) =
  case ex of
    -- Here the allocation has already been performed:
    -- To follow the calling convention, we are reponsible for tagging on the end here:
    VarE v -> do
      let ty = tenv ! v
      if isPackedTy ty
      then return $ mkDi (l$ VarE v) [ l$ VarE (toEndV v) ]
      else return $ dl $ VarE v

    LitE _n    -> error $ "Shouldn't encounter LitE in packed context:" ++ sdoc ex
    LitSymE _n -> error $ "Shouldn't encounter LitSymE in packed context:" ++ sdoc ex

    AppE f locs arg -> do
      let inT = arrIn $ funty (fundefs ! f)
      case inT of
        _ | isPackedTy inT -> do
              let [start,end] = locs
              return $ dl $ AppE f [] (l$ MkProdE [l$ VarE end, l$ VarE start])

        _ | hasPacked inT -> do
              error "todo appe"

        _ -> case locs of
               [] -> error $ "cursorizePackedExp AppE: empty locations"
               -- ASSUMPTION: arg is a scalar value
               _  -> do
                 arg' <- cursorizeExp ddfs fundefs tenv arg
                 return $ dl $ AppE f [] $ l$ MkProdE $ [ l$ VarE loc | loc <- locs] ++ [arg']


    PrimAppE _ _ -> error $ "cursorizePackedExp: unexpected PrimAppE in packed context" ++ sdoc ex

    -- The only primitive that returns packed data is ReadPackedFile:
    -- This is simpler than TimeIt below.  While it's out-of-line,
    -- it doesn't need memory allocation (NewBuffer/ScopedBuffer).
    -- This is more like the witness case below.
    LetE (vr,_locs, _ty, L _ (PrimAppE (ReadPackedFile path tyc ty2) [])) bod ->
      onDi (l <$> LetE (vr, [], CursorTy, l$ PrimAppE (L3.toL3Prim $ ReadPackedFile path tyc ty2) [])) <$>
        go (M.insert vr CursorTy tenv) bod


    --
    LetE (v,_locs,ty2@(ProdTy tys), rhs@(L _ (MkProdE ls))) bod -> do
      es <- forM (zip tys ls) $ \(ty,e) -> do
              case ty of
                  _ | isPackedTy ty -> fromDi <$> cursorizePackedExp ddfs fundefs tenv e
                  _ | hasPacked ty  -> error $ "cursorizePackedExp: nested tuples" ++ sdoc rhs
                  _ -> cursorizeExp ddfs fundefs tenv e
      let rhs' = l$ MkProdE es
          ty2' = L3.cursorizeTy ty2
      onDi (l <$> LetE (v,[],ty2', rhs')) <$>
        go (M.insert v ty2 tenv) bod

    -- HACK:
    -- Two ways in which we can cursorize this:
    --
    -- let pakd_tup = projE n something in
    -- let x        = projE 0 pakd_tup in
    -- let end_x    = projE 1 pakd_tup
    --
    -- OR
    --
    -- let x     = projE 0 (projE n something) in
    -- let end_x = projE 1 (projE n something)
    --
    -- `cursorizeLet` creates the former, while our special case here outputs the latter.
    --
    -- Reason: unariser can only eliminate direct projections of this form
    LetE (v,locs,ty, rhs@(L _ ProjE{})) bod | isPackedTy ty ->
      case locs of
        [] -> do
          let ty' = L3.cursorizeTy ty
              -- We cannot reuse ty' here because TEnv and expressions are tagged with different types
              ty'' = L3.cursorizeTy ty
              tenv' = M.union (M.fromList [(v, ty),
                                           (toEndV v, projEndsTy ty')])
                      tenv
          rhs' <- go tenv rhs
          bod' <- fromDi <$> go tenv' bod
          return $ Di $
            mkLets [ (v       ,[], projValTy ty'' , projVal rhs')
                   , (toEndV v,[], projEndsTy ty'', projEnds rhs')
                   ]
            bod'

        -- Not sure when will this be non-empty
        _  -> error $ "cursorizePackedExp: Got unexpected #locations: " ++ sdoc locs


    MkProdE{} -> error "cursorizePackedExp: unexpected MkProdE"


    LetE bnd bod -> dl <$> cursorizeLet ddfs fundefs tenv True bnd bod

    -- Here we route the dest cursor to both braches.  We switch
    -- back to the other mode for the (non-packed) test condition.
    IfE a b c -> do
      Di b' <- go tenv b
      Di c' <- go tenv c
      a'    <- cursorizeExp ddfs fundefs tenv a
      return $ Di $ l $ IfE a' b' c'

    -- Not sure if we need to replicate all the checks from Cursorize1
    ProjE i e -> dl <$> ProjE i <$> fromDi <$> go tenv e

    -- A case expression is eventually transformed into a ReadTag + switch statement.
    -- We first retrieve the cursor referred to by the scrutinee, and unpack
    -- the first bound variable 1 byte after that cursor. Thats all we need to do here,
    -- because we've already computed other locations in InferLocations and RouteEnds
    CaseE scrt brs -> do
      -- ASSUMPTION: scrutinee is always flat
      let (L _ (VarE v)) = scrt
      dl <$>
        CaseE (l$ VarE $ v) <$>
          mapM (unpackDataCon ddfs fundefs tenv True v) brs

    DataConE sloc dcon args -> do
      let
          -- Return (start,end) cursors
          -- The final return value lives at the position of the out cursors:
          go2 :: Var -> [(L Exp2, Ty2)] -> SyM L3.Exp3
          go2 d [] = return $ MkProdE [l$ VarE sloc, l$ VarE d]

          go2 d ((rnd, ty):rst) = do
            d' <- gensym "writecur"
            if isPackedTy ty

            then do
             rnd' <- go tenv rnd
             LetE (d',[], CursorTy, projEnds rnd') <$> l <$>
               go2 d' rst

            -- INT is the only scalar type right now
            else do
             rnd' <- cursorizeExp ddfs fundefs tenv rnd
             LetE (d',[], CursorTy, l$ Ext $ L3.WriteInt d rnd') <$> l <$>
               go2 d' rst

      writetag <- gensym "writetag"
      dl <$>
        LetE (writetag,[], CursorTy, l$ Ext $ L3.WriteTag dcon sloc) <$> l <$>
          go2 writetag (zip args (lookupDataCon ddfs dcon))

    TimeIt e t b -> do
      Di e' <- go tenv e
      return $ Di $ l$ TimeIt e' (L3.stripTyLocs t) b

    Ext ext ->
      case ext of

        -- All locations are transformed into cursors here. All the location expressions
        -- are expressed in terms of corresponding cursor operations. See `cursorizeLocExp`
        LetLocE loc rhs bod -> do
          let rhs' = cursorizeLocExp rhs
          onDi (l <$> LetE (loc,[],CursorTy,rhs')) <$>
             go (M.insert loc CursorTy tenv) bod

        -- ASSUMPTION: RetE forms are inserted at the tail position of functions,
        -- and we safely just return ends-witnesses & ends of the dilated expressions
        RetE locs v -> do
          v' <- go tenv (l$ VarE v)
          case locs of
            []    -> return v'
            [loc] -> return $ mkDi (l$ VarE loc) [ fromDi v' ]
            _ -> error $ "cursorizePackedExp: RetE todo "

        LetRegionE r bod -> do
          let (v,buf) = regionToBnd r
          dl <$>
            LetE (v,[],CursorTy, l$ Ext buf) <$>
              fromDi <$> go tenv bod

        _ -> trace ("TODO: cursorizeExp:\n" ++ sdoc ext) (return $ Di $ l$  VarE (toVar $ sdoc ext))


    MapE{}  -> error $ "TODO: cursorizePackedExp MapE"
    FoldE{} -> error $ "TODO: cursorizePackedExp FoldE"

  where go = cursorizePackedExp ddfs fundefs
        toEndV = varAppend "end_"
        dl = Di <$> L p


cursorizeLocExp :: LocExp -> L L3.Exp3
cursorizeLocExp locExp =
  case locExp of
    AfterConstantLE i loc -> l$ Ext $ L3.AddCursor loc (l$ LitE i)
    AfterVariableLE v loc -> let sizeV = varAppend "sizeof_"
                                 toEndV = varAppend "end_"
                             in
                               l$ LetE (sizeV v ,[], IntTy, l$ Ext $ L3.SizeOf v (toEndV v)) $
                               l$ Ext $ L3.AddCursor loc (l$ VarE (sizeV v))
    FromEndLE loc -> l$ VarE loc
    StartOfLE r   -> case r of
                       GlobR v -> l$ VarE v
                       VarR v  -> l$ VarE v
                       DynR v  -> l$ VarE v
    oth -> error $ "cursorizeLocExp: todo " ++ sdoc oth

cursorizeLet :: DDefs Ty2 -> NewFuns -> TEnv -> Bool
             -> (Var, [Var], Ty2, L Exp2) -> L Exp2 -> SyM L3.Exp3
cursorizeLet ddfs fundefs tenv isPackedContext (v,locs,ty,rhs) bod
    -- Process RHS and bind the following cursors
    --
    -- v     -> start_write
    -- end_v -> end_write
    -- loc   -> end_read (only if it's available)
    --
    -- An expression returning packed value can either be a `DataConE` or a `AppE`.
    -- DataConE returns a (start_write,end_write) tuple where AppE returns (end_read,end_write)
    --
    -- So we cannot always rely on the RHS to return a start_write cursor.
    -- But since the types of all packed expressions are already annotated with locations,
    -- we can take a shortcut here and directly bind `v` to the tagged location.
    --
    -- Other bindings are straightforward projections of the processed RHS.
    --
    | isPackedTy ty = do
        rhs' <- fromDi <$> cursorizePackedExp ddfs fundefs tenv rhs
        fresh <- gensym "tup_packed"
        let ty' = case locs of
                    [] -> L3.cursorizeTy ty
                    xs -> ProdTy ([CursorTy | _ <- xs] ++ [L3.cursorizeTy ty])

            tenv' = M.union (M.fromList [(v, ty),
                                         (fresh, ty'),
                                         (toEndV v, projTy 1 ty')])
                    tenv

            -- Sigh .. We cannot resuse ty' here because TEnv and expresssions are tagged with different types
            ty''  = case locs of
                      [] -> L3.cursorizeTy ty
                      xs -> ProdTy ([CursorTy | _ <- xs] ++ [L3.cursorizeTy ty])
            rhs'' = dl $ VarE fresh
            bnds  = case locs of
                      []    -> [ (fresh   , [], ty''            , rhs' )
                               , (v       , [], projValTy  ty'' , projVal  rhs'')
                               , (toEndV v, [], projEndsTy ty'' , projEnds rhs'')]

                      [loc] -> [ (fresh   , [], ty''    , rhs' )
                               , (loc     , [], CursorTy, projVal rhs'')
                               , (v       , [], projValTy  (projEndsTy ty''), projVal  (Di $ projEnds rhs''))
                               , (toEndV v, [], projEndsTy (projEndsTy ty''), projEnds (Di $ projEnds rhs''))]
                      __ -> error "cursorizeLet: isPackedTy"

        bod' <- go tenv' bod
        return $ unLoc $ mkLets bnds bod'

        --   ProjE{} ->
        --     case locs of
        --       [] -> LetE (v,[],CursorTy, l$ VarE outLoc) <$> l <$>
        --               LetE (toEndV v,[],L3.cursorizeTy ty, rhs') <$>
        --                 go (M.insert v ty tenv) bod

        --       _  -> error "ProjE todo"


        --   oth -> error $ sdoc oth ++ "\ncannot return a packed value"

    | hasPacked ty = do
        rhs' <- fromDi <$> cursorizePackedExp ddfs fundefs tenv rhs
        let ty' = L3.cursorizeTy ty
            tenv' = M.insert v ty tenv
        case locs of
          [] -> LetE (v,[], ty', rhs') <$>
                  go tenv' bod
          _  -> error "cursorizeLet: packed tuples"

    | otherwise = do
        rhs' <- cursorizeExp ddfs fundefs tenv rhs
        case locs of
            [] -> LetE (v,[],L3.stripTyLocs ty, rhs') <$>
                    go (M.insert v ty tenv) bod

            -- This was a scalar binding before, but now has been transformed to also
            -- return an end_read cursor. So the type of the binding now becomes:
            -- ProdTy [CursorTy, old_ty]
            --
            -- Also, the binding itself now changes to:
            -- end_read -> ProjE 0 RHS'
            -- v        -> ProjE 1 RHS'
            --
            -- `rightmost` is an example of a program that does this
            [loc] -> do
              fresh <- gensym "tup_scalar"
              let ty' = ProdTy [CursorTy, L3.stripTyLocs ty]
                  tenv' = M.union (M.fromList [(fresh, ProdTy [CursorTy, ty]),
                                               (v, ty),
                                               (loc, CursorTy)])
                                  tenv

              LetE (fresh,[],ty',rhs') <$> l <$>
                LetE (loc,[],CursorTy,l$ ProjE 0 (l$ VarE fresh)) <$> l <$>
                  LetE (v,[],L3.stripTyLocs ty,l$ ProjE 1 (l$ VarE fresh)) <$>
                    go tenv' bod

            _ -> error "cursorizeLet: packed tuples"

  where go t x = if isPackedContext
                 then fromDi <$> cursorizePackedExp ddfs fundefs t x
                 else cursorizeExp ddfs fundefs t x
        toEndV = varAppend "end_"
        dl = Di <$> L NoLoc


-- | Take a cursor pointing to the start of the tag, and advance it by 1 byte
-- If the first bound varaible is a scalar (IntTy), read it using the newly returned cursor.
-- Otherwise, just process the body. it'll have the correct instructions to process
-- other bound locations
unpackDataCon :: DDefs Ty2 -> NewFuns -> TEnv -> Bool -> Var
              -> (DataCon, [(Var, Var)], L Exp2) -> SyM (DataCon, [t], L L3.Exp3)
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

              (if isFirst
               then
                 l <$>
                   LetE (loc, [], CursorTy, l$ Ext $ L3.AddCursor scrtCur (l$ LitE 1)) <$> l <$>
                     LetE (tmp, [], ProdTy [IntTy, CursorTy], l$ Ext $ L3.ReadInt loc)
               else
                 l <$> LetE (tmp, [], ProdTy [IntTy, CursorTy], l$ Ext $ L3.ReadInt cur)
                ) <$> l <$>

               LetE (v, [], IntTy, l$ ProjE 0 (l$ VarE tmp)) <$> l <$>
                 LetE (toEndV v, [], CursorTy, l$ ProjE 1 (l$ VarE tmp)) <$>
                   go (toEndV v) rst rtys False (M.insert loc CursorTy env')


            _ -> do
              let env' = (M.insert v CursorTy env)
              if isFirst
              then l <$>
                     LetE (loc, [], CursorTy, l$ Ext $ L3.AddCursor scrtCur (l$ LitE 1)) <$> l <$>
                       LetE (v,[], CursorTy, l$ VarE loc) <$>
                         go (toEndV v) rst rtys False (M.insert loc CursorTy env')
              else
                l <$>
                  LetE (v,[], CursorTy, l$ VarE loc) <$>
                    go (toEndV v) rst rtys False env'

        go _ vls rtys _ _ = error $ "Unexpected numnber of varible, type pairs: " ++ show (vls,rtys)


-- | Smart constructor that immediately destroys products if it can:
--   Does NOT avoid single-element tuples.
mkProjE :: Int -> (L L3.Exp3) -> (L L3.Exp3)
mkProjE ix (L _ (MkProdE ls)) = ls !! ix
mkProjE ix e = l$ (ProjE ix e)


projTy :: (Out a) => Int -> UrTy a -> UrTy a
projTy 0 (ProdTy (ty:_))  = ty
projTy n (ProdTy (_:tys)) = projTy (n-1) (ProdTy tys)
projTy _ ty = error $ "projTy: " ++ sdoc ty ++ " is not a projection!"

projValTy :: (Out a) => UrTy a -> UrTy a
projValTy = projTy 0

projEndsTy :: (Out a) => UrTy a -> UrTy a
projEndsTy = projTy 1


-- | Return details to create a L3 binding for a region (var name and L3 extension)
regionToBnd :: Region -> (Var, L3.E3Ext loc dec)
regionToBnd r = case r of
                  GlobR v -> (v,L3.NewBuffer)
                  VarR  v -> (v,L3.NewBuffer)
                  DynR  v -> (v,L3.ScopedBuffer)


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
