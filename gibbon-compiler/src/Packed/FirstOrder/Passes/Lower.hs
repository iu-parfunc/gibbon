
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}


-------------------------------------------------------------------------------

-- | Lowering L1 to the target language.
module Packed.FirstOrder.Passes.Lower
  ( lower
  ) where

-------------------------------------------------------------------------------


import Control.Monad
import Packed.FirstOrder.Common hiding (FunDef)
import qualified Packed.FirstOrder.L1_Source as L1
import           Packed.FirstOrder.L1_Source (Exp(..))
import qualified Packed.FirstOrder.LTraverse as L2
import           Packed.FirstOrder.LTraverse ( FunDef(..), Prog(..) )
import qualified Packed.FirstOrder.Target as T
import qualified Packed.FirstOrder.Passes.Cursorize as C
import Data.Maybe
import qualified Data.List as L
import Data.List as L hiding (tail)
import Data.Map as M
import Data.Int (Int64)

import Prelude hiding (tail)

mkUnpackerName :: Constr -> Var
mkUnpackerName tyCons = "unpack_" ++ tyCons

genDcons :: [L1.Ty] -> Var -> [(T.Ty, T.Triv)] -> SyM T.Tail 
genDcons (x:xs) tail fields = case x of
  L2.IntTy             ->  do
    val  <- gensym "val"
    T.LetPrimCallT [(val, T.IntTy), (tail, T.CursorTy)] T.ReadInt [(T.VarTriv tail)] 
      <$> genDcons xs tail (fields ++ [(T.IntTy, T.VarTriv val)])
      
  L2.PackedTy tyCons _ -> do
    ptr  <- gensym "ptr"
    T.LetCallT [(ptr, T.CursorTy), (tail, T.CursorTy)] (mkUnpackerName tyCons) [(T.VarTriv tail)]
      <$> genDcons xs tail (fields ++ [(T.CursorTy, T.VarTriv ptr)]) 
  _                    -> undefined

genDcons [] tail fields     = do 
  ptr <- gensym "ptr"
  return $ T.LetAllocT ptr fields $ T.RetValsT [T.VarTriv ptr, T.VarTriv tail] 

genAlts :: [(Constr,[L1.Ty])] -> Var -> Int64 -> SyM T.Alts 
genAlts ((_, typs):xs) tail n = do
  curTail <- genDcons typs tail [] 
  alts    <- genAlts xs tail (n+1) 
  case alts of
    T.IntAlts tags -> return $ T.IntAlts ((n, curTail) : tags)
    _              -> undefined

genAlts [] _ _                    = return $ T.IntAlts [] 


genUnpacker :: DDef L1.Ty -> SyM T.FunDecl    
genUnpacker DDef{tyName, dataCons} = do
  p    <- gensym "p"
  tag  <- gensym "tag"
  tail <- gensym "tail"
  alts <- genAlts dataCons tail 0
  bod  <- return $ T.LetPrimCallT [(tag, T.TagTy), (tail, T.CursorTy)] T.ReadTag [(T.VarTriv p)] $
            T.Switch (T.VarTriv p) alts Nothing
  return T.FunDecl{ T.funName  = (mkUnpackerName tyName),
                    T.funArgs  = [(p, T.CursorTy)],
                    T.funRetTy = T.CursorTy,
                    T.funBody  = bod } 
                    
-------------------------------------------------------------------------------

-- | Convert into the target language.  This does not make much of a
-- change, but it checks the changes that have already occurred.
--
-- The only substantitive conversion here is of tupled arguments to
-- multiple argument functions.
lower :: Bool -> L2.Prog -> SyM T.Prog
lower pkd L2.Prog{fundefs,ddefs,mainExp} = do
  mn        <- case mainExp of
                 Nothing    -> return Nothing
                 Just (x,_) -> (Just . T.PrintExp) <$> tail x
--  funs       <- mapM fund (M.elems fundefs) 
--  unpackers  <- mapM genUnpacker (M.elems ddefs) 
--  T.Prog <$> pure (funs ++ unpackers) <*> pure mn
  T.Prog <$> mapM fund (M.elems fundefs) <*> pure mn
 where
  fund :: L2.FunDef -> SyM T.FunDecl
  fund L2.FunDef{funname,funty=(L2.ArrowTy inty _ outty),funarg,funbod} = do
      (args,bod) <- dbgTrace 1 ("Inspecting function with input type: "++show inty) $
                    case inty of
                      -- ASSUMPTION: no nested tuples after unariser:
                      L2.ProdTy ls -> do let tys'  = L.map (fmap (const ())) ls
                                             tys'' = L.map typ ls
                                         (vs,e') <- eliminateProjs funarg tys' funbod
                                         return $
                                          dbgTrace 5 (" [lower] unzipping funarg "++show funarg++" to "++show vs) $
                                          (zip vs tys'', e')
                      _ -> return ([(funarg, typ inty)], funbod)
      tl <- tail bod
      return T.FunDecl{ T.funName = funname
                      , T.funArgs = args
                      , T.funRetTy = typ outty
                      , T.funBody = tl }

  tail :: L1.Exp -> SyM T.Tail
  tail ex0 =
   dbgTrace 7 ("\n [lower] processing tail:\n  "++sdoc ex0) $
   case ex0 of

    -- HACK! We don't have LetSwitchT yet.  This means potential exponential code duplication:
    -- LetE (_,_, CaseE _ _) _ ->
    --    error "lower: unfinished, we cannot let-bind the result of a switch yet."
    LetE (vr,ty, CaseE scrt ls) bod -> tail $
                                       dbgTrace 1 ("WARNING: Let-bound CasE, code duplication of this body:\n  "
                                                   ++sdoc bod)$
         -- For now just duplicate code:
         CaseE scrt [ (k,vs, mkLet (vr,ty,e) bod)
                    | (k,vs,e) <- ls]

    -- Aaand... if we're going to push Let's under Case's, we have to repeat this bit of flattening:
    LetE (v1, t1, LetE (v2,t2,rhs2) rhs1) bod ->
       tail $ LetE (v2,t2,rhs2) $ LetE (v1,t1,rhs1) bod

    --------------------------------------------------------------------------------
    -- Packed codegen
    --------------------------------------------------------------------------------
    -- These are in a funny normal form atfer cursor insertion.  They take one cursor arg.
    -- They basically are a WriteTag.
    LetE (cursOut, _, MkPackedE k ls) bod | pkd -> do
      case ls of
       [cursIn] -> T.LetPrimCallT [(cursOut,T.CursorTy)] T.WriteTag
                     [ T.TagTriv (getTagOfDataCon ddefs k)
                     , triv "WriteTag cursor" cursIn ] <$>
                    tail bod
       _ -> error$ "Lower: Expected one argument to data-constructor (which becomes WriteTag): "
                   ++sdoc (MkPackedE k ls)

    -- Likewise, Case really means ReadTag.  Argument is a cursor.
    CaseE (VarE scrut) ls | pkd -> do
        let (last:restrev) = reverse ls; rest = reverse restrev
        tagtmp <- gensym "tmpval"
        ctmp   <- gensym "tmpcur"
        -- We only need to thread one value through, the cursor resulting from read.
        let doalt (k,ls,rhs) =
             (getTagOfDataCon ddefs k,) <$>
             case ls of
               []  -> tail rhs -- AUDITME -- is this legit, or should it have one cursor param anyway?
               [c] -> tail (L1.subst c (VarE ctmp) rhs)
        alts <- mapM doalt rest
        (_,last') <- doalt last
        return $
         T.LetPrimCallT [(tagtmp,T.TagTy),(ctmp,T.CursorTy)] T.ReadTag [T.VarTriv scrut] $
          T.Switch (T.VarTriv tagtmp)
                   (T.TagAlts alts)
                   (Just last')

    --------------------------------------------------------------------------------
    -- Not-packed, pointer-based codegen
    --------------------------------------------------------------------------------
    -- In pointer-based representation we don't use `TagTy`, because that's
    -- causing problems because by default gcc aligns struct fields but we don't
    -- take that into account in our codegen.
    --
    -- If we get here that means we're NOT packing trees on this run:
    -- Thus this operates on BOXED data:
    CaseE e [(c, bndrs, rhs)] | not pkd -> do
      -- a product, directly assign the fields
      let tys = L.map typ (lookupDataCon ddefs c)

      -- TODO(osa): enable this
      -- ASSERT(length tys == length bndrs)

      let T.VarTriv e_var = triv "product case scrutinee" e
      rhs' <- tail rhs
      return (T.LetUnpackT (zip bndrs tys) e_var rhs')

    CaseE e (def_alt : alts) | not pkd -> do
      tag_bndr <- gensym "tag"
      tail_bndr <- gensym "tail"

      let
        e_triv = triv "sum case scrutinee" e

        mk_alt :: (Constr, [Var], Exp) -> SyM (Int64, T.Tail)
        mk_alt (con, bndrs, rhs) = do
          let
            con_tag = getTagOfDataCon ddefs con
            bndr_tys = L.map typ (lookupDataCon ddefs con)
          rhs' <- tail rhs
          return ( fromIntegral con_tag, T.LetUnpackT (zip bndrs bndr_tys) tail_bndr rhs' )

      alts'    <- mapM mk_alt alts
      (_, def) <- mk_alt def_alt

      return $
        T.LetPrimCallT
          [(tag_bndr, T.IntTy), (tail_bndr, T.CursorTy)]
          T.ReadInt
          [e_triv]
          (T.Switch (T.VarTriv tag_bndr) (T.IntAlts alts') (Just def))

    -- Accordingly, constructor allocation becomes an allocation.
    LetE (v, _, MkPackedE k ls) bod | not pkd -> L1.assertTrivs ls $ do
      let tycon    = getTyOfDataCon ddefs k
          all_cons = dataCons (lookupDDef ddefs tycon)
          is_prod  = length all_cons == 1
          tag      = fromJust (L.findIndex ((==) k . fst) all_cons)

          field_tys= L.map typ (lookupDataCon ddefs k)
          fields0  = fragileZip field_tys (L.map (triv "MkPackedE args") ls)
          fields
            | is_prod   = fields0
            | otherwise = (T.IntTy, T.IntTriv (fromIntegral tag)) : fields0

      -- trace ("data con: " ++ show k) (return ())
      -- trace ("is_prod: " ++ show is_prod) (return ())
      -- trace ("fields: " ++ show fields) (return ())

      bod' <- tail bod

      return (T.LetAllocT v fields bod')

    -- This is legitimately flattened, but we need to move it off the spine:
    L1.MkPackedE k ls -> do
       tmp <- gensym "tailift"
       let ty = L1.PackedTy (getTyOfDataCon ddefs k) ()
       tail $ LetE (tmp, ty, ex0) (VarE tmp)
             
    --------------------------------------------------------------------------------
             
--    L1.LitE n       -> pure$ T.RetValsT [triv "literal in tail" (LitE n)]
    L1.MkProdE ls   -> pure$ T.RetValsT (L.map (triv "returned element of tuple") ls)
    e | L1.isTriv e -> pure$ T.RetValsT [triv "<internal error1>" e]
                       
    -- L1.LetE (v,t, L1.MkProdE ls) bod -> do
    --   let rhss = L.map triv ls
    --   vsts <- unzipTup v t
    --   let go _ [] = tail bod
    --       go ix ((v1,t1):rst) = T.LetTrivT (v1,t1, )

    -- We could eliminate these ahead of time (unariser):
    -- FIXME: Remove this when that is done a priori:
    L1.LetE (v, L2.ProdTy tys, MkProdE ls) bod -> do
      (tmps,bod') <- eliminateProjs v tys bod
      -- Bind tmps individually:a
      let go [] acc                 = acc
          go ((pvr,pty,rhs):rs) acc = go rs (LetE (pvr,pty,rhs) acc)
      -- Finally reprocess teh whole thing:
      tail (go (zip3 tmps tys ls) bod')

    -- We could eliminate these ahead of time:
    L1.LetE (v,t,rhs) bod | L1.isTriv rhs ->
      T.LetTrivT (v,typ t, triv "<internal error2>" rhs) <$> tail bod

    -- TWO OPTIONS HERE: we could push equality prims into the target lang.
    -- Or we could map directly onto the IfEqT form:
    -- L1.IfE (L1.PrimAppE L1.EqP __ ) b c -> __

    L1.IfE a b c       -> do b' <- tail b
                             c' <- tail c
                             return $ T.Switch (triv "if test" a)
                                      -- If we are treating the boolean as a tag, then tag "0" is false
                                      (T.IntAlts [(0, c')])
                                      -- And tag "1" is true:
                                      (Just b')

    LetE (vr, ty, L1.TimeIt rhs _ flg) bod ->
        do rhs' <- tail rhs
           case ty of 
             L2.ProdTy ls -> 
               do (tmps,bod') <- eliminateProjs vr ls bod
                  T.LetTimedT flg (zip tmps (L.map typ ls)) rhs' <$> tail bod'
             _ -> T.LetTimedT flg   [(vr, typ ty)]          rhs' <$> tail bod


    --------------------------------Start PrimApps----------------------------------
    -- (1) Primapps that become Tails:

    -- FIXME: No reason errors can't stay primitive at Target:
    L1.PrimAppE (L1.ErrorP str _ty) [] ->
      pure $ T.ErrT str
    L1.LetE (_,_,L1.PrimAppE (L1.ErrorP str _) []) _ ->
      pure $ T.ErrT str

    -- Whatever... a little just-in-time flattening.  Should obsolete this:
    L1.PrimAppE p ls -> do
      tmp <- gensym "flt"
      tail (L1.LetE (tmp, L1.primRetTy p, L1.PrimAppE p ls) (L1.VarE tmp))

    ---------------------
    -- (2) Next FAKE Primapps.  These could be added to L1 if we wanted to pollute it.
    L1.LetE (v,_,C.WriteInt c e) bod ->
      T.LetPrimCallT [(v,T.CursorTy)] T.WriteInt [triv "WriteTag arg" e, T.VarTriv c] <$>
         tail bod

    L1.LetE (pr,_,C.ReadInt c) bod -> do
      vtmp <- gensym "tmpval"
      ctmp <- gensym "tmpcur"
      T.LetPrimCallT [(vtmp,T.IntTy),(ctmp,T.CursorTy)] T.ReadInt [T.VarTriv c] <$>
        -- Here we lamely chase down all the tuple references and make them variablesa:
        let bod' = L1.substE (L1.ProjE 0 (L1.VarE pr)) (L1.VarE vtmp) $
                   L1.substE (L1.ProjE 1 (L1.VarE pr)) (L1.VarE ctmp) bod
        in
          dbgTrace 5 (" [lower] ReadInt, after substing references to "++pr++":\n  "++sdoc bod')$
          tail bod'

    L1.LetE (v,_,C.NewBuffer) bod ->
      T.LetPrimCallT [(v,T.CursorTy)] T.NewBuf [] <$>
         tail bod

    L1.LetE (v,_,C.ScopedBuffer) bod ->
      T.LetPrimCallT [(v,T.CursorTy)] T.ScopedBuf [] <$>
         tail bod

    ---------------------
    -- (3) Proper primapps.
    L1.LetE (v,t,L1.PrimAppE p ls) bod ->
        -- No tuple-valued prims here:
        T.LetPrimCallT [(v,typ t)]
             (prim p)
             (L.map (triv $ "prim rand "++show p) ls) <$>
             (tail bod)
    --------------------------------End PrimApps----------------------------------

    --
    L1.AppE v e | notSpecial ex0 -> return $ T.TailCall ( v) [triv "operand" e]

    -- Tail calls are just an optimization, if we have a Proj/App it cannot be tail:
    ProjE ix ap@(AppE f e) | notSpecial ap -> do
        tmp <- gensym "prjapp"
        let L2.ArrowTy (L2.ProdTy inTs) _ _ = funty (fundefs # f)
        tail $ LetE ( tmp
                    , fmap (const ()) (inTs !! ix)
                    , ProjE ix (AppE f e))
                 (VarE tmp)


    L1.LetE (_,_,L1.AppE f _) _ | M.notMember f fundefs ->
      error $ "Application of unbound function: "++show f

    -- Non-tail call:
    L1.LetE (vr,t, projOf -> (stk, L1.AppE f arg)) bod -> do
        let L2.ArrowTy _ _ outTy = funty (fundefs # f)
        let f' = cleanFunName f
        (vsts,bod') <- case outTy of
                        L1.ProdTy [] -> error "lower: FINISHME: unit valued function"
                        L1.ProdTy tys ->
                          case stk of
                            [] -> do (tmps,e) <- eliminateProjs vr (L.map (fmap (const ())) tys) bod
                                     return (zip tmps (L.map typ tys), e)
                            -- More than one should not currently be
                            -- possible (no nested tuple returns):
                            [ix] -> do garbages <- sequence [ gensym "garbage" | _ <- L.tail tys ]
                                       let (lead,trail) = L.splitAt ix garbages
                                       return ( zip (lead++[vr]++trail)
                                                    (L.map typ tys)
                                              , bod)
                        _ -> return ([(vr,typ t)], bod)
        case arg of
          MkProdE es ->
               T.LetCallT vsts f' (L.map (triv "one of app rands") es) <$> (tail bod')
          _ -> T.LetCallT vsts f' [(triv "app rand") arg]       <$> (tail bod')


    L1.LetE (v, t, L1.IfE a b c) bod -> do
      let a' = triv "if test" a
      b' <- tail b
      c' <- tail c
      case t of
        -- Finilize unarisation:
        L2.ProdTy ls -> do
             (tmps,bod') <- eliminateProjs v ls bod
             T.LetIfT (zip tmps (L.map typ ls)) (a', b', c') <$> tail bod'
        _ -> T.LetIfT [(v, typ t)] (a', b', c') <$> tail bod


    _ -> error$ "lower: unexpected expression in tail position:\n  "++sdoc ex0

-- | View pattern for matching agaist projections of Foo rather than just Foo.
projOf (ProjE ix e) = let (stk,e') = projOf e in
                      (stk++[ix], e')
projOf e = ([],e)


-- | Make sure an AppE doesn't encode one of our "virtual primops":
notSpecial :: Exp -> Bool
notSpecial ap =
  case ap of
   C.WriteInt _ _ -> False
   C.NewBuffer    -> False
   C.ScopedBuffer -> False
   C.ReadInt _    -> False
   _              -> True

{-
-- | Go under bindings and transform the very last return point.
chainTail :: T.Tail -> (T.Tail -> T.Tail) -> T.Tail
chainTail tl fn =
  case tl of
    T.LetCallT   bnd rat rnds bod -> T.LetCallT   bnd rat rnds (chainTail bod fn)
    T.LetPrimCallT bnd p rnds bod -> T.LetPrimCallT bnd p rnds (chainTail bod fn)
    T.LetTrivT  bnd           bod -> T.LetTrivT           bnd  (chainTail bod fn)
    T.LetIfT bnd pr bod           -> T.LetIfT           bnd pr (chainTail bod fn)
    T.LetAllocT lhs vals bod      -> T.LetAllocT     lhs vals  (chainTail bod fn)
    -- Question here is whether we plan to go under Ifs and Cases...
    -- T.IfE a b c -> T.IfE a (chainTail b fn) (chainTail c fn)
    oth -> fn oth

-- | Create the right kind of Target let binding based on the form of the RHS:a
mkLetTail :: (Var,L2.Ty, T.Tail) -> T.Tail -> T.Tail
mkLetTail (vr,ty,rhs) =
  case rhs of
    RetValsT [one] -> __
    _ -> __
-}

-- | Eliminate projections from a given tuple variable.  INEFFICIENT!
eliminateProjs :: Var -> [L1.Ty] -> Exp -> SyM ([Var],Exp)
eliminateProjs vr tys bod =
 dbgTrace 5 (" [lower] eliminating "++show (length tys)++
             " projections on variable "++show vr++" in expr with types "++show tys++":\n   "++sdoc bod) $
 do
    tmps <- mapM (\_ -> gensym "pvrtmp") [1.. (length tys)]


    let go _ [] acc =
            -- If there are ANY references left, we are forced to make the products:
            L1.subst vr (MkProdE (L.map VarE tmps)) acc
        go ix ((pvr,_pty):rs) acc =
           go (ix+1) rs
             (L1.substE (ProjE ix (VarE vr)) (VarE pvr) acc)
    let bod' = go 0 (zip tmps tys) bod
    return (tmps,bod')



mkLet :: (Var, L1.Ty, Exp) -> Exp -> Exp
mkLet (v,t,LetE (v2,t2,rhs2) bod1) bod2 = LetE (v2,t2,rhs2) $ LetE (v,t,bod1) bod2
mkLet (v,t,rhs) bod = LetE (v,t,rhs) bod



triv :: String -> L1.Exp -> T.Triv
triv msg e0 =
  case e0 of
    (L1.VarE x) -> T.VarTriv x
    (L1.LitE x) -> T.IntTriv (fromIntegral x) -- TODO: back propogate Int64 toL1
    -- Bools become ints:
    (L1.PrimAppE L1.MkTrue [])  -> T.IntTriv 1
    (L1.PrimAppE L1.MkFalse []) -> T.IntTriv 0
    -- TODO: I think we should allow tuples and projection in trivials:

    -- Heck, let's map Unit onto Int too:
    (L1.MkProdE []) -> T.IntTriv 0
--      (ProjE x1 x2) -> __
--      (MkProdE x) -> __
    _ | L1.isTriv e0 -> error $ "lower/triv: this function is written wrong.  "++
                         "It won't handle the following, which satisfies 'isTriv':\n "++sdoc e0++
                         "\nMessage: "++msg
    _ -> error $ "lower/triv, expected trivial in "++msg++", got "++sdoc e0

typ :: L1.Ty1 a -> T.Ty
typ t =
  case t of
    L1.IntTy  -> T.IntTy
    L1.SymTy  -> T.SymTy
    L1.BoolTy -> T.IntTy
    L1.ListTy{} -> error "lower/typ: FinishMe: List types"
    L1.ProdTy xs -> T.ProdTy $ L.map typ xs
    L1.SymDictTy x -> T.SymDictTy $ typ x
    -- t | isCursorTy t -> T.CursorTy
    L1.PackedTy{} -> T.PtrTy

prim :: L1.Prim -> T.Prim
prim p =
  case p of
    L1.AddP -> T.AddP
    L1.SubP -> T.SubP
    L1.MulP -> T.MulP
    L1.EqSymP -> T.EqP
    L1.EqIntP -> T.EqP
    L1.SizeParam -> T.SizeParam
    L1.DictInsertP ty -> T.DictInsertP $ typ ty
    L1.DictLookupP ty -> T.DictLookupP $ typ ty
    L1.DictEmptyP ty -> T.DictEmptyP $ typ ty
    L1.ErrorP{} -> error$ "lower/prim: internal error, should not have got to here: "++show p

    L1.MkTrue  -> error "lower/prim: internal error. MkTrue should not get here."
    L1.MkFalse -> error "lower/prim: internal error. MkFalse should not get here."

