{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- | Eliminate tuples except under special circumstances expected by
-- Lower.

-- WARNING: seeded with DUPLICATED code from InlinePacked

module Packed.FirstOrder.Passes.Unariser
    (unariser) where

import Data.Maybe
import Packed.FirstOrder.Common (SyM, Var, dbgTrace, sdoc, gensym, fragileZip,ndoc,toVar)
import qualified Packed.FirstOrder.L1_Source as L1
import Packed.FirstOrder.L2_Traverse as L2
import Prelude hiding (exp)
import qualified Data.Map as M

-- | This pass gets ready for Lower by converting most uses of
-- projection and tuple-construction into finer-grained bindings.
--
-- OUTPUT INVARIANTS:
--
-- (1) only flat tuples as function arguments (no nesting), all
-- arguments immediately present, e.g. `AppE "f" (MkProd [x,y,z])`
--  rather than `AppE "f" (MkProdE [x,MkProdE[y,z]])`
--
-- (2) The only MkProdE allowed outside of function operands is within
-- return/tail position (of a function or If branch).
--
-- (3) Primitives are allowed to return tuples, but are let-bound
-- (these will turn into LetPrimCall).  The references to these tuples
-- are all of the form `ProjE i (VarE v)` and they are then
-- transformed to varrefs in lower.
--
unariser :: L2.Prog -> SyM L2.Prog
unariser prg = do
  prg' <- mapMExprs unariserExp prg
  fns  <- mapM unariserFun $ M.elems (fundefs prg')
  let fns' = M.fromList $ map (\f -> (L2.funname f,f)) fns
  return prg'{fundefs = fns'}


-- | A projection stack can be viewed as a list of ProjE operations to
-- perform, from left to right.
type ProjStack = [Int]

-- | Maps variables onto tuples of (projections of) other variables
type Env = [(Var,[(ProjStack,Var)])]


-- | Modifies function to satisfy output invariant (1)
--
unariserFun :: L2.FunDef -> SyM L2.FunDef
unariserFun f@L2.FunDef{funty,funarg,funbod} = do
  let inT = arrIn funty

      flattenTy :: Ty -> [Ty]
      flattenTy (ProdTy tys) = concatMap flattenTy tys
      flattenTy ty = [ty]

      -- | Generate projections for non-product types inside a tuple
      --
      -- Examples:
      -- (1) ProdTy [IntTy, ProdTy [IntTy, IntTy, IntTy]] ==
      --     [[0],[1,0],[1,1],[1,2]]
      --
      -- (2) ProdTy [IntTy, ProdTy [IntTy, IntTy, IntTy, ProdTy [IntTy, IntTy]]] ==
      --     [[0],[1,0],[1,1],[1,2],[1,3,0],[1,3,1]]
      --
      projections :: Ty -> ProjStack -> [ProjStack]
      projections (ProdTy tys) acc =
        concatMap (\(ty,i) -> projections ty (acc ++ [i])) (zip tys [0..])
      projections _ acc = [acc]

  case inT of
    ProdTy tys -> do
      let projs = projections inT []
          substs = map (\ps -> (foldr (\i acc -> ProjE i acc) (VarE funarg) ps,
                                ProjE (sum ps) (VarE funarg)))
                   projs
          -- FIXME: This is in-efficient because of the substE ?
          bod = foldr (\(from,to) acc -> L1.substE from to acc) funbod substs
      return f{funbod = bod, funty = funty{arrIn = ProdTy $ concatMap flattenTy tys}}

    _ -> return f


-- | Take an ignored argument to match mapMExprs' conventions.
--
-- In the recursive "go" worker here, we maintain:
--   (1) pending projections that enclose our current context, and
--   (2) a map from variable bindings with tuple type, to
--       finer-grained bindings to individual components.
--
unariserExp :: ignored -> L1.Exp -> SyM L1.Exp
unariserExp _ = go [] []
  where
  var v = v
  l ! i = if i <= length l
          then l!!i
          else error$ "unariserExp: attempt to project index "++show i++" of list:\n "++sdoc l

  -- | Reify a stack of projections.
  discharge [] e = e
  discharge (ix:rst) (MkProdE ls) = discharge rst (ls ! ix)
  discharge (ix:rst) p | isExtendedPattern p =
     error $ "Cannot discharge projections directly agains extended L2 form: "++ndoc p
  discharge (ix:rst) e = discharge rst (ProjE ix e)

  flattenExp :: Exp -> [Exp]
  flattenExp (MkProdE es) = concatMap flattenExp es
  flattenExp e = [e]

  -- FIXME: need to track full expr binds like InlineTrivs
  -- Or do we?  Not clear yet.
  go :: ProjStack -> Env -> L1.Exp -> SyM L1.Exp
  go stk env e0 =
   dbgTrace 7 ("Unariser processing with stk "++
               ndoc stk++", env: "++ndoc env++"\n exp: "++sdoc e0) $
   case e0 of
    (MkProdE es) -> case stk of
                      (ix:s') -> go s' env (es ! ix)
                      [] -> MkProdE <$> mapM (go stk env) (concatMap flattenExp es)

    -- (ProjE ix (VarE v)) -> discharge stk <$> -- Danger.
    --                        case lookup v env of
    --                          Just vs -> pure$ let (stk,v') = vs ! ix in
    --                                           applyProj (ix:stk, v')
    --                          Nothing -> pure$ VarE v -- This must be one that Lower can handle.
    (ProjE i e)  -> go (i:stk) env e  -- Push a projection inside lets or conditionals.

    (VarE v) -> case lookup v env of
                  Nothing -> pure$ discharge stk $ VarE (var v)
                  -- Reprocess after substituting in case they were not terminal after all:a
                  Just vs -> go stk env (L1.mkProd (map applyProj vs)) -- Works for var-to-var aliases.

    LetE (vr,ty, CaseE scrt ls) bod | isCheap bod ->
         go stk env $
          CaseE scrt [ (k,vs, mkLet (vr,ty,e) bod)
                     | (k,vs,e) <- ls]

    -- Flatten so that we can see what's stopping us from unzipping:
    (LetE (v1,t1, LetE (v2,t2,rhs2) rhs1) bod) -> do
         go stk env $ LetE (v2,t2,rhs2) $ LetE (v1,t1,rhs1) bod

    -- TEMP: HACK/workaround.  See FIXME above.
    -- LetE (v1,ProdTy _,rhs@LetE{})  (ProjE ix (VarE v2)) | v1 == v2 -> go (ix:stk) env rhs
    LetE (v1,ProdTy _,rhs@CaseE{}) (ProjE ix (VarE v2)) | v1 == v2 -> go (ix:stk) env rhs

    ----- These three cases are permitted to remain tupled by Lower: -----
    (LetE (v,ty@ProdTy{}, rhs@IfE{})   bod)-> LetE <$> ((v,ty,) <$> go [] env rhs) <*> go stk env bod
    (LetE (v,ty@ProdTy{}, rhs@CaseE{}) bod)-> LetE <$> ((v,ty,) <$> go [] env rhs) <*> go stk env bod
    (LetE (v,ty@ProdTy{}, rhs@AppE{})  bod)-> LetE <$> ((v,ty,) <$> go [] env rhs) <*> go stk env bod
    ------------------

    (LetE (vr,ProdTy tys, MkProdE ls) bod) -> do
        vs <- sequence [ gensym (toVar "unzip") | _ <- ls ]
        let -- Here's a little bit of extra complexity to NOT introduce var/var copies:
            (mbinds,substs) = unzip
                              [ case projOfVar e of
                                  Just pr -> (Nothing, pr)
                                  Nothing -> (Just (v,t,e), ([],v))
                              | (v,t,e) <- (zip3 vs tys ls) ]
            binds = catMaybes mbinds
            env' = (vr, substs):env

        -- Here we *reprocess* the results in case there is more unzipping to do:
        go stk env' $ mklets binds bod

    -- Bulk copy prop, WRONG:
    -- (LetE (v1,ProdTy tys, VarE v2) bod) ->
    --     case lookup v2 env of
    --       Just vs -> go stk env $ LetE (v1,ProdTy tys, MkProdE (map VarE vs)) bod
    --       Nothing -> go stk ((v1,[v2]):env) bod -- Copy-prop

    -- More nuanced copy-prop:
    (LetE (v1,ProdTy tys, proj) bod) | Just (stk2,v2) <- projOfVar proj ->
        let env' = buildAliases (v1,tys) (stk2,v2) env in
        go stk env' bod
        -- case lookup v2 env of
        --   Just vs -> go stk env $ LetE (v1,ProdTy tys, MkProdE (map VarE vs)) bod
        --   -- This is problematic:
        --   -- Nothing -> go stk ((v1,[v2]):env) bod -- Copy-prop
        --   Nothing -> LetE <$> ((v1,ProdTy tys) <$> go [] proj) <*>
        --                 go stk ((v1,Nothing):env) bod

    -- And this is a HACK.  Need a more general solution:
    (LetE (v,ty@ProdTy{}, rhs@(TimeIt{})) bod)->
        LetE <$> ((v,ty,) <$> go [] env rhs) <*> go stk env bod
    ------------------

    (LetE (_,ProdTy _, _) _) ->
        error$ " [unariser] this is stopping us from unzipping a tupled binding:\n "++sdoc e0

    (LetE (v,t,rhs) e) -> LetE <$> ((v,t,) <$> go [] env rhs) <*>
                            (go stk env e)

    (LitE i) | [] <- stk -> pure$ LitE i
             | otherwise -> error $ "Impossible. Non-empty projection stack on LitE "++show stk
    (LitSymE v) | [] <- stk -> pure $ LitSymE v
                | otherwise -> error $ "Impossible. Non-empty projection stack on LitSymE "++show stk

    (PrimAppE p es) -> discharge stk <$>
                        PrimAppE p <$> mapM (go stk env) es

    -- TODO: these need to be handled by lower to become varrefs into a multi-valued return.
    (AppE f e)  -> discharge stk <$> AppE f <$> go [] env e

    (IfE e1 e2 e3) ->
         IfE <$> go [] env e1 <*> go stk env e2 <*> go stk env e3
    (CaseE e ls) -> CaseE <$> go [] env e <*>
                     sequence [ (k,ls,) <$> go stk env x
                              | (k,ls,x) <- ls ]

    (MkPackedE c es)
        | [] <- stk -> MkPackedE c <$> mapM (go [] env) es
        | otherwise -> error $ "Impossible. Non-empty projection stack on MkPackedE: "++show stk

    (TimeIt e ty b) -> do
       -- Put this in the form Lower wants:
       tmp <- gensym $ toVar "timed"
       e' <- go stk env e
       return $ LetE (tmp,ty, TimeIt e' ty b) (VarE tmp)

    MapE{}  -> error "FINISHLISTS"
    FoldE{} -> error "FINISHLISTS"
    -- (MapE (v,t,e') e) -> let env' = (v,Nothing) : env in
    --                      MapE (var v,t,go stk env e') (go stk env' e)
    -- (FoldE (v1,t1,e1) (v2,t2,e2) e3) ->
    --      let env' = (v1,Nothing) : (v2,Nothing) : env in
    --      FoldE (var v1,t1,go stk env e1) (var v2,t2,go stk env e2)
    --            (go stk env' e3)

isCheap :: Exp -> Bool
isCheap _ = True

applyProj :: (ProjStack,Var) -> Exp
applyProj (stk,v) = go stk (VarE v)
  where
    go [] e     = e
    go (i:is) e = go is (ProjE i e)

projOfVar :: Exp -> Maybe (ProjStack, Var)
projOfVar = lp []
 where
   lp stk (VarE v)    = Just (stk,v)
   lp stk (ProjE i e) = lp (i:stk) e
   lp _   _           = Nothing

-- | Binnd v1 to a projection of v2 in the current environment.
buildAliases :: (Var,[L1.Ty]) -> (ProjStack, Var) -> Env -> Env
buildAliases (v1,tys) (stk,v2) env =
  let maxIx = length tys - 1 in
  let new = case lookup v2 env of
             -- We cannot inline v2, it must come from a function return or something.
             -- So instead we can still unzip ourselves, and reference v2.
             Nothing -> ( v1, [ ([ix],v2) | ix <- [0..maxIx] ] )
             -- When we get a hit, we expect it to have the right number of entries:
             Just hits ->
                 -- We are bound to a PROJECTION of v2, so combine stk with what's already there.
                 (v1, [ (s ++ stk, v')
                      | (_ix,(s,v')) <- fragileZip [0..maxIx] hits ])
  in dbgTrace 5 (" [unariser] Extending environment with these mappings: "++show new) $
     new : env

mklets :: [(Var, L1.Ty, Exp)] -> Exp -> Exp
mklets [] bod = bod
mklets (bnd:rst) bod = LetE bnd $ mklets rst bod


mkLet :: (Var, L1.Ty, Exp) -> Exp -> Exp
mkLet (v,t,LetE (v2,t2,rhs2) bod1) bod2 = LetE (v2,t2,rhs2) $ LetE (v,t,bod1) bod2
mkLet (v,t,rhs) bod = LetE (v,t,rhs) bod
