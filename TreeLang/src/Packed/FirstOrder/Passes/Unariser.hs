-- | Eliminate tuples except under special circumstances expected by
-- Lower.

-- WARNING: seeded with DUPLICATED code from InlinePacked

module Packed.FirstOrder.Passes.Unariser
    (unariser) where

import qualified Data.Map as M    
import Packed.FirstOrder.Common (SyM, Var, dbgTrace, sdoc, gensym)
import qualified Packed.FirstOrder.L1_Source as L1
import Packed.FirstOrder.LTraverse as L2
import Prelude hiding (exp)

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
unariser = mapMExprs unariserExp 
-- unariser prg@L2.Prog{fundefs,mainExp} = return $
 --  prg { fundefs = M.map fd fundefs 
 --      , mainExp = case mainExp of
 --                    Nothing      -> Nothing
 --                    (Just (e,t)) -> Just (unariserExp [] [] e, t)
 --      }
 -- where
 --   fd f@FunDef{funarg, funbod} =
 --       f { funbod = unariserExp [] [] funbod }

type ProjStack = [Int]
         
-- | Keep (1) pending projections that enclose our current context,
-- and (2) a map from variable bindings with tuple type, to
-- finer-grained bindings to individual components.
--
unariserExp :: ignored -> L1.Exp -> SyM L1.Exp
unariserExp _ = go [] [] 
  where
  var v = v
  mklets [] bod = bod
  mklets (bnd:rst) bod = LetE bnd $ mklets rst bod

  l ! i = if i <= length l
          then l!!i
          else error$ "unariserExp: attempt to project index "++show i++" of list:\n "++sdoc l
                         
  discharge [] e = e
  discharge (ix:rst) (MkProdE ls) = discharge rst (ls ! ix)
  discharge (ix:rst) e = discharge rst (ProjE ix e)

  -- FIXME: need to track full expr binds like InlineTrivs
  go :: ProjStack -> [(Var,[Var])] -> L1.Exp -> SyM L1.Exp
  go stk env e0 =
   -- dbgTrace 5 ("Inline, processing with env:\n "++sdoc env++"\n exp: "++sdoc e0) $
   case e0 of
    -- TEMP: HACK/workaround.  See FIXME above.
    LetE (v1,ProdTy tys,rhs) (ProjE ix (VarE v2)) | v1 == v2 ->
       go (ix:stk) env rhs
     
    (ProjE i e)  -> go (i:stk) env e  -- Push a projection inside lets or conditionals.
    (MkProdE es) -> case stk of
                      (ix:s') -> go s' env (es ! ix)
                      [] -> MkProdE <$> mapM (go stk env) es

    (ProjE ix (VarE v)) -> discharge stk <$> -- Danger.
                           case lookup v env of
                             Just vs -> pure$ VarE (vs ! ix)
                             Nothing -> pure$ VarE v -- This must be one that Lower can handle.
    (VarE v) -> discharge stk <$> 
                 case lookup v env of
                  Nothing -> pure$ VarE (var v)
                  Just vs -> pure$ MkProdE [ VarE v | v <- vs ]

    -- Flatten so that we can see what's stopping us from unzipping:
    (LetE (v1,t1, LetE (v2,t2,rhs2) rhs1) bod) -> do
         go stk env $ LetE (v2,t2,rhs2) $ LetE (v1,t1,rhs1) bod

    (LetE (v,ProdTy tys, MkProdE ls) e) -> do
        vs <- sequence [ gensym "unzip" | _ <- ls ]
        let env' = (v,vs):env
        -- Here we *reprocess* the results in case there is more unzipping to do:
        go stk env' $ mklets (zip3 vs tys ls) e

    ----- These three cases are permitted to remain tupled by Lower: -----
    (LetE (v,ty@ProdTy{}, rhs@IfE{})   bod)-> LetE <$> ((v,ty,) <$> go [] env rhs) <*> go stk env bod
    (LetE (v,ty@ProdTy{}, rhs@CaseE{}) bod)-> LetE <$> ((v,ty,) <$> go [] env rhs) <*> go stk env bod
    (LetE (v,ty@ProdTy{}, rhs@AppE{})  bod)-> LetE <$> ((v,ty,) <$> go [] env rhs) <*> go stk env bod

    -- And this is a HACK.  Need a more general solution:
    (LetE (v,ty@ProdTy{}, rhs@(TimeIt _ _)) bod)->
        LetE <$> ((v,ty,) <$> go [] env rhs) <*> go stk env bod
    ------------------
    (LetE (_,ProdTy _, _) _) ->
        error$ " [unariser] this is stopping us from unzipping a tupled binding:\n "++sdoc e0
        
    (LetE (v,t,rhs) e) -> LetE <$> ((v,t,) <$> go [] env rhs) <*>
                            (go stk env e)

    (LitE i)    -> case stk of [] -> pure$ LitE i

    -- TODO: these need to be handled by lower to become varrefs into a multi-valued return.
    (AppE f e)  -> discharge stk <$> AppE f <$> go [] env e
    (PrimAppE p es) -> discharge stk <$>
                        PrimAppE p <$> mapM (go stk env) es

    (IfE e1 e2 e3) ->
         IfE <$> go [] env e1 <*> go stk env e2 <*> go stk env e3
    (CaseE e ls) -> CaseE <$> go [] env e <*>
                     sequence [ (k,ls,) <$> go stk env x
                              | (k,ls,x) <- ls ]

    (MkPackedE c es) -> case stk of [] -> MkPackedE c <$> mapM (go [] env) es
    (TimeIt e ty) -> do
       -- Put this in the form Lower wants:
       tmp <- gensym "timed"
       e' <- go stk env e
       return $ LetE (tmp,ty, TimeIt e' ty) (VarE tmp)

    -- (MapE (v,t,e') e) -> let env' = (v,Nothing) : env in
    --                      MapE (var v,t,go stk env e') (go stk env' e)
    -- (FoldE (v1,t1,e1) (v2,t2,e2) e3) ->
    --      let env' = (v1,Nothing) : (v2,Nothing) : env in
    --      FoldE (var v1,t1,go stk env e1) (var v2,t2,go stk env e2)
    --            (go stk env' e3)

