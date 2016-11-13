
-- | Compiler pass to inline trivials.
module Packed.FirstOrder.Passes.InlineTriv (inlineTriv, inlineTrivExp) where
    
import Packed.FirstOrder.Common
import Packed.FirstOrder.L1_Source as L1 hiding (mkProj)
import Packed.FirstOrder.Passes.Flatten (typeExp, TEnv)
import Prelude hiding (exp)
-- import Debug.Trace

-- | Inline trivial let bindings (binding a var to a var or int), mainly to clean up
--   the output of `flatten`.
inlineTriv :: Prog -> Prog
inlineTriv (Prog ddefs funs main) =
    Prog ddefs (fmap inlineTrivFun funs) (fmap (inlineTrivExp ddefs) main)
  where
    inlineTrivFun (FunDef nam (narg,targ) ty bod) =
      FunDef nam (narg,targ) ty (inlineTrivExp ddefs bod)

inlineTrivExp :: DDefs _ -> Exp -> Exp
inlineTrivExp _ddefs = go []
  where

  -- Just a hook for debugging:
  go :: [(Var,Exp)] -> Exp -> Exp
  go env e  =
      -- dbgTrace 7 ("Inline, processing with env:\n "++sdoc env++"\n exp: "++sdoc e) $
      exp env e          
      
  exp :: [(Var,Exp)] -> Exp -> Exp
  exp env (VarE v) =
       case lookup v env of
         Nothing -> VarE v
         Just e  -> e
  exp _env (LitE i) = LitE i
  exp env (AppE v e) = AppE v $ go env e
  exp env (PrimAppE p es) = PrimAppE p $ map (go env) es
  exp env (LetE (v,t,e') e) =
       case e' of
         VarE v' -> case lookup v' env of
                         Nothing  -> go ((v,e'):env) e
                         Just e'' -> go ((v,e''):env) e
         et | isTriv et ->
                -- Apply existing renames:
                let et' = go env et in
                go ((v,et'):env) e
         _ -> LetE (v,t,go env e') (go env e)
  exp env (IfE e1 e2 e3) =
       IfE (go env e1) (go env e2) (go env e3)

  -- TODO: Type check here:
  exp env (ProjE i e) = mkProj i $ go env e
  exp env (MkProdE es) = MkProdE $ map (go env) es
  exp env (CaseE e mp) =
       let e' = go env e
           mp' = map (\(c,args,ae) -> (c,args,go env ae)) mp
       in CaseE e' mp'
  exp env (MkPackedE c es) = MkPackedE c $ map (go env) es
  exp env (TimeIt e t b) = TimeIt (go env e) t b
  exp env (MapE (v,t,e') e) = MapE (v,t,go env e') (go env e)
  exp env (FoldE (v1,t1,e1) (v2,t2,e2) e3) =
       FoldE (v1,t1,go env e1) (v2,t2,go env e2) (go env e3)


-- Helpers which do opportunistic reduction:

mkProj :: Int -> Exp -> Exp
mkProj ix (MkProdE ls) = ls !! ix
mkProj ix e = ProjE ix e
