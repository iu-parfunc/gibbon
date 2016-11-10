
-- | Compiler pass to inline trivials.
module Packed.FirstOrder.Passes.InlineTriv (inlineTriv, inlineTrivExp) where
    
import Packed.FirstOrder.Common
import Packed.FirstOrder.L1_Source as L1 hiding (mkProj)
import Prelude hiding (exp)
-- import Debug.Trace

-- | Inline trivial let bindings (binding a var to a var or int), mainly to clean up
--   the output of `flatten`.
inlineTriv :: L1.Prog -> L1.Prog
inlineTriv (L1.Prog defs funs main) =
    L1.Prog defs (fmap inlineTrivFun funs) (fmap inlineTrivExp main)
  where
    inlineTrivFun (FunDef nam (narg,targ) ty bod) =
      FunDef nam (narg,targ) ty (inlineTrivExp bod)

inlineTrivExp :: L1.Exp -> L1.Exp
inlineTrivExp = go []
  where

  -- Just a hook for debugging:
  go :: [(Var,L1.Exp)] -> L1.Exp -> L1.Exp
  go env e  =
   -- trace ("Inline, processing with env:\n "++sdoc env++"\n exp: "++sdoc e) $
      exp env e          
      
  exp :: [(Var,L1.Exp)] -> L1.Exp -> L1.Exp
  exp env (L1.VarE v) =
       case lookup v env of
         Nothing -> L1.VarE v
         Just e  -> e
  exp _env (L1.LitE i) = L1.LitE i
  exp env (L1.AppE v e) = L1.AppE v $ go env e
  exp env (L1.PrimAppE p es) = L1.PrimAppE p $ map (go env) es
  exp env (L1.LetE (v,t,e') e) =
       case e' of
         L1.VarE v' -> case lookup v' env of
                         Nothing  -> go ((v,e'):env) e
                         Just e'' -> go ((v,e''):env) e
         et | isTriv et ->
                -- Apply existing renames:
                let et' = go env et in
                go ((v,et'):env) e
         _ -> L1.LetE (v,t,go env e') (go env e)
  exp env (L1.IfE e1 e2 e3) =
       L1.IfE (go env e1) (go env e2) (go env e3)
  exp env (L1.ProjE i e) = mkProj i $ go env e
  exp env (L1.MkProdE es) = L1.MkProdE $ map (go env) es
  exp env (L1.CaseE e mp) =
       let e' = go env e
           mp' = map (\(c,args,ae) -> (c,args,go env ae)) mp
       in L1.CaseE e' mp'
  exp env (L1.MkPackedE c es) = L1.MkPackedE c $ map (go env) es
  exp env (L1.TimeIt e t b) = L1.TimeIt (go env e) t b
  exp env (L1.MapE (v,t,e') e) = L1.MapE (v,t,go env e') (go env e)
  exp env (L1.FoldE (v1,t1,e1) (v2,t2,e2) e3) =
       L1.FoldE (v1,t1,go env e1) (v2,t2,go env e2) (go env e3)


-- Helpers which do opportunistic reduction:

mkProj :: Int -> Exp -> Exp
mkProj ix (MkProdE ls) = ls !! ix
mkProj ix (VarE v) = ProjE ix (VarE v)
