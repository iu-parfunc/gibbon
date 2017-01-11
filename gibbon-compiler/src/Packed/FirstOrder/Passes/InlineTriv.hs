
-- | Compiler pass to inline trivials.
module Packed.FirstOrder.Passes.InlineTriv (inlineTriv, inlineTrivExp) where


import Packed.FirstOrder.Common
import Packed.FirstOrder.L1_Source as L1 hiding (mkProj)
-- import Packed.FirstOrder.Passes.Flatten (typeExp, TEnv)
import Prelude hiding (exp)
-- import Debug.Trace

import qualified Packed.FirstOrder.L2_Traverse as L2
import GHC.Stack (errorWithStackTrace)

-- | Inline trivial let bindings (binding a var to a var or int), mainly to clean up
--   the output of `flatten`.
inlineTriv :: Prog -> Prog
inlineTriv (Prog ddefs funs main) =
    Prog ddefs (fmap inlineTrivFun funs) (fmap (inlineTrivExp ddefs) main)
  where
    inlineTrivFun (FunDef nam (narg,targ) ty bod) =
      FunDef nam (narg,targ) ty (inlineTrivExp ddefs bod)

type Env = [(Var, (Ty,Exp))]
             
inlineTrivExp :: DDefs a -> Exp -> Exp
inlineTrivExp _ddefs = go []
  where

  -- Just a hook for debugging:
  go :: Env -> Exp -> Exp
  go env e  =
      -- dbgTrace 7 ("Inline, processing with env:\n "++sdoc env++"\n exp: "++sdoc e) $
      exp env e          

  -- | Hree we go to some lengths to maintain the syntactic invariants
  -- for the extended L2 forms. The idea is that we can only reference
  -- variables within these forms, but we still must apply the
  -- environment because the old bindings have been removed.
  -- 
  -- An alternative would be to let the extended forms disappear at
  -- this point, and handle them at the level of "AppE" in Lower.hs.
  withVar :: Env -> Var -> (Var -> Exp) -> Exp
  withVar env v fn =
    case lookup v env of
      Nothing        -> fn v
      Just (_,VarE v2) -> fn v2
      -- fixme, need gensym:
      Just (ty,oth)  -> LetE (v,ty,oth) $ fn v
          
  exp :: Env -> Exp -> Exp
  exp env (VarE v) =
       case lookup v env of
         Nothing -> VarE v
         Just (_,e) -> e
  exp _env (LitE i) = LitE i

  -- Because this pass is applied on both L1 and L2...
  exp _   L2.NewBuffer      = L2.NewBuffer
  exp _   L2.ScopedBuffer   = L2.ScopedBuffer
  exp env (L2.ReadInt v)     = withVar env v $ L2.ReadInt 
  exp env (L2.WriteInt v e)  = withVar env v $ \v2 -> L2.WriteInt v2 (go env e)
  exp env (L2.AddCursor v i) = withVar env v $ \v2 -> L2.AddCursor v2 i
  exp env p | L2.isExtendedPattern p =
    errorWithStackTrace $ "InlineTriv: failed to handle extended L2 form: "
                          ++ndoc p++", env: "++ndoc env

  exp env (AppE v e) = AppE v $ go env e
  exp env (PrimAppE p es) = PrimAppE p $ map (go env) es
  exp env (LetE (v,t,e') e) =
       case e' of
         VarE v' -> case lookup v' env of
                         Nothing -> go ((v,(t,e')):env) e
                         Just pr -> go ((v,pr):env) e
         et | isTriv et ->
                -- Apply existing renames:
                let et' = go env et in
                go ((v,(t,et')):env) e
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
