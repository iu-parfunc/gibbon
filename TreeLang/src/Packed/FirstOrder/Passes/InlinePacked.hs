-- | Aggressive inliner to put Packed-typed expressions syntactically
-- under constructors that they flow to.

-- WARNING: DUPLICATED code from InlineTriv.hs

module Packed.FirstOrder.Passes.InlinePacked
    (inlinePacked) where
    
import Packed.FirstOrder.Common
import qualified Packed.FirstOrder.L1_Source as L1
import Packed.FirstOrder.LTraverse as L2
import Prelude hiding (exp)
-- import Debug.Trace

-- | This pass gets ready for cursorDirect by pushing tree-creating
-- expressions within the syntactic scope of data constructor
-- applications.
inlinePacked :: L2.Prog -> SyM L2.Prog
inlinePacked prg = L2.mapMExprs (inlinePackedExp) prg

inlinePackedExp :: Env2 L1.Ty -> L1.Exp -> SyM L1.Exp
inlinePackedExp _env2 = return . go []
  where

  -- Just a hook for debugging:
  go :: [(Var,L1.Exp)] -> L1.Exp -> L1.Exp
  go env e0 =
   -- trace ("Inline, processing with env:\n "++sdoc env++"\n exp: "++sdoc e) $
   case e0 of      
    (VarE v) -> case lookup v env of
                  Nothing -> dbgTrace 1 ("WARNING [inlinePacked] unbound variable: "++v)$
                             VarE v
                  Just e  -> e         
    (LitE i)    -> LitE i
    (AppE v e)  -> AppE v $ go env e
    (PrimAppE p es) -> PrimAppE p $ map (go env) es
    (LetE (v,t,rhs) e)
       | L1.hasPacked t -> let rhs' = go env rhs in 
                           go ((v,rhs'):env) e
       | otherwise -> LetE (v,t,go env rhs) (go env e)

    ------ boilerplate -------                      
    (IfE e1 e2 e3) ->
         IfE (go env e1) (go env e2) (go env e3)
    (ProjE i e) -> ProjE i $ go env e
    (MkProdE es) -> MkProdE $ map (go env) es
    (CaseE e mp) -> let mp' = map (\(c,args,ae) -> (c,args,go env ae)) mp
                    in CaseE (go env e) mp'
    (MkPackedE c es) -> MkPackedE c $ map (go env) es
    (TimeIt e t) -> TimeIt (go env e) t
    (MapE (v,t,e') e) -> MapE (v,t,go env e') (go env e)
    (FoldE (v1,t1,e1) (v2,t2,e2) e3) ->
         FoldE (v1,t1,go env e1) (v2,t2,go env e2) (go env e3)
