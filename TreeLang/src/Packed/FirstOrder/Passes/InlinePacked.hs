-- | Aggressive inliner to put Packed-typed expressions syntactically
-- under constructors that they flow to.

-- WARNING: DUPLICATED code from InlineTriv.hs

module Packed.FirstOrder.Passes.InlinePacked
    (inlinePacked) where

import qualified Data.Map as M    
import Packed.FirstOrder.Common (SyM, Var, dbgTrace)
import qualified Packed.FirstOrder.L1_Source as L1
import Packed.FirstOrder.LTraverse as L2
import Prelude hiding (exp)

-- | This pass gets ready for cursorDirect by pushing tree-creating
-- expressions within the syntactic scope of data constructor
-- applications.
inlinePacked :: L2.Prog -> SyM L2.Prog
inlinePacked prg@L2.Prog{fundefs,mainExp} = return $
  prg { fundefs = M.map fd fundefs 
      , mainExp = case mainExp of
                    Nothing      -> Nothing
                    (Just (e,t)) -> Just (inlinePackedExp [] e, t)
      }
 where
   fd f@FunDef{funarg, funbod} =
       f { funbod = inlinePackedExp [(funarg,Nothing)] funbod }

-- | Keep a map of the entire lexical environment, but only part of it
-- is inlinable. (I.e. function arguments are not.)
inlinePackedExp :: [(Var,Maybe L1.Exp)] -> L1.Exp -> L1.Exp
inlinePackedExp = go
  where
  go :: [(Var,Maybe L1.Exp)] -> L1.Exp -> L1.Exp
  go env e0 =
   -- dbgTrace 5 ("Inline, processing with env:\n "++sdoc env++"\n exp: "++sdoc e0) $
   case e0 of      
    (VarE v) -> case lookup v env of
                  Nothing -> dbgTrace 1 ("WARNING [inlinePacked] unbound variable: "++v)$
                             VarE v
                  Just (Just e) -> e
                  Just Nothing  -> VarE v -- Bound, but non-inlinable binding.
    (LitE i)    -> LitE i
    (AppE v e)  -> AppE v $ go env e
    (PrimAppE p es) -> PrimAppE p $ map (go env) es
    (LetE (v,t,rhs) e)
       | L1.hasPacked t -> let rhs' = go env rhs in 
                           go ((v,Just rhs'):env) e
       | otherwise -> LetE (v,t, go env rhs)
                           (go ((v,Nothing):env) e)

    ------ boilerplate -------                      
    (IfE e1 e2 e3) ->
         IfE (go env e1) (go env e2) (go env e3)
    (ProjE i e) -> ProjE i $ go env e
    (MkProdE es) -> MkProdE $ map (go env) es
    (CaseE e mp) -> let mp' = map dorhs mp
                        dorhs (c,args,ae) =
                            let env' = [(v,Nothing) | v <- args] ++ env in
                            (c,args,go env' ae)
                    in CaseE (go env e) mp'
    (MkPackedE c es) -> MkPackedE c $ map (go env) es
    (TimeIt e t) -> TimeIt (go env e) t
    (MapE (v,t,e') e) -> let env' = (v,Nothing) : env in
                         MapE (v,t,go env e') (go env' e)
    (FoldE (v1,t1,e1) (v2,t2,e2) e3) ->
         let env' = (v1,Nothing) : (v2,Nothing) : env in
         FoldE (v1,t1,go env e1) (v2,t2,go env e2)
               (go env' e3)
