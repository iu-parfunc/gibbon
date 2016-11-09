-- | Aggressive dead code elimination.  No regard for terminator,
-- effects, or partiality.

-- [Populated with duplicated code from InlinePacked]

module Packed.FirstOrder.Passes.ShakeTree
    (shakeTree) where

import qualified Data.Map as M    
import Packed.FirstOrder.Common (SyM, Var, dbgTrace, sdoc)
import qualified Packed.FirstOrder.L1_Source as L1
import Packed.FirstOrder.LTraverse as L2
import Prelude hiding (exp)

-- | Drop all unreferenced let-bindings.    
shakeTree :: L2.Prog -> SyM L2.Prog
shakeTree prg@L2.Prog{fundefs,mainExp} = return $
  prg { fundefs = M.map fd fundefs 
      , mainExp = case mainExp of
                    Nothing      -> Nothing
                    (Just (e,t)) -> Just (shakeTreeExp e, t)
      }
 where
   fd f@FunDef{funarg, funbod} =
       f { funbod = shakeTreeExp funbod }

shakeTreeExp :: L1.Exp -> L1.Exp
shakeTreeExp = go
  where    
  go :: L1.Exp -> L1.Exp
  go e0 =
   -- dbgTrace 5 ("Inline, processing with env:\n "++sdoc env++"\n exp: "++sdoc e0) $
   case e0 of
    (VarE v)           -> VarE v 
    (LitE i)           -> LitE i
    (AppE f e)         -> AppE f $ go e
    (PrimAppE p es)    -> PrimAppE p $ map (go) es
    (LetE (v,t,rhs) e) -> LetE (v,t, go rhs) (go e)
    (IfE e1 e2 e3)     -> IfE (go e1) (go e2) (go e3)
    (ProjE i e)  -> ProjE i $ go e
    (MkProdE es) -> MkProdE $ map (go) es
    -- We don't rename field binders with to/from witness:
    (CaseE e mp) -> let mp' = map dorhs mp
                        dorhs (c,args,ae) =
                            (c,args,go ae)
                    in CaseE (go e) mp'
    (MkPackedE c es) -> MkPackedE c $ map (go) es
    (TimeIt e t) -> TimeIt (go e) t
    (MapE (v,t,e') e) -> MapE (v,t,go e') (go e)
    (FoldE (v1,t1,e1) (v2,t2,e2) e3) ->
         FoldE (v1,t1,go e1) (v2,t2,go e2)
               (go e3)

