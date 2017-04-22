-- | Aggressive dead code elimination.  No regard for termination,
-- effects, or partiality.

-- [Populated with duplicated code from InlinePacked]

module Packed.FirstOrder.Passes.ShakeTree
    (shakeTree) where

import qualified Data.Map as M    
import Packed.FirstOrder.Common (SyM, Var, dbgTrace, sdoc)
import qualified Packed.FirstOrder.L1_Source as L1
import Packed.FirstOrder.L2_Traverse as L2
import Packed.FirstOrder.Passes.Cursorize
import Prelude hiding (exp)
import qualified Data.Set as S

-- | Drop all unreferenced let-bindings.    
shakeTree :: L2.Prog -> SyM L2.Prog
shakeTree prg@L2.Prog{fundefs,mainExp} = return $
  prg { fundefs = M.map fd fundefs 
      , mainExp = case mainExp of
                    Nothing      -> Nothing
                    (Just (e,t)) -> Just (shakeTreeExp e, t)
      }
 where
   fd f@FunDef{funbod} = f { funbod = shakeTreeExp funbod }

shakeTreeExp :: L1.Exp -> L1.Exp
shakeTreeExp = go
  where 

  go :: L1.Exp -> L1.Exp
  go e0 =
   -- dbgTrace 5 ("Inline, processing with env:\n "++sdoc env++"\n exp: "++sdoc e0) $
   case e0 of

    (LetE (v,t,rhs) bod) ->
        let bod' = go bod
            fv   = L1.freeVars bod'
        in
        if S.member v fv || hasEffect rhs
        then LetE (v,t, go rhs) bod'
        else dbgTrace 4 (" [shakeTreeExp] dropping binding: "++show (v,t,rhs))$
             bod'

    (VarE v)           -> VarE v 
    (LitE i)           -> LitE i
    (LitSymE v)        -> LitSymE v
    (AppE f e)         -> AppE f $ go e
    (PrimAppE p es)    -> PrimAppE p $ map (go) es
    (IfE e1 e2 e3)     -> IfE (go e1) (go e2) (go e3)
    (ProjE i e)  -> ProjE i $ go e
    (MkProdE es) -> MkProdE $ map (go) es
    -- We don't rename field binders with to/from witness:
    (CaseE e mp) -> let mp' = map dorhs mp
                        dorhs (c,args,ae) =
                            (c,args,go ae)
                    in CaseE (go e) mp'
    (MkPackedE c es) -> MkPackedE c $ map (go) es
    (TimeIt e t b) -> TimeIt (go e) t b
    (MapE (v,t,e') e) -> MapE (v,t,go e') (go e)
    (FoldE (v1,t1,e1) (v2,t2,e2) e3) ->
         FoldE (v1,t1,go e1) (v2,t2,go e2)
               (go e3)


-- | On cursors we have affine types rather than linear.  Thus, unfortunately, we don't
-- have the invariant that the cursors returned by WriteTag are actually USED.  Thus we
-- cannot use dataflow alone to determine what must be kept.
--
-- This contains details that are specific to this pass, which is
-- post-cursorize.  It's not really a good general definition of "hasEffect".
hasEffect :: Exp -> Bool
hasEffect rhs =
    -- Trivials have been inlined, but we're still flat-ish:
    case rhs of
      MkPackedE _ _ -> True
      WriteInt _ _  -> True
      -- Reads are covered by dataflow...
      -- AppE _ _      
      VarE _ -> False
      LitE _ -> False 

      -- These might have effects on output cursors, but the output cursors aren't used
      -- again!  We need to tie the not in dataflow dependencies, making the start (value)
      -- depend on the end (final cursor).
      AppE _ _ -> True  -- For now, don't drop.
      PrimAppE _ _ -> False -- No prims have effects.      
      ProjE _ e    -> hasEffect e      -- Flattening should make this equivalent to "False"
      MkProdE ls   -> any hasEffect ls -- Flattening should make this equivalent to "False"
                        
      IfE a b c -> hasEffect a || hasEffect b || hasEffect c
      CaseE _ _ -> True -- Umm, just don't drop for now. FIXME/ REVISIT THIS!

      TimeIt{} -> True -- Yes, has effect of printing!

      LetE (_,_,e1) e2 -> hasEffect e1 || hasEffect e2
                    
      -- oth -> error $" [shakeTrees] unexpected RHS on Let:\n "++sdoc rhs
               
