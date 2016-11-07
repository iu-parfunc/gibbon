{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- | 

module Packed.FirstOrder.Passes.FindWitnesses 
    ( findWitnesses) where

import           Packed.FirstOrder.Common hiding (FunDef)
import           Packed.FirstOrder.LTraverse as L2
import Data.List as L hiding (tail)


-- | This pass must find witnesses if they exist in the lexical
-- environment, and it must *reorder* let bindings to bring start/end
-- witnesses into scope.
findWitnesses :: L2.Prog -> SyM L2.Prog
findWitnesses = L2.mapMExprs fn
 where
  fn _ ex = return (go ex)
  go ex =
    case ex of 
      VarE v   -> VarE v
      LitE n   -> LitE n
      AppE v e -> AppE v (go e)
      PrimAppE p ls      -> PrimAppE p (L.map go ls)
      LetE (v,t,rhs) bod -> LetE (v,t,go rhs) (go bod)
      ProjE i e      -> ProjE i (go e)
      CaseE e ls     -> CaseE (go e) [ (k,vs,go e) | (k,vs,e) <- ls ]
      MkProdE ls     -> MkProdE (L.map go ls)
      MkPackedE k ls -> MkPackedE k (L.map go ls)
      TimeIt e t     -> TimeIt (go e) t
      IfE a b c      -> IfE (go a) (go b) (go c)
      MapE (v,t,rhs) bod -> MapE (v,t,go rhs) (go bod)
      FoldE (v1,t1,r1) (v2,t2,r2) bod -> FoldE (v1,t1,go r1) (v2,t2,go r2) (go bod)

