{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- | 

module Packed.FirstOrder.Passes.FindWitnesses (findWitnesses) where

import Packed.FirstOrder.Common hiding (FunDef)
import Packed.FirstOrder.LTraverse as L2
import qualified Packed.FirstOrder.L1_Source as L1
import Data.List as L hiding (tail)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | This pass must find witnesses if they exist in the lexical
-- environment, and it must *reorder* let bindings to bring start/end
-- witnesses into scope.
findWitnesses :: L2.Prog -> SyM L2.Prog
findWitnesses = L2.mapMExprs fn
 where
  fn _ ex = return (go Map.empty ex)
  go mp ex =
    case ex of 
      VarE v   -> buildLets mp [v] $ VarE v
      LitE n   -> LitE n
      AppE v e -> handle mp $ AppE v (go Map.empty e)
      PrimAppE p ls      -> handle mp $ PrimAppE p (map (go Map.empty) ls)
      LetE (v,t,rhs) bod -> go (Map.insert v (v,t,rhs) mp) bod
      ProjE i e      -> handle mp $ ProjE i (go Map.empty e)
      CaseE e ls     -> handle mp $ CaseE e [ (k,vs,go Map.empty e) | (k,vs,e) <- ls ] 
      MkProdE ls     -> handle mp $ MkProdE (map (go Map.empty) ls)
      MkPackedE k ls -> handle mp $ MkPackedE k (map (go Map.empty) ls)
      TimeIt e t     -> TimeIt (go mp e) t
      IfE a b c      -> handle mp $ IfE a (go Map.empty b) (go Map.empty c)
      MapE (v,t,rhs) bod -> handle mp $ MapE (v,t,rhs) (go Map.empty bod)
      FoldE (v1,t1,r1) (v2,t2,r2) bod -> handle mp $ FoldE (v1,t1,r1) (v2,t2,r2) (go Map.empty bod)

  buildLets _mp [] bod = bod
  buildLets mp (v:vs) bod =
      case Map.lookup v mp of
        Nothing -> buildLets mp vs bod
        Just bnd -> LetE bnd $ buildLets mp vs bod

  handle mp exp = buildLets mp (vars fvs []) exp
      where fvs = Set.toList $ L1.freeVars exp
            freeInBind v = case Map.lookup v mp of
                             Nothing -> []
                             Just (_v,_t,e) -> Set.toList $ L1.freeVars e
            vars [] found = found
            vars (w:ws) found =
                let nw = freeInBind w
                in vars ((nw \\ ws) ++ ws) (w:(found \\ nw))
