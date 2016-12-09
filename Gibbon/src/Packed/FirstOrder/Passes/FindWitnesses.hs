{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- | 

module Packed.FirstOrder.Passes.FindWitnesses (findWitnesses) where

import Packed.FirstOrder.Common hiding (FunDef)
import Packed.FirstOrder.LTraverse as L2
import qualified Packed.FirstOrder.L1_Source as L1
import Data.List as L hiding (tail)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Graph

bigNumber :: Int
bigNumber = 10 -- limit number of loops

-- | This pass must find witnesses if they exist in the lexical
-- environment, and it must *reorder* let bindings to bring start/end
-- witnesses into scope.
findWitnesses :: L2.Prog -> SyM L2.Prog
findWitnesses = L2.mapMExprs fn
 where
  -- From the point of view of this pass, we "see through" witness markerS:
  view :: Var -> Var
  view v = v  -- RRN: actually, coming up with a good policy her is problematic.
           
  -- view v | isWitnessVar v = let Just v' = fromWitnessVar v in v'
  --        | otherwise      = v
   
  fn _ ex = return (goFix ex bigNumber)
  goFix ex 0 = error $ "timeout in findWitness on " ++ (show ex)
  goFix ex n = let ex1 = go Map.empty ex
                   ex2 = go Map.empty ex1
               in if ex1 == ex2 then ex2 else goFix ex2 (n - 1)
  go mp ex =
    case ex of 
      LetE (v,t,TimeIt e ty b) bod -> 
          handle mp $ LetE (v,t,TimeIt (go Map.empty e) ty b) (go Map.empty bod)

      LetE (v,t,rhs) bod
          -- | isWitnessVar v -> error$ " findWitnesses: internal error, did not expect to see BINDING of witness var: "++show v
          | otherwise -> go (Map.insert v (v,t,rhs') mp) bod -- don't put the bod in the map
            where rhs' = go Map.empty rhs -- recur on rhs

      VarE v         -> handle mp $ VarE v
      LitE n         -> handle mp $ LitE n
      AppE v e       -> handle mp $ AppE v (go Map.empty e)
      PrimAppE p ls  -> handle mp $ PrimAppE p (map (go Map.empty) ls)
      ProjE i e      -> handle mp $ ProjE i (go Map.empty e)
      CaseE e ls     -> handle mp $ CaseE e [ (k,vs,go Map.empty e) | (k,vs,e) <- ls ] 
      MkProdE ls     -> handle mp $ MkProdE (map (go Map.empty) ls)
      MkPackedE k ls -> handle mp $ MkPackedE k (map (go Map.empty) ls)
      TimeIt e t b   -> handle mp $ TimeIt (go Map.empty e) t b -- prevent pushing work into timeit
      IfE a b c      -> handle mp $ IfE a (go Map.empty b) (go Map.empty c)
      MapE (v,t,rhs) bod -> handle mp $ MapE (v,t,rhs) (go Map.empty bod)
      FoldE (v1,t1,r1) (v2,t2,r2) bod -> handle mp $ FoldE (v1,t1,r1) (v2,t2,r2) (go Map.empty bod)

  buildLets _mp [] bod = bod
  buildLets mp (v:vs) bod =
      case Map.lookup (view v) mp of
        Nothing -> buildLets mp vs bod
        Just bnd -> LetE bnd $ buildLets mp vs bod

  -- TODO: this needs to preserve any bindings that have TimeIt forms (hasTimeIt).
  -- OR we can only match a certain pattern like (Let (_,_,TimeIt _ _) _)
  handle mp exp =
      dbgTrace 6 (" [findWitnesses] building lets using vars "++show vs++" for expr: "++ take 80 (show exp)) $      
      buildLets mp vars exp
      where freeInBind v = case Map.lookup (view v) mp of
                             Nothing -> []
                             Just (_v,_t,e) -> withWitnesses $ Set.toList $ L1.freeVars e 
            (g,vf,_) = graphFromEdges $ zip3 vs vs $ map freeInBind vs
            vars = reverse $ map (\(x,_,_) -> x) $ map vf $ topSort g
            vs = Map.keys mp

                 
  withWitnesses ls = concatMap f ls
      where f v = if isWitnessVar v
                  then [v]
                  else [v,toWitnessVar v] -- maybe?
