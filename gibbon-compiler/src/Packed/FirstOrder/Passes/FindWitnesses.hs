{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

 -- |

module Packed.FirstOrder.Passes.FindWitnesses
  (findWitnesses) where


import Data.Loc
import Data.Graph
import qualified Data.Map as Map
import qualified Data.Set as Set
-- import Data.List as L hiding (tail)

import Packed.FirstOrder.GenericOps
import Packed.FirstOrder.Common hiding (FunDef)
import Packed.FirstOrder.L1.Syntax hiding (FunDef, FunDefs, Prog)
import Packed.FirstOrder.L3.Syntax
-- import Packed.FirstOrder.L2.Syntax as L2


bigNumber :: Int
bigNumber = 10 -- limit number of loops

-- | This pass must find witnesses if they exist in the lexical
-- environment, and it must *reorder* let bindings to bring start/end
-- witnesses into scope.
--
-- Phase Ordering: This must run after flatten.
findWitnesses :: Prog -> SyM Prog
findWitnesses = mapMExprs fn
 where
  fn Env2{vEnv,fEnv} ex = return (goFix (Map.keysSet vEnv `Set.union` Map.keysSet fEnv)
                                        ex bigNumber)
  goFix _    ex 0 = error $ "timeout in findWitness on " ++ (show ex)
  goFix bound0 ex0 n = let ex1 = goE bound0 Map.empty ex0
                           ex2 = goE bound0 Map.empty ex1
                       in if ex1 == ex2 then ex2
                          else goFix bound0 ex2 (n - 1)
  goE bound mp (L p ex) =
    let go      = goE bound -- Shorthand.
        goClear = goE (bound `Set.union` Map.keysSet mp) Map.empty
        -- shorthand for applying (L p)
        handle' e = handle mp $ L p e
    in
      case ex of
        LetE (v,locs,t, L p2 (TimeIt e ty b)) bod ->
            handle' $ LetE (v,locs,t, L p2 $ TimeIt (go Map.empty e) ty b)
                      (goE (Set.insert v (bound `Set.union` Map.keysSet mp)) Map.empty bod)

        LetE (v,locs,t,rhs) bod
            -- | isWitnessVar v -> error$ " findWitnesses: internal error, did not expect to see BINDING of witness var: "++show v
            | otherwise -> go (Map.insert v (v,locs,t,rhs') mp) bod -- don't put the bod in the map
              where rhs' = go Map.empty rhs -- recur on rhs, but flatten makes these pretty boring.

        VarE v         -> handle' $ VarE v
        LitE n         -> handle' $ LitE n
        LitSymE v      -> handle' $ LitSymE v
        AppE v locs e  -> handle' $ AppE v locs (goClear e)
        PrimAppE p ls  -> handle' $ PrimAppE p (map goClear ls)
        ProjE i e      -> handle' $ ProjE i (goClear e)

        -- It's ok that we don't go deeper on scrutinees, subexpressions
        -- of tuples, and so on, because flatten should have done
        -- let-lifting out of these contexts:
        MkProdE ls     -> handle' $ MkProdE (map goClear ls)
        DataConE loc k ls -> handle' $ DataConE loc k (map goClear ls)
        TimeIt e t b   -> handle' $ TimeIt (goClear e) t b -- prevent pushing work into timeit

        -- FIXME: give CaseE a treatment like IfE:
        CaseE e ls     -> handle' $ CaseE e [ (k,vs,goClear e) | (k,vs,e) <- ls ]

        IfE a b c ->
            -- If we have "succeeded" in accumulating all the bindings
            -- we need, then we can discharge their topological sort
            -- here, without duplicating bindings:
            if   closed bound mp
            then handle' $ IfE a (goClear b) (goClear c)
            else (L p) $ IfE (go mp a) -- Otherwise we duplicate...
                             (go mp b)
                             (go mp c)
        MapE  (v,t,rhs) bod -> handle' $ MapE (v,t,rhs) (goClear bod)
        FoldE (v1,t1,r1) (v2,t2,r2) bod -> handle' $ FoldE (v1,t1,r1) (v2,t2,r2) (goClear bod)

        Ext _ -> handle' $ ex

-- TODO: this needs to preserve any bindings that have TimeIt forms (hasTimeIt).
-- OR we can only match a certain pattern like (Let (_,_,TimeIt _ _) _)
handle :: Map.Map Var (Var, [()], Ty3, L Exp3) -> L Exp3 -> L Exp3
handle mp exp =
    dbgTrace 6 (" [findWitnesses] building lets using vars "++show vs++" for expr: "++ take 80 (show exp)) $
    buildLets mp vars exp
    where freeInBind v = case Map.lookup (view v) mp of
                           Nothing -> []
                           Just (_v,_locs,_t,e) -> Set.toList $ gFreeVars e
          (g,vf,_) = graphFromEdges $ zip3 vs vs $ map freeInBind vs
          vars = reverse $ map (\(x,_,_) -> x) $ map vf $ topSort g
          vs = Map.keys mp


-- withWitnesses :: [LocVar] -> [LocVar]
-- withWitnesses ls = concatMap f ls
--     where f v = if isWitnessVar v
--                 then [v]
--                 else [v,toWitnessVar v] -- maybe?

-- From the point of view of this pass, we "see through" witness markerS:
view :: Var -> Var
view v = v  -- RRN: actually, coming up with a good policy here is problematic.

-- view v | isWitnessVar v = let Just v' = fromWitnessVar v in v'
--        | otherwise      = v


buildLets :: Map.Map Var (Var,[()], Ty3, L Exp3) -> [Var] -> L Exp3-> L Exp3
buildLets _mp [] bod = bod
buildLets mp (v:vs) bod =
    case Map.lookup (view v) mp of
      Nothing -> buildLets mp vs bod
      Just bnd -> l$ LetE bnd $ buildLets mp vs bod


-- | Are all the free variables currently bound (transitively) in the
-- environment?
closed :: Set.Set Var -> Map.Map Var (v, [()], t, L Exp3) -> Bool
closed bound mp = Set.null (allBound `Set.difference` allUsed)
  where
   allBound = bound `Set.union` Map.keysSet mp
   allUsed = Set.unions [ gFreeVars rhs | (_,_,_,rhs) <- Map.elems mp ]
