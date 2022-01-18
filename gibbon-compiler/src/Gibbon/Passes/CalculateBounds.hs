{-# LANGUAGE FlexibleInstances #-}
module Gibbon.Passes.CalculateBounds where

import           Gibbon.Common
import qualified Data.Map                      as M
import           Gibbon.L2.Syntax
import           Debug.Trace
import Data.List (unzip4)

type LocationMapping = M.Map LocVar Region
type VarSizeMapping = M.Map Var RegionSize
type RegionSizeMapping = M.Map Region RegionSize

calculateBounds :: Prog2 -> PassM Prog2
calculateBounds Prog { ddefs, fundefs, mainExp } = do
  let env2 = Env2 M.empty (initFunEnv fundefs)
  fundefs' <- mapM (calculateBoundsFun ddefs env2 M.empty) fundefs
  mainExp' <- case mainExp of
    Nothing       -> return Nothing
    Just (mn, ty) -> Just . (, ty) . fst4 <$> calculateBoundsExp2 ddefs env2 M.empty M.empty M.empty mn
  return $ Prog ddefs fundefs' mainExp'


calculateBoundsFun :: DDefs Ty2 -> Env2 Ty2 -> VarSizeMapping -> FunDef2 -> PassM FunDef2
calculateBoundsFun ddefs env2 szEnv f@FunDef { funBody, funTy } = do
  let locEnv = M.fromList $ map (\lv -> (lrmLoc lv, lrmReg lv)) (locVars funTy)
  funBody' <- fst4 <$> calculateBoundsExp2 ddefs env2 szEnv locEnv M.empty funBody
  return $ f { funBody = funBody' }

calculateBoundsExp2 :: DDefs Ty2 -> Env2 Ty2 -> VarSizeMapping -> LocationMapping -> RegionSizeMapping -> Exp2 -> PassM (Exp2, RegionSize, LocationMapping, RegionSizeMapping)
calculateBoundsExp2 ddefs env2 szEnv locEnv regEnv ex = do
  res <- calculateBoundsExp ddefs env2 szEnv locEnv regEnv ex
  traceM $ "res = " ++ sdoc res
  return res


calculateBoundsExp
  :: DDefs Ty2 -- ^ Data Definitions
  -> Env2 Ty2 -- ^ Type Environment (Variables + Functions)
  -> VarSizeMapping -- ^ var => region
  -> LocationMapping -- ^ location => region
  -> RegionSizeMapping -- ^ region => size 
  -> Exp2 -- ^ expression 
  -> PassM (Exp2, RegionSize, LocationMapping, RegionSizeMapping)
  -- TODO do we really need to return regionsize mapiing or location mapping?
-- TODO: track allocations to locations and update the bound for region
-- TODO: know which regions are input (0 allocations)
calculateBoundsExp ddefs env2 szEnv locEnv regEnv ex = case ex of
  Ext  (BoundsCheck{} ) -> return (ex, Unbounded, locEnv, regEnv)
  Ext  (IndirectionE{}) -> return (ex, BoundedSize 8, locEnv, regEnv)
  VarE v                -> case M.lookup v szEnv of
    Just w -> return (ex, w, locEnv, regEnv)
    _      -> do
      traceM $ "No size in env for " ++ sdoc v ++ " (" ++ sdoc szEnv ++ ")"
      return (ex, Undefined, locEnv, regEnv)
  _ ->
    let ty   = gRecoverType ddefs env2 ex
        go   = calculateBoundsExp2 ddefs env2 szEnv locEnv regEnv
        err  = error "Should have been covered by sizeOfTy"
        todo = return (ex, Undefined, locEnv, regEnv)
    in  case sizeOfTy ty of
          Just v -> return (ex, BoundedSize v, locEnv, regEnv)
          _      -> case ex of
            LitE    _        -> err
            FloatE  _        -> err
            LitSymE _        -> err
            ProjE{}          -> todo
            TimeIt{}         -> todo
            WithArenaE{}     -> todo
            SpawnE{}         -> todo
            SyncE{}          -> todo
            MapE{}           -> todo
            FoldE{}          -> todo
            -- TODO use input/output loc 
            AppE v locs args -> do
              traceM $ "env2 = " ++ sdoc env2
              traceM $ "v = " ++ sdoc env2
              traceM $ "func = " ++ sdoc (M.lookup v (fEnv env2))
              return (ex, Unbounded, locEnv, regEnv)
            PrimAppE{}             -> return (ex, Unbounded, locEnv, regEnv)
            DataConE loc dcon args -> do
              (_, szs, les, res) <- unzip4 <$> mapM go args
              return (DataConE loc dcon args, 1 + sum szs, mconcat les, mconcat res)
            IfE cond bod1 bod2 -> do
              (bod1', sz1, le1, regEnv1) <- go bod1
              (bod2', sz2, le2, regEnv2) <- go bod2
              let sz' = max sz1 sz2
              return (IfE cond bod1' bod2', sz', le1 <> le2, regEnv1 <> regEnv2)
            MkProdE ls -> do
              (ls', szs', les, regEnvs) <- unzip4 <$> mapM go ls
              let sz = sum szs'
              return (MkProdE ls', sz, mconcat les, mconcat regEnvs)
            LetE (v, locs, ty0, bind) bod -> do
              (bind', sz1, le1, regEnv') <- go bind
              let venv'  = M.insert v ty0 (vEnv env2)
                  szEnv' = M.insert v sz1 szEnv
              (bod', sz, le2, regEnv'') <- calculateBoundsExp2 ddefs env2 { vEnv = venv' } szEnv' locEnv regEnv bod
              return (LetE (v, locs, ty0, bind') bod', sz, le1 <> le2, regEnv' <> regEnv'')
            CaseE ex2 cases -> do
              (cases', sizes, les, res) <-
                unzip4
                  <$> mapM
                        (\(dcon, vlocs, bod) -> do
                          -- (dcon', sz1) <- go dcon 
                          -- TODO insert datacon argument sizes and locations for case arguments
                          -- let szEnv' = M.insert dcon sz1 
                          -- let locEnv' = M.union locEnv $ M.fromList $ map (\(v, l)) vlocs
                          (bod', sz2, le, re) <- calculateBoundsExp2 ddefs env2 szEnv locEnv regEnv bod
                          return ((dcon, vlocs, bod'), sz2, le, re)
                        )
                        cases
              let sz' = maximum sizes
              return (CaseE ex2 cases', sz', mconcat les, mconcat res)
            Ext ext -> case ext of
              LetRegionE reg bod -> do
                (bod', sz, le, re) <- go bod
                return (Ext $ LetRegionE (AnalyzedRegion reg sz) bod', sz, le, re)
              LetParRegionE{}        -> todo
              LetLocE loc locExp ex1 -> do
                traceM $ "locEnv = " ++ sdoc locEnv
                traceM $ "locExp = " ++ sdoc locExp
                let le' = M.insert loc (getRegionSize locEnv szEnv locExp) locEnv
                (ex1', sz, le'', re) <- calculateBoundsExp2 ddefs env2 szEnv le' regEnv ex1
                return (Ext $ LetLocE loc locExp ex1', sz, le'', re)
              -- TODO use locs
              RetE locs v -> do
                traceM $ "ret.locs = " ++ sdoc locs
                (_, sz, le, re) <- go (VarE v)
                return (ex, sz, le, re)
              FromEndE{}         -> todo
              AddFixed{}         -> todo
              GetCilkWorkerNum{} -> todo
              LetAvail{}         -> todo

getRegionSize :: LocationMapping ->VarSizeMapping ->   PreLocExp LocVar -> (Region, RegionSize)
getRegionSize _  ve (StartOfLE r          ) = (r, 0)
getRegionSize le ve (AfterConstantLE n l  ) = (le M.! l, BoundedSize n)
getRegionSize le ve (AfterVariableLE v l _) = (le M.! l, ve M.! v )
getRegionSize _  ve (InRegionLE r         ) = (r, Undefined )
getRegionSize le ve (FromEndLE  l         ) = (le M.! l, Unbounded )
getRegionSize _  ve FreeLE                  = error "Not bounded to any region"


