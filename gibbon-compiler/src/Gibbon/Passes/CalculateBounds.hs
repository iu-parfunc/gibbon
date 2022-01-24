{-# LANGUAGE FlexibleInstances #-}
module Gibbon.Passes.CalculateBounds where

import           Gibbon.Common
import qualified Data.Map                      as M
import           Gibbon.L2.Syntax
import           Debug.Trace

type LocationMapping = M.Map LocVar Region
type VarSizeMapping = M.Map Var RegionSize
type RegionSizeMapping = M.Map Region RegionSize
type RegionTypeMapping = M.Map Region Modality

calculateBounds :: Prog2 -> PassM Prog2
calculateBounds Prog { ddefs, fundefs, mainExp } = do
  let env2 = Env2 M.empty (initFunEnv fundefs)
  fundefs' <- mapM (calculateBoundsFun ddefs env2 M.empty) fundefs
  mainExp' <- case mainExp of
    Nothing       -> return Nothing
    Just (mn, ty) -> Just . (, ty) . fst <$> calculateBoundsExp2 ddefs env2 M.empty M.empty M.empty mn
  return $ Prog ddefs fundefs' mainExp'


calculateBoundsFun :: DDefs Ty2 -> Env2 Ty2 -> VarSizeMapping -> FunDef2 -> PassM FunDef2
calculateBoundsFun ddefs env2 szEnv f@FunDef { funBody, funTy, funArgs } = do
  let locEnv = M.fromList $ map (\lv -> (lrmLoc lv, lrmReg lv)) (locVars funTy)
  let argTys = M.fromList $ zip funArgs (arrIns funTy)
  let env2'  = env2 { vEnv = argTys }
  funBody' <- fst <$> calculateBoundsExp2 ddefs env2' szEnv locEnv M.empty funBody
  return $ f { funBody = funBody' }

calculateBoundsExp2
  :: DDefs Ty2
  -> Env2 Ty2
  -> VarSizeMapping
  -> LocationMapping
  -> RegionSizeMapping
  -> Exp2
  -> PassM (Exp2, RegionSizeMapping)
calculateBoundsExp2 ddefs env2 szEnv locEnv regEnv ex = do
  res <- calculateBoundsExp ddefs env2 szEnv locEnv regEnv ex
  traceM $ "res = " ++ sdoc res
  return res

{- 
  * We recurse using three mappings (variable => size, location => region and region => size)
  * 1. the variable => size mapping is inferred using type of the variable
  * 2. the location => region mapping is inferred using threading location initializations 
  *     using LocVars
  * 3. the region => size mapping is calculated by adding up the incremental offsets from 
  *     previous location in the same reigon and then adding the size of the last location/variable 
  *     size in the region
  * 
  * While recursing we return the 
  * 1. the updated expression (i.e. possibly attaching the size of the regions in the expression)
  * 2. the region to size mapping in the expression (which can be used to find maximum size 
  *     from all the branches)
  * 
  * NOTE: since input and output regions are not created inside the function, 
  * we will not update the region size inside that function .
-}
calculateBoundsExp
  :: DDefs Ty2 -- ^ Data Definitions
  -> Env2 Ty2 -- ^ Type Environment (Variables + Functions)
  -> VarSizeMapping -- ^ var => size
  -> LocationMapping -- ^ location => region
  -> RegionSizeMapping -- ^ region => size 
  -> Exp2 -- ^ expression 
  -> PassM (Exp2, RegionSizeMapping)
  -- TODO do we really need to return regionsize mapiing or location mapping?
-- TODO: track allocations to locations and update the bound for region
-- TODO: know which regions are input (0 allocations)
calculateBoundsExp ddefs env2 szEnv locEnv regEnv ex = case ex of
  Ext  (BoundsCheck{}                ) -> return (ex, regEnv)
  Ext  (IndirectionE _ _ (loc, _) _ _) -> return (ex, M.insertWith (+) (locEnv M.! loc) 9 regEnv)
  VarE v                               -> case M.lookup v szEnv of
    Just _ -> return (ex, regEnv)
    _      -> do
      traceM $ "No size in env for " ++ sdoc v ++ " (" ++ sdoc szEnv ++ ")"
      return (ex, regEnv)
  _ ->
    let ty   = gRecoverType ddefs env2 ex
        go   = calculateBoundsExp2 ddefs env2 szEnv locEnv regEnv
        err  = error "Should have been covered by sizeOfTy"
        todo = return (ex, regEnv)
    in  case sizeOfTy ty of
          Just _ -> return (ex, regEnv)
          _      -> case ex of
            LitE    _          -> err
            FloatE  _          -> err
            LitSymE _          -> err
            ProjE{}            -> todo
            TimeIt{}           -> todo
            WithArenaE{}       -> todo
            SpawnE{}           -> todo
            SyncE{}            -> todo
            MapE{}             -> todo
            FoldE{}            -> todo
            -- TODO use input/output loc 
            AppE v _locs _args -> do
              traceM $ "env2 = " ++ sdoc env2
              traceM $ "v = " ++ sdoc env2
              traceM $ "func = " ++ sdoc (M.lookup v (fEnv env2))
              -- TODO locs are input region for 'v'. No change done by v. Pick up from regEnv?
              return (ex, regEnv)
              -- TODO no change to regions by primapp so pick up from regEnv?
            PrimAppE{}             -> return (ex, regEnv)
            DataConE loc dcon args -> do
              (_, res) <- unzip <$> mapM go args
              return (DataConE loc dcon args, mconcat res)
            IfE cond bod1 bod2 -> do
              (bod1', regEnv1) <- go bod1
              (bod2', regEnv2) <- go bod2
              return (IfE cond bod1' bod2', M.unionWith max regEnv1 regEnv2)
            MkProdE ls -> do
              (ls', regEnvs) <- unzip <$> mapM go ls
              return (MkProdE ls', M.unionsWith max regEnvs)
            LetE (v, locs, ty0, bind) bod -> do
              (bind', regEnv') <- go bind
              let venv' = M.insert v ty0 (vEnv env2)
              (bod', regEnv'') <- calculateBoundsExp2 ddefs env2 { vEnv = venv' } szEnv locEnv regEnv bod
              return (LetE (v, locs, ty0, bind') bod', M.unionWith max regEnv' regEnv'')
            CaseE ex2 cases -> do
              (cases', res) <-
                unzip
                  <$> mapM
                        (\(dcon, vlocs, bod) -> do
                          traceM $ "ex2: " ++ sdoc ex2
                          traceM $ "Case: " ++ sdoc (dcon, vlocs, bod)
                          -- (dcon', sz1) <- go dcon 
                          -- TODO insert datacon argument sizes and locations for case arguments
                          -- For now, I'm just putting them at offset 1 from the start, but ideally 
                          -- we have to add up the size of the types and set appropriate offset.
                          -- let szEnv' = M.insert dcon sz1 
                          -- let locEnv' = M.union locEnv $ M.fromList $ map (\(v, l)) vlocs
                          (bod', re) <- calculateBoundsExp2 ddefs env2 szEnv locEnv regEnv bod
                          return ((dcon, vlocs, bod'), re)
                        )
                        cases
              return (CaseE ex2 cases', M.unionsWith max res)
            Ext ext -> case ext of
              LetRegionE reg bod -> do
                (bod', re) <- go bod
                -- TODO now the recursive ADT has been analyzed, we can update the size from regEnv?
                return (Ext $ LetRegionE (AnalyzedRegion reg (regEnv M.! reg)) bod', re)
              LetParRegionE{}        -> todo
              LetLocE loc locExp ex1 -> do
                let (re, rs) = getRegionSize locExp
                let regEnv'  = M.insertWith max re rs regEnv
                let le'      = M.insert loc re locEnv
                (ex1', re') <- calculateBoundsExp2 ddefs env2 szEnv le' regEnv' ex1
                return (Ext $ LetLocE loc locExp ex1', re')
              RetE _locs v -> do
                -- TODO use locs?
                (_, re) <- go (VarE v)
                return (ex, re)
              FromEndE{}         -> todo
              AddFixed{}         -> todo
              GetCilkWorkerNum{} -> todo
              LetAvail{}         -> todo
 where
  getRegionSize :: PreLocExp LocVar -> (Region, RegionSize)
  getRegionSize (StartOfLE r          ) = (r, 0)
  getRegionSize (AfterConstantLE n l  ) = (locEnv M.! l, BoundedSize n)
  getRegionSize (AfterVariableLE v l _) = (locEnv M.! l, szEnv M.! v)
  getRegionSize (InRegionLE r         ) = (r, Undefined)
  getRegionSize (FromEndLE  l         ) = (locEnv M.! l, Unbounded)
  getRegionSize FreeLE                  = error "Not bound to any region"



