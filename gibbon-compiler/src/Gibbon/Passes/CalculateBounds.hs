{-# LANGUAGE FlexibleInstances #-}
module Gibbon.Passes.CalculateBounds where

import           Gibbon.Common
import qualified Data.Map                      as M
import           Gibbon.L2.Syntax
import           Debug.Trace
import Data.List

type LocationMapping = M.Map LocVar Var
type VarSizeMapping = M.Map Var RegionSize
type RegionSizeMapping = M.Map Var RegionSize
type RegionTypeMapping = M.Map Var Modality

calculateBounds :: Prog2 -> PassM Prog2
calculateBounds Prog { ddefs, fundefs, mainExp } = do
  let env2 = Env2 M.empty (initFunEnv fundefs)
  fundefs' <- mapM (calculateBoundsFun ddefs env2 M.empty) fundefs
  mainExp' <- case mainExp of
    Nothing       -> return Nothing
    Just (mn, ty) -> Just . (, ty) . fst <$> calculateBoundsExp ddefs env2 M.empty M.empty M.empty mn
  return $ Prog ddefs fundefs' mainExp'


calculateBoundsFun :: DDefs Ty2 -> Env2 Ty2 -> VarSizeMapping -> FunDef2 -> PassM FunDef2
calculateBoundsFun ddefs env2 szEnv f@FunDef { funBody, funTy, funArgs } = do
  let locEnv = M.fromList $ map (\lv -> (lrmLoc lv, regionToVar $ lrmReg lv)) (locVars funTy)
  let argTys = M.fromList $ zip funArgs (arrIns funTy)
  let env2'  = env2 { vEnv = argTys }
  funBody' <- fst <$> calculateBoundsExp ddefs env2' szEnv locEnv M.empty funBody
  return $ f { funBody = funBody' }

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
calculateBoundsExp ddefs env2 szEnv locEnv regEnv ex = case ex of
  Ext  (BoundsCheck{}                ) -> return (ex, regEnv)
  Ext  (IndirectionE _ _ (loc, _) _ _) -> do
    let res = (ex, M.insertWith (+) (locEnv M.! loc) 9 regEnv)
    return res
  VarE v                               -> case M.lookup v szEnv of
    Just _ -> return (ex, regEnv)
    _      -> do
      -- TODO: handle this case?
      return (ex, regEnv)
  _ ->
    let ty   = gRecoverType ddefs env2 ex
        go   = calculateBoundsExp ddefs env2 szEnv locEnv regEnv
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
            AppE _ _locs _args -> do
              return (ex, regEnv)
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
              (bod', regEnv'') <- calculateBoundsExp ddefs env2 { vEnv = venv' } szEnv locEnv regEnv bod
              return (LetE (v, locs, ty0, bind') bod', M.unionWith max regEnv' regEnv'')
            CaseE ex2 cases -> do
              (cases', res) <-
                unzip
                  <$> mapM
                        (\(dcon :: DataCon, vlocs :: [(Var, LocVar)], bod :: Exp2) -> do
                          -- let offsets = M.fromList 
                          --       . tail -- remove tag
                          --       . scanl1 (\(_, s1) (v2, s2) -> (v2, s1+s2)) -- accumulate offsets
                          --       . ((undefined, BoundedSize 1):)  -- add size for tag
                          --       . zip (map snd vlocs) -- take locations
                          --       . map (maybe Undefined BoundedSize . sizeOfTy) -- map to our region size type
                          --       $ lookupDataCon ddefs dcon -- find ddef)
                          -- * NOTE: After thinking, no location can be allocated from offset to an internal argument
                          -- of a data constructor, except jumps so we can skip this analysis.
                          (bod', re) <- go bod
                          return ((dcon, vlocs, bod'), re)
                        )
                        cases
              return (CaseE ex2 cases', M.unionsWith max res)
            Ext ext -> case ext of
              LetRegionE reg bod -> do
                (bod', re) <- go bod
                traceM $ ">> RegionSize: " ++ show reg ++ " -> " ++ show (re M.! regionToVar reg)
                return (Ext $ LetRegionE (AnalyzedRegion reg (re M.! regionToVar reg)) bod', re)
              LetParRegionE{}        -> todo
              LetLocE loc locExp ex1 -> do
                -- * NOTE: jumps are only necessary for route ends, skipping them.
                if "jump_" `isPrefixOf` fromVar loc
                  then do
                    (ex1', re') <- go ex1
                    return (Ext $ LetLocE loc locExp ex1', re')
                  else do
                    let (re, rs) = getRegionSize locExp
                    let regEnv'  = M.insertWith (+) re rs regEnv
                    let le'      = M.insert loc re locEnv
                    (ex1', re') <- calculateBoundsExp ddefs env2 szEnv le' regEnv' ex1
                    return (Ext $ LetLocE loc locExp ex1', re')
              RetE _locs v -> do
                (_, re) <- go (VarE v)
                return (ex, re)
              FromEndE{}         -> todo
              AddFixed{}         -> todo
              GetCilkWorkerNum{} -> todo
              LetAvail{}         -> todo
 where
  getRegionSize :: PreLocExp LocVar -> (Var, RegionSize)
  getRegionSize (StartOfLE r          ) = (regionToVar r, 0)
  getRegionSize (AfterConstantLE n l  ) = (locEnv M.! l, BoundedSize n)
  getRegionSize (AfterVariableLE v l _) = (locEnv M.! l, szEnv M.! v)
  getRegionSize (InRegionLE r         ) = (regionToVar r, Undefined)
  getRegionSize (FromEndLE  l         ) = (locEnv M.! l, Undefined)
  getRegionSize FreeLE                  = error "Not bound to any region"



