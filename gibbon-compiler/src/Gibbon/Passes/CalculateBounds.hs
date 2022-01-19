{-# LANGUAGE FlexibleInstances #-}
module Gibbon.Passes.CalculateBounds where

import           Gibbon.Common
import qualified Data.Map                      as M
import           Gibbon.L2.Syntax
import           Debug.Trace
import           Data.List                      ( unzip4 )

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
    Just (mn, ty) -> Just . (, ty) . fst4 <$> calculateBoundsExp2 ddefs env2 M.empty M.empty M.empty M.empty mn
  return $ Prog ddefs fundefs' mainExp'


calculateBoundsFun :: DDefs Ty2 -> Env2 Ty2 -> VarSizeMapping -> FunDef2 -> PassM FunDef2
calculateBoundsFun ddefs env2 szEnv f@FunDef { funBody, funTy, funArgs } = do
  let locEnv = M.fromList $ map (\lv -> (lrmLoc lv, lrmReg lv)) (locVars funTy)
  let regTy = M.fromList $ map (\lv -> (lrmReg lv, lrmMode lv)) (locVars funTy)
  let argTys = M.fromList $ zip funArgs (arrIns funTy)
  let env2'  = env2 { vEnv = argTys }
  funBody' <- fst4 <$> calculateBoundsExp2 ddefs env2' szEnv locEnv M.empty regTy funBody
  return $ f { funBody = funBody' }

calculateBoundsExp2
  :: DDefs Ty2
  -> Env2 Ty2
  -> VarSizeMapping
  -> LocationMapping
  -> RegionSizeMapping
  -> RegionTypeMapping
  -> Exp2
  -> PassM (Exp2, RegionSize, LocationMapping, RegionSizeMapping)
calculateBoundsExp2 ddefs env2 szEnv locEnv regEnv regTy ex = do
  res <- calculateBoundsExp ddefs env2 szEnv locEnv regEnv regTy ex
  traceM $ "res = " ++ sdoc res
  return res


calculateBoundsExp
  :: DDefs Ty2 -- ^ Data Definitions
  -> Env2 Ty2 -- ^ Type Environment (Variables + Functions)
  -> VarSizeMapping -- ^ var => region
  -> LocationMapping -- ^ location => region
  -> RegionSizeMapping -- ^ region => size 
  -> RegionTypeMapping -- ^ region => input/ouput
  -> Exp2 -- ^ expression 
  -> PassM (Exp2, RegionSize, LocationMapping, RegionSizeMapping)
  -- TODO do we really need to return regionsize mapiing or location mapping?
-- TODO: track allocations to locations and update the bound for region
-- TODO: know which regions are input (0 allocations)
calculateBoundsExp ddefs env2 szEnv locEnv regEnv regTy ex = case ex of
  Ext  (BoundsCheck{}                ) -> return (ex, Unbounded, locEnv, regEnv)
  Ext  (IndirectionE _ _ (loc, _) _ _) -> return (ex, 9, locEnv, M.insertWith (+) (locEnv M.! loc) 9 regEnv)
  VarE v                               -> case M.lookup v szEnv of
    Just w -> return (ex, w, locEnv, regEnv)
    _      -> do
      traceM $ "No size in env for " ++ sdoc v ++ " (" ++ sdoc szEnv ++ ")"
      return (ex, Undefined, locEnv, regEnv)
  _ ->
    let
      ty   = gRecoverType ddefs env2 ex
      go   = calculateBoundsExp2 ddefs env2 szEnv locEnv regEnv regTy
      err  = error "Should have been covered by sizeOfTy"
      todo = return (ex, Undefined, locEnv, regEnv)
    in
      case sizeOfTy ty of
        Just v -> return (ex, BoundedSize v, locEnv, regEnv)
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
            return (ex, Unbounded, locEnv, regEnv)
            -- TODO no change to regions by primapp so pick up from regEnv?
          PrimAppE{}             -> return (ex, Unbounded, locEnv, regEnv)
          DataConE loc dcon args -> do
            (_, szs, les, res) <- unzip4 <$> mapM go args
            return (DataConE loc dcon args, 1 + sum szs, mconcat les, mconcat res)
          IfE cond bod1 bod2 -> do
            (bod1', sz1, le1, regEnv1) <- go bod1
            (bod2', sz2, le2, regEnv2) <- go bod2
            let sz' = max sz1 sz2
            return (IfE cond bod1' bod2', sz', le1 <> le2, M.unionWith max regEnv1 regEnv2)
          MkProdE ls -> do
            (ls', szs', les, regEnvs) <- unzip4 <$> mapM go ls
            let sz = sum szs'
            return (MkProdE ls', sz, mconcat les, M.unionsWith max regEnvs)
          LetE (v, locs, ty0, bind) bod -> do
            (bind', sz1, le1, regEnv') <- go bind
            let venv'  = M.insert v ty0 (vEnv env2)
                szEnv' = M.insert v sz1 szEnv
            (bod', sz, le2, regEnv'') <- calculateBoundsExp2 ddefs env2 { vEnv = venv' } szEnv' locEnv regEnv regTy bod
            return (LetE (v, locs, ty0, bind') bod', sz, le1 <> le2, M.unionWith max regEnv' regEnv'')
          CaseE ex2 cases -> do
            (cases', sizes, les, res) <-
              unzip4
                <$> mapM
                      (\(dcon, vlocs, bod) -> do
                        traceM $ "ex2: " ++ sdoc ex2
                        traceM $ "CAse: " ++ sdoc (dcon, vlocs, bod)
                        -- (dcon', sz1) <- go dcon 
                        -- TODO insert datacon argument sizes and locations for case arguments
                        -- let szEnv' = M.insert dcon sz1 
                        -- let locEnv' = M.union locEnv $ M.fromList $ map (\(v, l)) vlocs
                        (bod', sz2, le, re) <- calculateBoundsExp2 ddefs env2 szEnv locEnv regEnv regTy bod
                        return ((dcon, vlocs, bod'), sz2, le, re)
                      )
                      cases
            let sz' = maximum sizes
            return (CaseE ex2 cases', sz', mconcat les, M.unionsWith max res)
          Ext ext -> case ext of
            LetRegionE reg bod -> do
              (bod', sz, le, re) <- go bod
              -- TODO now the recursive ADT has been analyzed, we can update the size from regEnv?
              return (Ext $ LetRegionE (AnalyzedRegion reg sz) bod', sz, le, re)
            LetParRegionE{}        -> todo
            LetLocE loc locExp ex1 -> do
              let (re, rs) = getRegionSize locExp
              let regEnv'  = M.insertWith max re rs regEnv
              let le'      = M.insert loc re locEnv
              (ex1', sz, le'', re') <- calculateBoundsExp2 ddefs env2 szEnv le' regEnv' regTy ex1
              return (Ext $ LetLocE loc locExp ex1', sz, le'', re')
            RetE _locs v -> do
              -- TODO use locs?
              (_, sz, le, re) <- go (VarE v)
              return (ex, sz, le, re)
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



