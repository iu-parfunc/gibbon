module Gibbon.Passes.CalculateBounds where

import           Gibbon.Common
import qualified Data.Map                      as M
import           Gibbon.L2.Syntax
import           Debug.Trace

type LocationMapping = M.Map LocVar Region
type VarSizeMapping = M.Map Var RegionSize

calculateBounds :: Prog2 -> PassM Prog2
calculateBounds Prog { ddefs, fundefs, mainExp } = do
  let env2 = Env2 M.empty (initFunEnv fundefs)
  fundefs' <- mapM (calculateBoundsFun ddefs env2 M.empty) fundefs
  mainExp' <- case mainExp of
    Nothing       -> return Nothing
    Just (mn, ty) -> Just . (, ty) . fst3 <$> calculateBoundsExp2 ddefs env2 M.empty M.empty mn
  return $ Prog ddefs fundefs' mainExp'


calculateBoundsFun :: DDefs Ty2 -> Env2 Ty2 -> M.Map Var RegionSize -> FunDef2 -> PassM FunDef2
calculateBoundsFun ddefs env2 szEnv f@FunDef { funBody } = do
  funBody' <- fst3 <$> calculateBoundsExp2 ddefs env2 szEnv M.empty funBody
  return $ f { funBody = funBody' }

calculateBoundsExp2 ddefs env2 szEnv locEnv ex = do
  res <- calculateBoundsExp ddefs env2 szEnv locEnv ex
  traceShowM res
  return res


calculateBoundsExp
  :: DDefs Ty2 -- ^ Data Definitions
  -> Env2 Ty2 -- ^ Type Environment (Variables + Functions)
  -> VarSizeMapping
  -> LocationMapping
  -> Exp2 -- ^ expression 
  -> PassM (Exp2, RegionSize, LocationMapping)
-- TODO: track allocations to locations and update the bound for region
-- TODO: know which regions are input (0 allocations)
calculateBoundsExp ddefs env2 szEnv locEnv ex = case ex of
  Ext  (BoundsCheck{} ) -> return (ex, Unbounded, locEnv)
  Ext  (IndirectionE{}) -> return (ex, BoundedSize 8, locEnv)
  VarE v                -> case M.lookup v szEnv of
    Just w -> return (ex, w, locEnv)
    _      -> do
      traceM $ "No size in env for " ++ sdoc v ++ " (" ++ sdoc szEnv ++ ")"
      return (ex, Undefined, locEnv)
  _ ->
    let ty   = gRecoverType ddefs env2 ex
        go   = calculateBoundsExp2 ddefs env2 szEnv locEnv
        err  = error "Should have been covered by sizeOfTy"
        todo = return (ex, Undefined, locEnv)
    in  case sizeOfTy ty of
          Just v -> return (ex, BoundedSize v, locEnv)
          _      -> case ex of
            LitE    _              -> err
            FloatE  _              -> err
            LitSymE _              -> err
            ProjE{}                -> todo
            TimeIt{}               -> todo
            WithArenaE{}           -> todo
            SpawnE{}               -> todo
            SyncE{}                -> todo
            MapE{}                 -> todo
            FoldE{}                -> todo
            -- TODO use input/output loc 
            AppE v locs args       -> return (ex, Unbounded, locEnv)
            PrimAppE{}             -> return (ex, Unbounded, locEnv)
            DataConE loc dcon args -> do
              (_, szs, les) <- unzip3 <$> mapM go args
              return (DataConE loc dcon args, 1 + sum szs, mconcat les)
            IfE cond bod1 bod2 -> do
              (bod1', sz1, le1) <- go bod1
              (bod2', sz2, le2) <- go bod2
              let sz' = max sz1 sz2
              return (IfE cond bod1' bod2', sz', le1 <> le2)
            MkProdE ls -> do
              (ls', szs', les) <- unzip3 <$> mapM go ls
              let sz = sum szs'
              return (MkProdE ls', sz, mconcat les)
            LetE (v, locs, ty0, bind) bod -> do
              (bind', sz1, le1) <- go bind
              let venv'  = M.insert v ty0 (vEnv env2)
                  szEnv' = M.insert v sz1 szEnv
              (bod', sz, le2) <- calculateBoundsExp2 ddefs env2 { vEnv = venv' } szEnv' locEnv bod
              return (LetE (v, locs, ty0, bind') bod', sz, le1 <> le2)
            CaseE ex2 cases -> do
              (cases', sizes, les) <-
                unzip3
                  <$> mapM
                        (\(dcon, vlocs, bod) -> do
                          -- (dcon', sz1) <- go dcon
                          -- TODO insert datacon types
                          traceM $ "ddefs: " ++ sdoc ddefs
                          traceM $ "dcon: " ++ sdoc dcon ++ ", vlocs = " ++ sdoc vlocs ++ ", bod = " ++ sdoc bod
                          -- let szEnv' = M.insert dcon sz1 
                          (bod', sz2, le) <- calculateBoundsExp2 ddefs env2 szEnv locEnv bod
                          return ((dcon, vlocs, bod'), sz2, le)
                        )
                        cases
              let sz' = maximum sizes
              return (CaseE ex2 cases', sz', mconcat les)
            Ext ext -> case ext of
              LetRegionE reg bod -> do
                (bod', sz, le) <- go bod
                return (Ext $ LetRegionE (AnalyzedRegion reg sz) bod', sz, le)
              LetParRegionE{}        -> todo
              LetLocE loc locExp ex1 -> do
                (ex1', sz, le) <- go ex1
                -- TODO also update region size here and make it 4-tuple and return updated region size. That means expression size gets invalidated - so do I remove it from the 3-tuple and add region size instead, making it a 3-tuple again?
                let le' = M.insert loc (getRegion le locExp) le
                return (Ext $ LetLocE loc locExp ex1', sz, le')
              -- TODO use locs
              RetE locs v -> do
                (_, sz, le) <- go (VarE v)
                return (ex, sz, le)
              FromEndE{}         -> todo
              AddFixed{}         -> todo
              GetCilkWorkerNum{} -> todo
              LetAvail{}         -> todo

getRegion :: LocationMapping -> PreLocExp LocVar -> Region
getRegion _  (StartOfLE r          ) = r
getRegion le (AfterConstantLE _ l  ) = le M.! l
getRegion le (AfterVariableLE _ l _) = le M.! l
getRegion _  (InRegionLE r         ) = r
getRegion le (FromEndLE  l         ) = le M.! l
getRegion _  FreeLE                  = error "Not bounded to any region"


