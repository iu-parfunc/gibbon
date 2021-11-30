module Gibbon.Passes.CalculateBounds where

import           Gibbon.Common
import qualified Data.Map                      as M
import           Gibbon.L2.Syntax
import           Debug.Trace
import           Data.Maybe



calculateBounds :: Prog2 -> PassM Prog2
calculateBounds Prog { ddefs, fundefs, mainExp } = do
  let env2 = Env2 M.empty (initFunEnv fundefs)
  fundefs' <- mapM (calculateBoundsFun ddefs env2 M.empty) fundefs
  mainExp' <- case mainExp of
    Nothing       -> return Nothing
    Just (mn, ty) -> Just . (, ty) . fst <$> calculateBoundsExp2 ddefs env2 M.empty mn
  return $ Prog ddefs fundefs' mainExp'


calculateBoundsFun :: DDefs Ty2 -> Env2 Ty2 -> M.Map Var RegionSize -> FunDef2 -> PassM FunDef2
calculateBoundsFun ddefs env2 szEnv f@FunDef { funBody } = do
  funBody' <- fst <$> calculateBoundsExp2 ddefs env2 szEnv funBody
  return $ f { funBody = funBody' }

calculateBoundsExp2 ddefs env2 szEnv ex = do
  res <- calculateBoundsExp ddefs env2 szEnv ex
  traceShowM res
  return res

calculateBoundsExp :: DDefs Ty2 -> Env2 Ty2 -> M.Map Var RegionSize -> Exp2 -> PassM (Exp2, RegionSize)
calculateBoundsExp ddefs env2 szEnv ex = case ex of
  Ext  (BoundsCheck{} ) -> return (ex, Unbounded)
  Ext  (IndirectionE{}) -> return (ex, BoundedSize 8)
  VarE v                -> case M.lookup v szEnv of
    Just w -> return (ex, w)
    _      -> do 
      traceM $ "No size in env for " ++ sdoc v ++ " (" ++ sdoc szEnv ++ ")"
      return (ex, Undefined)
  _ ->
    let ty   = gRecoverType ddefs env2 ex
        go   = calculateBoundsExp2 ddefs env2 szEnv
        err  = error "Should have been covered by sizeOfTy"
        todo = return (ex, Undefined)
    in  case sizeOfTy ty of
          Just v -> return (ex, BoundedSize v)
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
            AppE{}                 -> return (ex, Unbounded)
            PrimAppE{}             -> return (ex, Unbounded)
            DataConE loc dcon args -> do
              szs <- map snd <$> mapM go args
              return (DataConE loc dcon args, 1 + sum szs)
            IfE cond bod1 bod2 -> do
              (bod1', sz1) <- go bod1
              (bod2', sz2) <- go bod2
              let sz' = max sz1 sz2
              return (IfE cond bod1' bod2', sz')
            MkProdE ls -> do
              (ls', szs') <- unzip <$> mapM go ls
              let sz = sum szs'
              return (MkProdE ls', sz)
            LetE (v, locs, ty0, bind) bod -> do
              (bind', sz1) <- go bind
              let venv'  = M.insert v ty0 (vEnv env2)
                  szEnv' = M.insert v sz1 szEnv
              (bod', sz) <- calculateBoundsExp2 ddefs env2 { vEnv = venv' } szEnv' bod
              return (LetE (v, locs, ty0, bind') bod', sz)
            CaseE ex2 cases -> do
              (cases', sizes) <-
                unzip
                  <$> mapM
                        (\(dcon, vlocs, bod) -> do
                          -- (dcon', sz1) <- go dcon
                          -- TODO insert datacon types
                          traceM $ "ddefs: " ++ sdoc ddefs
                          traceM $ "dcon: " ++ sdoc dcon ++ ", vlocs = " ++ sdoc vlocs ++", bod = " ++ sdoc bod
                          -- let szEnv' = M.insert dcon sz1 
                          (bod', sz2) <- calculateBoundsExp2 ddefs env2 szEnv bod
                          return ((dcon, vlocs, bod'), sz2)
                        )
                        cases
              let sz' = maximum sizes
              return (CaseE ex2 cases', sz')
            Ext ext -> case ext of
              LetRegionE reg bod -> do
                (bod', sz) <- go bod
                return (Ext $ LetRegionE (AnalyzedRegion reg sz) bod', sz)
              LetParRegionE{} -> todo
              LetLocE a b ex1 -> do
                (ex1', sz) <- go ex1
                return (Ext $ LetLocE a b ex1', sz)
              RetE _ v -> do
                (_, sz) <- go (VarE v)
                return (ex, sz)
              FromEndE{}         -> todo
              AddFixed{}         -> todo
              GetCilkWorkerNum{} -> todo
              LetAvail{}         -> todo

