module Gibbon.Passes.CalculateBounds where

import           Gibbon.Common
import qualified Data.Map                      as M
import           Gibbon.L2.Syntax



calculateBounds :: Prog2 -> PassM Prog2
calculateBounds Prog { ddefs, fundefs, mainExp } = do
  let env2 = Env2 M.empty (initFunEnv fundefs)
  fundefs' <- mapM (calculateBoundsFun ddefs env2) fundefs
  mainExp' <- case mainExp of
    Nothing       -> return Nothing
    Just (mn, ty) -> Just . (, ty) . fst <$> calculateBoundsExp ddefs env2 mn
  return $ Prog ddefs fundefs' mainExp'

calculateBoundsFun
  :: DDefs Ty2 -> Env2 Ty2 -> FunDef2 -> PassM FunDef2
calculateBoundsFun ddefs env2 f@FunDef { funBody } = do
  funBody' <- fst <$> calculateBoundsExp ddefs env2 funBody
  return $ f { funBody = funBody' }

calculateBoundsExp
  :: DDefs Ty2 -> Env2 Ty2 -> Exp2 -> PassM (Exp2, RegionSize)
calculateBoundsExp ddefs env2 ex =
  let ty = gRecoverType ddefs env2 ex
      go = calculateBoundsExp ddefs env2
  in  case sizeOfTy ty of
        Just v -> return (ex, BoundedSize v)
        _      -> case ex of
          AppE{}                 -> return (ex, Unbounded)
          PrimAppE{}             -> return (ex, Unbounded)
          DataConE loc dcon args -> do
            szs <- map snd <$> mapM go args
            return (DataConE loc dcon args, 1 + sum szs)
          -- ProjE i exp   -> return (ex, go (exp !!i)) -- TODO what if exp is not a list?
          IfE cond bod1 bod2 -> do
            (bod1', sz1) <- go bod1
            (bod2', sz2) <- go bod2
            let sz' = max sz1 sz2
            return (IfE cond bod1' bod2', sz')
          MkProdE ls -> do
            (ls', szs') <- unzip <$> mapM go ls
            let sz = sum szs'
            return (MkProdE ls', sz)
          LetE (v, locs, dec, bind) bod -> do
            bind' <- fst <$> go bind
            (bod', sz)  <- go bod
            return (LetE (v, locs, dec, bind') bod', sz)
          CaseE ex2 cases -> do
            (cases', sizes) <- unzip <$> mapM (\(dcon, vlocs, bod) -> do 
              (bod', sz) <- go bod 
              return ((dcon, vlocs, bod'), sz)) cases
            let sz' = maximum sizes
            return (CaseE ex2 cases', sz')
          Ext ext -> case ext of
            LetRegionE reg bod -> do
              (bod', sz) <- go bod
              return (Ext $ LetRegionE (AnalyzedRegion reg sz) bod', Unbounded) -- no escape?
            IndirectionE{} -> return (ex, BoundedSize 8)


