{-# LANGUAGE OverloadedStrings #-}

module Packed.FirstOrder.Passes.ThreadRegions where

import Data.Loc
import Data.List as L
import qualified Data.Map as M

import Packed.FirstOrder.Common hiding (FunDef(..))
import Packed.FirstOrder.L1.Syntax hiding (Prog(..), FunDef(..))
import Packed.FirstOrder.L2.Syntax as L2

-- Maps a location to a region
type LocEnv = M.Map LocVar Var

type TypeEnv = M.Map Var Ty2

threadRegions :: L2.Prog -> SyM L2.Prog
threadRegions Prog{ddefs,fundefs,mainExp} = do
  fds' <- mapM (threadRegionsFn ddefs fundefs) $ M.elems fundefs
  let fundefs' = M.fromList $ map (\f -> (funname f,f)) fds'
  mainExp' <- case mainExp of
                Nothing -> return Nothing
                Just (mn, ty) -> Just . (,ty) <$>
                  threadRegionsExp ddefs fundefs True M.empty M.empty mn
  return $ Prog ddefs fundefs' mainExp'

threadRegionsFn :: DDefs Ty2 -> NewFuns -> L2.FunDef -> SyM L2.FunDef
threadRegionsFn ddefs fundefs f@FunDef{funarg,funty,funbod} = do
  let initLocEnv = M.fromList $ map (\(LRM lc r _) -> (lc, regionVar r)) (locVars funty)
      initTyEnv  = M.singleton funarg (arrIn funty)
  bod' <- threadRegionsExp ddefs fundefs False initLocEnv initTyEnv funbod
  return $ f {funbod = bod'}

threadRegionsExp :: DDefs Ty2 -> NewFuns -> Bool -> LocEnv -> TypeEnv -> L L2.Exp2
              -> SyM (L L2.Exp2)
threadRegionsExp ddefs fundefs isMain lenv tenv (L p ex) = L p <$>
  case ex of
    LetE (v,locs,ty, rhs@(L _ (AppE f applocs arg))) bod
      | hasPacked ty -> do
          let tylocs = getTyLocs ty
              regs   = map (lenv M.!) tylocs
          regs' <- mapM gensym regs
          -- Update all locations to point to the fresh region
          let lenv' = foldr (\(lc,r,r') acc ->
                               M.insert lc r' $
                               M.map (\tyl -> if tyl == r then r' else tyl) acc)
                      lenv
                      (L.zip3 tylocs regs regs')
              newlocs    = (map toEndV regs') ++ locs
              newapplocs = (map toEndV regs)  ++ applocs
          LetE (v, newlocs, ty, l$ AppE f newapplocs arg) <$>
            threadRegionsExp ddefs fundefs isMain lenv' (M.insert v ty tenv) bod

      | otherwise ->
          LetE (v,locs,ty, rhs) <$>
            threadRegionsExp ddefs fundefs isMain lenv (M.insert v ty tenv) bod

    LetE (v,locs,ty, rhs) bod ->
      LetE <$> (v,locs,ty,) <$> go rhs <*>
        threadRegionsExp ddefs fundefs isMain lenv (M.insert v ty tenv) bod

    Ext ext ->
      case ext of
        -- Update lenv with a binding for loc
        LetLocE loc rhs bod -> do
          let reg = case rhs of
                      StartOfLE r  -> regionVar r
                      InRegionLE r -> regionVar r
                      AfterConstantLE _ lc -> lenv M.! lc
                      AfterVariableLE _ lc -> lenv M.! lc
                      FromEndLE lc         -> lenv M.! lc
          Ext <$> LetLocE loc rhs <$>
            threadRegionsExp ddefs fundefs isMain (M.insert loc reg lenv) tenv bod

        -- Implements (2)
        RetE locs v -> do
          let ty = tenv M.! v
          if not isMain && hasPacked ty
          then do
            let tylocs  = getTyLocs ty
                regs    = map (lenv M.!) tylocs
                newlocs = map toEndV regs
            return $ Ext $ RetE (newlocs ++ locs) v
          else return $ Ext ext

        LetRegionE r bod -> Ext <$> LetRegionE r <$> go bod
        FromEndE{}    -> return ex
        BoundsCheck sz _bound cur -> do
          return $ Ext $ BoundsCheck sz (toEndV (lenv M.! cur)) cur

    -- Straightforward recursion

    VarE{}     -> return ex
    LitE{}     -> return ex
    LitSymE{}  -> return ex
    AppE{}     -> return ex
    PrimAppE{} -> return ex
    DataConE{} -> return ex
    ProjE i e  -> ProjE i <$> go e
    IfE a b c  -> IfE <$> go a <*> go b <*> go c
    MkProdE ls -> MkProdE <$> mapM go ls
    CaseE scrt mp -> do
      let L _ (VarE v) = scrt
          PackedTy _ tyloc = tenv M.! v
          reg = lenv M.! tyloc
      CaseE scrt <$> mapM (docase reg lenv tenv) mp
    TimeIt e ty b -> do
      e' <- go e
      return $ TimeIt e' ty b
    MapE{}  -> error $ "go: TODO MapE"
    FoldE{} -> error $ "go: TODO FoldE"

  where
    go = threadRegionsExp ddefs fundefs isMain lenv tenv
    toEndV = varAppend "end_"

    docase reg lenv1 tenv1 (dcon,vlocs,bod) = do
      -- Update the envs with bindings for pattern matched variables and locations.
      -- The locations point to the same region as the scrutinee.
      let (vars,locs) = unzip vlocs
          lenv1' = foldr (\lc acc -> M.insert lc reg acc) lenv1 locs
          tys = lookupDataCon ddefs dcon
          tenv1' = foldr (\(x,ty) acc -> M.insert x ty acc) tenv1 (zip vars tys)
      (dcon,vlocs,) <$> (threadRegionsExp ddefs fundefs isMain lenv1' tenv1' bod)
