{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}

-- | Unique names.

module Gibbon.Passes.ModuleRename (moduleRename) where

import           Control.Exception
import           Data.Foldable ( foldrM )
import           Prelude hiding (exp)
import qualified Data.List as L
import qualified Data.Map as M

import           Gibbon.Common
import           Gibbon.L0.Syntax
import qualified Gibbon.L1.Syntax as L1

--------------------------------------------------------------------------------

type VarEnv     = M.Map Var (M.Map Var Var)
type TyVarEnv t = M.Map TyVar t

moduleRename :: Prog0 -> PassM Prog0
moduleRename (Prog defs funs main) =
    do 
      -- defs
      renamedDefs <- mapM (\k -> (gensym k)) (M.keys defs)
      let defNameMap = M.fromList $ zip (M.keys defs) renamedDefs
      let initDefEnv :: VarEnv = M.empty 
      let (defenv, _) = M.mapAccumWithKey buildEnv initDefEnv defNameMap
      let transformDefKey :: Var -> Var
          transformDefKey k = do
            let (mod, name) = parseOutMod k
            resolveNameInEnv mod name defenv
      
      -- funs
      renamedFuns <- mapM (\k -> (gensym k)) (M.keys funs)
      let funNameMap = M.fromList $ zip (M.keys funs) renamedFuns
      let initFunEnv :: VarEnv = M.empty 
      let (funenv, _) = M.mapAccumWithKey buildEnv initFunEnv funNameMap
      let transformFunKey :: Var -> Var
          transformFunKey k = do
            let (mod, name) = parseOutMod k
            resolveNameInEnv mod name funenv

      -- main
      main' <- case main of
                 Nothing -> return Nothing
                 Just (m,ty) -> do m' <- resolveModInExp m defenv funenv
                                   return $ Just (m',ty)
      
      defs' <- traverse (\v -> resolveModsInDefs v defenv funenv) defs
      funs' <- traverse (\v -> resolveModsInFuns v defenv funenv) funs

      let defs'' = M.mapKeys transformDefKey defs'
      let funs'' = M.mapKeys transformFunKey funs'

      return $ Prog defs'' funs'' main'

resolveModsInDefs :: DDef Ty0 -> VarEnv -> VarEnv -> PassM (DDef Ty0)
resolveModsInDefs (DDef tyName tyArgs dataCons) defenv funenv =
    do 
      let (mod, name) = parseOutMod tyName
      let dataCons' = map (\v -> resolveModsInDataCons v defenv funenv) dataCons
      pure $ DDef (resolveNameInEnv mod name defenv) tyArgs dataCons'

resolveModsInFuns :: FunDef Exp0 -> VarEnv -> VarEnv -> PassM (FunDef Exp0)
resolveModsInFuns (FunDef nam nargs funty bod meta) defenv funenv =
    do 
      let nam' = parseAndResolve nam funenv
      let funty' = resolveModsInTyScheme funty defenv
      let funenv' = foldr appendEnv funenv nargs
      bod' <- resolveModInExp bod defenv funenv' 
      pure $ FunDef nam' nargs funty' bod' meta

resolveModsInTyScheme :: TyScheme -> VarEnv -> TyScheme
resolveModsInTyScheme (ForAll tvs ty) defenv = do
  let ty' = resolveModInTy ty defenv
  ForAll tvs ty'

resolveModsInDataCons :: (DataCon, [(IsBoxed, Ty0)]) -> VarEnv -> VarEnv -> (DataCon, [(IsBoxed, Ty0)])
resolveModsInDataCons (con, tys) defenv funenv =
  do
    let tys' = map (\(boxed, ty) -> (boxed, (resolveModInTy ty defenv))) tys
    (con, tys')

resolveModInTy :: Ty0 -> VarEnv -> Ty0
resolveModInTy ty defenv =
  case ty of
     IntTy    -> ty
     CharTy   -> ty
     FloatTy  -> ty
     SymTy0   -> ty
     BoolTy   -> ty
     ArenaTy  -> ty
     SymSetTy -> ty
     SymHashTy -> ty
     MetaTv{} -> ty
     TyVar tv -> ty
     ProdTy tys    ->  ProdTy $ map (\v -> resolveModInTy v defenv) tys
     SymDictTy v ty   -> SymDictTy v $ resolveModInTy ty defenv
     PDictTy k v -> PDictTy (resolveModInTy k defenv) (resolveModInTy v defenv)
     ArrowTy tys t -> ArrowTy (map (\v -> resolveModInTy v defenv) tys) $ resolveModInTy t defenv
     
     PackedTy tycon tys -> PackedTy (fromVar (parseAndResolve (toVar tycon) defenv)) $ map (\v -> resolveModInTy v defenv) tys
     
     VectorTy el_t -> VectorTy $ resolveModInTy el_t defenv
     ListTy el_t -> ListTy $ resolveModInTy el_t defenv
     IntHashTy -> ty

resolveModInExp :: Exp0 -> VarEnv -> VarEnv -> PassM Exp0
resolveModInExp exp defenv funenv =
  case exp of
    LitE i    -> return $ LitE i
    CharE c   -> return $ CharE c
    FloatE i  -> return $ FloatE i
    LitSymE v -> return $ LitSymE v
    --VarE v -> return $ VarE (varAppend (toVar "seen-") v)
    VarE v -> return $ VarE (parseAndResolve (parseAndResolve v funenv) defenv)

    AppE v locs ls -> do
      let v' = parseAndResolve v funenv
      ls' <- traverse (\v -> resolveModInExp v defenv funenv) ls
      return $ AppE v' locs ls'

    PrimAppE p es -> do
      es' <- traverse (\v -> resolveModInExp v defenv funenv) es
      return $ PrimAppE p es'

    LetE (v,_locs,ty, e1) e2 -> do
      let ty' = resolveModInTy ty defenv
      let funenv' = appendEnv v funenv
      e1' <- resolveModInExp e1 defenv funenv'
      e2' <- resolveModInExp e2 defenv funenv'
      return $ LetE (v, [], ty', e1') e2'

    IfE e1 e2 e3 -> do
      e1' <- resolveModInExp e1 defenv funenv
      e2' <- resolveModInExp e2 defenv funenv
      e3' <- resolveModInExp e3 defenv funenv
      return $ IfE e1' e2' e3'

    ProjE i e -> do
      e' <- resolveModInExp e defenv funenv
      return $ ProjE i e'

    MkProdE es -> do
      es' <- traverse (\v -> resolveModInExp v defenv funenv) es
      return $ MkProdE es'

    CaseE e mp -> do
      e' <- resolveModInExp e defenv funenv
      mp' <- mapM (\(c,prs,ae) -> do
                     ae' <- resolveModInExp ae defenv funenv
                     return (c, prs, ae')) mp
      return $ CaseE e' mp'

    DataConE loc c es -> do
      es' <- traverse (\v -> resolveModInExp v defenv funenv) es
      return $ DataConE loc c es'

    TimeIt e t b -> do
      e' <- resolveModInExp e defenv funenv
      return $ TimeIt e' t b
    WithArenaE v e -> do
      e' <- resolveModInExp e defenv funenv
      return $ WithArenaE v e'
    SpawnE v locs ls -> do
      ls' <- traverse (\v -> resolveModInExp v defenv funenv) ls
      return $ SpawnE v locs ls'
    SyncE -> return $ SyncE
    MapE (v, d, ve) e -> do
      e' <- resolveModInExp e defenv funenv
      ve' <- resolveModInExp ve defenv funenv
      return $ MapE (v, d, ve') e'
    FoldE (v1, d1, e1) (v2, d2, e2) e3 -> do
      e1' <- resolveModInExp e1 defenv funenv
      e2' <- resolveModInExp e2 defenv funenv
      e3' <- resolveModInExp e3 defenv funenv
      return $ FoldE (v1, d1, e1') (v2, d2, e2') e3'
    Ext ext -> case ext of
      LambdaE args bod -> do
        bod' <- resolveModInExp bod defenv funenv
        return $ Ext $ LambdaE args bod'
      PolyAppE a b -> do
        return $ Ext $ PolyAppE a b
      FunRefE tyapps f -> do
        return $ Ext $ FunRefE tyapps f
      BenchE fn tyapps args b -> do
        args' <- mapM (\arg -> resolveModInExp arg defenv funenv) args
        return $ Ext $ BenchE fn tyapps args' b
      ParE0 ls -> do
        ls' <- mapM (\l -> resolveModInExp l defenv funenv) ls
        return $ Ext $ ParE0 ls'
      PrintPacked ty arg -> do
        let ty' = resolveModInTy ty defenv
        arg' <- resolveModInExp arg defenv funenv
        return $ Ext $ PrintPacked ty' arg'
      CopyPacked ty arg -> do
        let ty' = resolveModInTy ty defenv
        arg' <- resolveModInExp arg defenv funenv
        return $ Ext $ CopyPacked ty' arg'
      TravPacked ty arg -> do
        let ty' = resolveModInTy ty defenv
        arg' <- resolveModInExp arg defenv funenv
        return $ Ext $ TravPacked ty' arg'
      L p e -> do
        e' <- resolveModInExp e defenv funenv
        return $ Ext $ L p e'
      LinearExt a -> do
        return $ Ext $ LinearExt a


-- environment interactions

appendEnv :: Var -> VarEnv -> VarEnv
appendEnv v env =
  case (M.lookup v env) of
    Just m -> (M.insert v (M.insert (toVar "") v m) env)
    Nothing -> (M.insert v (M.singleton (toVar "") v) env)

buildEnv :: VarEnv -> Var -> Var -> (VarEnv, Var)
buildEnv env k v = do
  let (mod, name) = parseOutMod k
  let mod' = case mod of
              Just m -> m
              Nothing -> (toVar "")
  case (M.lookup name env) of
    Just m -> 
      if (M.member mod' m) then error $ "duplicate function call in mod" ++ (fromVar mod')
      else ((M.insert name (M.insert mod' v m) env), v)
    Nothing -> ((M.insert name (M.singleton mod' v) env), v)

parseAndResolve :: Var -> VarEnv -> Var
parseAndResolve v env = do
  let (mod, name) = parseOutMod v
  resolveNameInEnv mod name env

parseOutMod :: Var -> (Maybe Var, Var)
parseOutMod v = do
  let str = (fromVar v)
  case (L.elemIndices '.' str) of
    [] -> (Nothing, v)
    x -> (Just (toVar (L.take (L.last x) str)), (toVar (L.drop ((L.last x)+1) str)))

resolveNameInEnv :: Maybe Var -> Var -> VarEnv -> Var
resolveNameInEnv mod name e = 
  do case (M.lookup name e) of
      Just modspace -> case mod of
        Just m -> case (M.lookup m modspace) of
                    Just n -> n
                    Nothing -> error $ "can't find " ++ (fromVar name) ++ " in module " ++ (fromVar m)
        Nothing -> case (M.lookup (toVar "") modspace) of
                    Just n -> n
                    Nothing ->  if(M.size modspace == 1) then head $ M.elems modspace
                                else error $ "can't find/ambiguous reference " ++ (fromVar name) 
      Nothing -> case mod of
                  Just m -> (toVar ((fromVar m) ++ "." ++ (fromVar name)))
                  Nothing -> name
      --Nothing -> error $ "can't find " ++ (fromVar name)