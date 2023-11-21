{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}

-- | Unique names.

module Gibbon.Passes.ModuleFillImports (fillImports) where

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

fillImports :: Prog0 -> PassM Prog0
fillImports (Prog defs funs main) =
    do 
      -- defs
      let defNameMap = M.fromList $ zip (M.keys defs) (M.keys defs)
      let initDefEnv :: VarEnv = M.empty 
      let (defenv, _) = M.mapAccumWithKey buildEnv initDefEnv defNameMap
      let transformDefKey :: Var -> Var
          transformDefKey k = do
            let (mod, name) = parseOutMod k
            resolveNameInEnv mod name defenv
      
      -- funs
      let funNameMap = M.fromList $ zip (M.keys funs) (M.keys funs)
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
      bod' <- resolveModInExp bod defenv funenv 
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
    VarE v -> return $ VarE v

    AppE v locs ls -> do
      let (mod, fun) = parseOutMod v
      let v' = resolveNameInEnv mod fun funenv
      ls' <- traverse (\v -> resolveModInExp v defenv funenv) ls
      return $ AppE v' locs ls

    PrimAppE p es -> return $ PrimAppE p es

    LetE (v,_locs,ty, e1) e2 -> do
      e1' <- resolveModInExp e1 defenv funenv
      e2' <- resolveModInExp e2 defenv funenv
      return $ LetE (v, [], ty, e1') e2'

    IfE e1 e2 e3 -> return $ IfE e1 e2 e3
    ProjE i e -> return $ ProjE i e
    MkProdE es -> return $ MkProdE es

    CaseE e mp -> do
      e' <- resolveModInExp e defenv funenv
      mp' <- mapM (\(c,prs,ae) -> do
                     ae' <- resolveModInExp ae defenv funenv
                     return (c, prs, ae')) mp
      return $ CaseE e' mp'

    DataConE loc c es -> do
      es' <- traverse (\v -> resolveModInExp v defenv funenv) es
      return $ DataConE loc c es'

    TimeIt e t b -> return $ TimeIt e t b
    WithArenaE v e -> return $ WithArenaE v e
    SpawnE v locs ls -> return $ SpawnE v locs ls
    SyncE -> return $ SyncE
    MapE v e -> return $ MapE v e
    FoldE e1 e2 e3 -> return $ FoldE e1 e2 e3
    Ext ext -> return $ Ext ext


-- environment interactions

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
        Nothing -> if(M.size modspace == 1) then head $ M.elems modspace
                   else error $ "can't find " ++ (fromVar name)
      Nothing -> error $ "can't find " ++ (fromVar name)