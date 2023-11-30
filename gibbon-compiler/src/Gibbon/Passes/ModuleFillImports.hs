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

import qualified Language.Haskell.Exts.Syntax as H

--------------------------------------------------------------------------------

type VarEnv     = M.Map Var (M.Map Var (Var, Bool))
type TyVarEnv t = M.Map TyVar t

fillImports :: Prog0 -> Var -> [H.ImportDecl a] -> [Prog0]-> PassM Prog0
fillImports (Prog defs funs main) localMod imports imported_progs =
    do 

      -- resolve aliases in imported functions
      let importMeta = getImportMeta imports
      let initImportedNames :: [(Var, (Var, Bool))] = [] 
      let applyImportMeta :: Var -> [(Var, (Var, Bool))] -> [(Var, (Var, Bool))]
          applyImportMeta v acc = do
            let (mod, name) = parseOutMod v 
            case mod of
              Just modName ->
                case (M.lookup modName importMeta) of
                  Just (alias, qual, specs) ->
                    case specs of 
                      Just spec -> if (elem name spec) then acc ++ [(toVar ((fromVar alias) ++ "." ++ (fromVar name)), (v, qual))]
                                   else acc
                      Nothing -> acc ++ [(toVar ((fromVar alias) ++ "." ++ (fromVar name)), (v, qual))]
                  Nothing -> error $ "could not find module or alias: " ++ (show modName)
              Nothing -> error "how did we get here?"

      let importedConstrs = foldr (\(Prog idefs _ _) acc -> acc ++ (foldr applyImportMeta [] (L.map (\(constrName, _) -> (toVar constrName)) (foldr (\(DDef _ _ dataCons) acc -> acc ++ dataCons) [] idefs)))) initImportedNames imported_progs
      let localConstrs = (L.map (\(constrName, _) -> 
                          (toVar constrName)) (foldr (\(DDef _ _ dataCons) acc -> 
                            acc ++ dataCons
                          ) [] (M.elems defs))
                        )
      let importedDefs = foldr (\(Prog idefs _ _) acc -> acc ++ (foldr applyImportMeta [] (M.keys idefs))) initImportedNames imported_progs
      let importedFuns = foldr (\(Prog _ ifuns _) acc -> acc ++ (foldr applyImportMeta [] (M.keys ifuns))) initImportedNames imported_progs
      
      let initEnv :: VarEnv = M.empty
      -- build constructor env

      let constrNameMap = M.fromList $ importedConstrs ++ (zip localConstrs (map (\v -> (v, False)) localConstrs))
      let (constrenv, _) = M.mapAccumWithKey buildEnv initEnv constrNameMap

      -- build def env
      let defNameMap = M.fromList $ importedDefs ++ (zip (M.keys defs) (map (\v -> (v, False)) (M.keys defs)))
      let (defenv, _) = M.mapAccumWithKey buildEnv initEnv defNameMap
      let transformDefKey :: Var -> Var
          transformDefKey k = do
            let (mod, name) = parseOutMod k
            resolveNameInEnv mod name defenv
      
      -- build fun env
      let funNameMap = M.fromList $ importedFuns ++ (zip (M.keys funs) (map (\v -> (v, False)) (M.keys funs)) )
      let (funenv, _) = M.mapAccumWithKey buildEnv initEnv funNameMap
      let transformFunKey :: Var -> Var
          transformFunKey k = do
            let (mod, name) = parseOutMod k
            resolveNameInEnv mod name funenv

      -- main
      main' <- case main of
                 Nothing -> return Nothing
                 Just (m,ty) -> do --error $ "fun env: " ++ (show funenv)
                                   m' <- resolveModInExp m defenv funenv constrenv
                                   return $ Just (m',ty)
      

      defs' <- traverse (\v -> resolveModsInDefs v defenv funenv) defs
      funs' <- traverse (\v -> resolveModsInFuns v defenv funenv constrenv) funs

      let defs'' = M.mapKeys transformDefKey defs'
      let funs'' = M.mapKeys transformFunKey funs'

      return $ Prog defs'' funs'' main'

getImportMeta :: [H.ImportDecl a] -> M.Map Var (Var, Bool, Maybe [Var])
getImportMeta imports = do 
  let parseSpecs :: Maybe (H.ImportSpecList a)-> Maybe [Var]
      parseSpecs maybeSpec = do
        case maybeSpec of
          Just (H.ImportSpecList _ _ specList) ->
              Just $ map (\spec -> case spec of
                H.IVar _ n -> case n of
                  H.Ident _ v -> toVar v
                  H.Symbol _ v -> toVar v
                H.IAbs _ _ n -> case n of
                  H.Ident _ v -> toVar v
                  H.Symbol _ v -> toVar v
                H.IThingAll _ n -> case n of
                  H.Ident _ v -> toVar v
                  H.Symbol _ v -> toVar v
                H.IThingWith _ n _ -> case n of
                  H.Ident _ v -> toVar v
                  H.Symbol _ v -> toVar v
              ) specList
          Nothing -> Nothing
  M.fromList $ map 
    (\(H.ImportDecl _ (H.ModuleName _ importName) qualified _ _ _ aliased spec) -> 
      case aliased of
          Just (H.ModuleName _ importAs) ->
            ((toVar importName), ((toVar importAs), qualified, (parseSpecs spec)))
          Nothing -> ((toVar importName), ((toVar importName), qualified, (parseSpecs spec)))
      ) 
    imports

resolveModsInDefs :: DDef Ty0 -> VarEnv -> VarEnv -> PassM (DDef Ty0)
resolveModsInDefs (DDef tyName tyArgs dataCons) defenv funenv =
    do 
      let (mod, name) = parseOutMod tyName
      let dataCons' = map (\v -> resolveModsInDataCons v defenv funenv) dataCons
      pure $ DDef (resolveNameInEnv mod name defenv) tyArgs dataCons'

resolveModsInFuns :: FunDef Exp0 -> VarEnv -> VarEnv -> VarEnv -> PassM (FunDef Exp0)
resolveModsInFuns (FunDef nam nargs funty bod meta) defenv funenv constrenv =
    do 
      let nam' = parseAndResolve nam funenv
      let funty' = resolveModsInTyScheme funty defenv
      bod' <- resolveModInExp bod defenv funenv constrenv
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

resolveModInExp :: Exp0 -> VarEnv -> VarEnv -> VarEnv -> PassM Exp0
resolveModInExp exp defenv funenv constrenv =
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
      ls' <- traverse (\v -> resolveModInExp v defenv funenv constrenv) ls
      return $ AppE v' locs ls'

    PrimAppE p es -> do
      es' <- traverse (\v -> resolveModInExp v defenv funenv constrenv) es
      return $ PrimAppE p es'

    LetE (v,_locs,ty, e1) e2 -> do
      e1' <- resolveModInExp e1 defenv funenv constrenv
      e2' <- resolveModInExp e2 defenv funenv constrenv
      return $ LetE (v, [], ty, e1') e2'

    IfE e1 e2 e3 -> do
      e1' <- resolveModInExp e1 defenv funenv constrenv
      e2' <- resolveModInExp e2 defenv funenv constrenv
      e3' <- resolveModInExp e3 defenv funenv constrenv
      return $ IfE e1' e2' e3'

    ProjE i e -> do
      e' <- resolveModInExp e defenv funenv constrenv
      return $ ProjE i e'

    MkProdE es -> do
      es' <- traverse (\v -> resolveModInExp v defenv funenv constrenv) es
      return $ MkProdE es'

    CaseE e mp -> do
      e' <- resolveModInExp e defenv funenv constrenv
      mp' <- mapM (\(c,prs,ae) -> do
                     let c' = (fromVar (parseAndResolve (toVar c) constrenv))
                     ae' <- resolveModInExp ae defenv funenv constrenv
                     return (c', prs, ae')) mp
      return $ CaseE e' mp'

    DataConE loc c es -> do
      let c' = (fromVar (parseAndResolve (toVar c) constrenv))
      es' <- traverse (\v -> resolveModInExp v defenv funenv constrenv) es
      return $ DataConE loc c' es'

    TimeIt e t b -> do
      e' <- resolveModInExp e defenv funenv constrenv
      return $ TimeIt e' t b
    WithArenaE v e -> do
      e' <- resolveModInExp e defenv funenv constrenv
      return $ WithArenaE v e'
    SpawnE v locs ls -> do
      ls' <- traverse (\v -> resolveModInExp v defenv funenv constrenv) ls
      return $ SpawnE v locs ls'
    SyncE -> return $ SyncE

    MapE (v, d, ve) e -> do
      e' <- resolveModInExp e defenv funenv constrenv
      ve' <- resolveModInExp ve defenv funenv constrenv
      return $ MapE (v, d, ve') e'
    FoldE (v1, d1, e1) (v2, d2, e2) e3 -> do
      e1' <- resolveModInExp e1 defenv funenv constrenv
      e2' <- resolveModInExp e2 defenv funenv constrenv
      e3' <- resolveModInExp e3 defenv funenv constrenv
      return $ FoldE (v1, d1, e1') (v2, d2, e2') e3'
    Ext ext -> return $ Ext ext


-- environment interactions

buildEnv :: VarEnv -> Var -> (Var, Bool) -> (VarEnv, (Var, Bool))
buildEnv env k v = do
  let (mod, name) = parseOutMod k
  let mod' = case mod of
              Just m -> m
              Nothing -> (toVar "")
  case (M.lookup name env) of
    Just m -> 
      if (M.member mod' m) then error $ "duplicate function call in module" ++ (fromVar mod')
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
                    Just (n, _) -> n
                    Nothing -> error $ "can't find " ++ (fromVar name) ++ " in module " ++ (fromVar m) ++ " in env: " ++ (show e)
        Nothing -> case (foldr findUnqualified (False, "") modspace) of
                    (True, n) -> n
                    (False, _) -> error $ "can't find unquilified reference to " ++ (fromVar name)
      Nothing -> error $ "can't find " ++ (fromVar name) ++ " in env: " ++ (show e)

findUnqualified :: (Var, Bool) -> (Bool, Var) -> (Bool, Var)
findUnqualified (n, q) (acc, out) =
      if (not q) && (not acc) then (True, n)
      else if (not q) && acc then error $ "Ambiguous reference"
      else (acc, out)