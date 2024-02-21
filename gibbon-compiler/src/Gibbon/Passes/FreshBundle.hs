{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Fuse foldr/map" #-}

module Gibbon.Passes.FreshBundle (freshBundleNames) where
import qualified Data.Map               as M
import qualified Data.List              as L
import           Gibbon.Common
import Gibbon.L0.Syntax
import Language.Haskell.Exts (Name, ImportDecl (ImportDecl), SrcSpanInfo, name, ImportSpec (..), CName)
import Data.Maybe  (fromJust)
import Language.Haskell.Exts.Syntax (CName(..))
import Language.Haskell.Exts (Name(..))
import Language.Haskell.Exts (ImportSpecList(..))
import Language.Haskell.Exts (ModuleName(..), Module, eList)
import GHC.Stack (HasCallStack)

type VarEnv = M.Map Var Var

-------------------------------------------------------------------------------
-- exported fresh bundle pass
-- replaces all references with new, globally unique names (data types, functions, constructors)
-------------------------------------------------------------------------------

freshBundleNames :: ProgBundle0 -> PassM ProgBundle0
freshBundleNames bundle = do
    -- build global map of uniques
    -- {legal reference} => unique
    (uniquedefenv, uniquefunenv, uniqueconstenv) <- buildGlobalEnv bundle

    let ProgBundle modules main = bundle
    -- run through modules, fresh names
    modules' <- mapM (\v -> freshModule v bundle uniquedefenv uniquefunenv uniqueconstenv) modules
    main' <- freshModule main bundle uniquedefenv uniquefunenv uniqueconstenv
    -- update keys
    modules'' <- mapM (\v -> freshModuleKeys v uniquedefenv uniquefunenv) modules'
    main'' <- freshModuleKeys main' uniquedefenv uniquefunenv
    pure $ ProgBundle modules'' main''

-- helper functions -----------------------------------------------------------

-- update the keys
freshModuleKeys :: ProgModule0 -> VarEnv -> VarEnv -> PassM ProgModule0
freshModuleKeys (ProgModule name (Prog defs funs main) imports) uniquedefenv uniquefunenv = do
  let funs' = M.mapKeys (\k -> findFreshedName (varAppend (toVar (name ++ ".")) k) uniquefunenv) funs
  let defs' = M.mapKeys (\k -> findFreshedName (varAppend (toVar (name ++ ".")) k) uniquedefenv) defs
  pure $ ProgModule name (Prog defs' funs' main) imports

-- find the imported module based off an import declarations
findImportedModule :: ImportDecl SrcSpanInfo -> M.Map String ProgModule0 -> ProgModule0
findImportedModule mod modmap = do
  let (ImportDecl _ (ModuleName _ name) _ _ _ _ _ _) = mod
  case M.lookup name modmap of
    Just found -> found
    Nothing -> error $ "Could not find module " ++ name ++ " in imported modules: " ++ (show (M.keys modmap))

-- transform names to their unique counterparts
freshModule :: ProgModule0 -> ProgBundle0 -> VarEnv -> VarEnv -> VarEnv -> PassM ProgModule0
freshModule (ProgModule modname (Prog defs funs main) imports) (ProgBundle bundle _) uniquedefenv uniquefunenv uniqueconstrenv =
    do
       defs' <- traverse (\v -> freshDDef v defenv'' constrenv'') defs 
       funs' <- traverse (\v -> freshFun v defenv'' funenv'' constrenv'') funs 
       main' <- case main of
                  Nothing -> return Nothing
                  Just (m,ty) -> do m' <- findFreshInExp m defenv'' funenv'' constrenv''
                                    return $ Just (m',ty)
       return $ ProgModule modname (Prog defs' funs' main') imports
    where
        modname' = toVar (modname ++ ".")
        constrs = L.map (\(constrName, _) -> toVar constrName) 
                    (foldr (\(DDef _ _ dataCons) acc -> acc ++ dataCons) [] (M.elems defs))
        -- add qualified and unqualified names to the env
        modmap = M.fromList $ L.zip (L.map (\(ProgModule m _ _) -> m) bundle) bundle
        funenv = foldr (\f acc -> M.insert (varAppend modname' f) (findFreshedName (varAppend modname' f) uniquefunenv) acc) M.empty (M.keys funs)
        defenv = foldr (\d acc -> M.insert (varAppend modname' d) (findFreshedName (varAppend modname' d) uniquedefenv) acc) M.empty (M.keys defs)
        constrenv = foldr(\c acc -> M.insert (varAppend modname' c) (findFreshedName (varAppend modname' c) uniqueconstrenv) acc) M.empty constrs
        funenv' = foldr (\f acc -> M.insert f (findFreshedName (varAppend modname' f) uniquefunenv) acc) funenv (M.keys funs)
        defenv' = foldr (\d acc -> M.insert d (findFreshedName (varAppend modname' d) uniquedefenv) acc) defenv (M.keys defs)
        constrenv' = foldr (\c acc -> M.insert c (findFreshedName (varAppend modname' c) uniqueconstrenv) acc) constrenv constrs
        (defenv'', funenv'', constrenv'') = 
          foldr (\(d, f, c) (dacc, facc, cacc) -> (M.union d dacc, M.union f facc, M.union c cacc)) (defenv', funenv', constrenv') 
          $ map (\i -> getImportedEnv (findImportedModule i modmap) i uniquedefenv uniquefunenv uniqueconstrenv) imports

freshDDef :: HasCallStack => DDef Ty0 -> VarEnv -> VarEnv -> PassM (DDef Ty0)
freshDDef DDef{tyName,tyArgs,dataCons} defenv constrenv = do
    let dataCons' = L.map (\(dataCon, vs) -> (fromVar (findFreshedName (toVar dataCon) constrenv), vs)) dataCons
    let dataCons'' = L.map (\v -> findFreshInDataCons v defenv) dataCons'
    let tyName' = findFreshedName tyName defenv
    pure $ DDef tyName' tyArgs dataCons''

freshFun :: FunDef Exp0 -> VarEnv -> VarEnv -> VarEnv -> PassM (FunDef Exp0)
freshFun (FunDef nam nargs funty bod meta) defenv funenv constrenv =
    do 
      let nam' = findFreshedName nam funenv
      funty' <- findFreshInTyScheme funty defenv
      let funenv' = foldr (\v acc -> M.insert v v acc ) funenv nargs 
      bod' <- findFreshInExp bod defenv funenv' constrenv
      pure $ FunDef nam' nargs funty' bod' meta

findFreshInTyScheme :: TyScheme -> VarEnv -> PassM TyScheme
findFreshInTyScheme (ForAll tvs ty) defenv = do
  pure $ ForAll tvs $ findFreshInTy ty defenv

findFreshInTy :: Ty0 -> VarEnv -> Ty0
findFreshInTy ty defenv = 
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
     ProdTy tys    -> ProdTy $ L.map (\v -> findFreshInTy v defenv) tys
     SymDictTy v t   -> SymDictTy v $ findFreshInTy t defenv
     PDictTy k v -> do
        let k' = findFreshInTy k defenv
        let v' = findFreshInTy v defenv
        PDictTy k' v'  
     ArrowTy tys t -> do
        let tys' = L.map (\v -> findFreshInTy v defenv) tys
        let t' = findFreshInTy t defenv
        ArrowTy tys' t'
     PackedTy tycon tys -> PackedTy (fromVar (findFreshedName (toVar tycon) defenv)) $ L.map (\v -> findFreshInTy v defenv) tys
     VectorTy el_t -> VectorTy $ findFreshInTy el_t defenv
     ListTy el_t -> ListTy $ findFreshInTy el_t defenv
     IntHashTy -> ty

findFreshInDataCons :: (DataCon, [(IsBoxed, Ty0)]) -> VarEnv -> (DataCon, [(IsBoxed, Ty0)])
findFreshInDataCons (con, tys) defenv =
  do
    let tys' = L.map (\(boxed, ty) -> (boxed, (findFreshInTy ty defenv))) tys
    (con, tys')

findFreshInExp :: Exp0 -> VarEnv -> VarEnv -> VarEnv -> PassM Exp0
findFreshInExp exp defenv funenv constrenv =
  case exp of
    LitE i    -> return $ LitE i
    CharE c   -> return $ CharE c
    FloatE i  -> return $ FloatE i
    LitSymE v -> return $ LitSymE v
    --VarE v -> return $ VarE (varAppend (toVar "seen-") v)
    VarE v -> return $ VarE (tryToFindFreshedName v funenv)

    AppE v locs ls -> do
      let v' = findFreshedName v funenv
      ls' <- traverse (\v -> findFreshInExp v defenv funenv constrenv) ls
      return $ AppE v' locs ls'

    PrimAppE p es -> do
      es' <- traverse (\v -> findFreshInExp v defenv funenv constrenv) es
      return $ PrimAppE p es'

    LetE (v,_locs,ty, e1) e2 -> do
      let ty' = findFreshInTy ty defenv
      let funenv' = M.insert v v funenv
      e1' <- findFreshInExp e1 defenv funenv' constrenv
      e2' <- findFreshInExp e2 defenv funenv' constrenv
      return $ LetE (v, [], ty', e1') e2'

    IfE e1 e2 e3 -> do
      e1' <- findFreshInExp e1 defenv funenv constrenv
      e2' <- findFreshInExp e2 defenv funenv constrenv
      e3' <- findFreshInExp e3 defenv funenv constrenv
      return $ IfE e1' e2' e3'

    ProjE i e -> do
      e' <- findFreshInExp e defenv funenv constrenv
      return $ ProjE i e'

    MkProdE es -> do
      es' <- traverse (\v -> findFreshInExp v defenv funenv constrenv) es
      return $ MkProdE es'

    CaseE e mp -> do
      e' <- findFreshInExp e defenv funenv constrenv
      mp' <- mapM (\(c,prs,ae) -> do
                    let c' = (fromVar (findFreshedName (toVar c) constrenv))
                    ae' <- findFreshInExp ae defenv funenv constrenv
                    return (c', prs, ae')) mp
      return $ CaseE e' mp'

    DataConE loc c es -> do
      let c' = (fromVar (findFreshedName (toVar c) constrenv))
      es' <- traverse (\v -> findFreshInExp v defenv funenv constrenv) es
      return $ DataConE loc c' es'

    TimeIt e t b -> do
      e' <- findFreshInExp e defenv funenv constrenv
      return $ TimeIt e' t b
    WithArenaE v e -> do
      e' <- findFreshInExp e defenv funenv constrenv
      return $ WithArenaE v e'
    SpawnE v locs ls -> do
      ls' <- traverse (\v -> findFreshInExp v defenv funenv constrenv) ls
      return $ SpawnE v locs ls'
    SyncE -> return $ SyncE
    MapE (v, d, ve) e -> do
      e' <- findFreshInExp e defenv funenv constrenv
      ve' <- findFreshInExp ve defenv funenv constrenv
      return $ MapE (v, d, ve') e'
    FoldE (v1, d1, e1) (v2, d2, e2) e3 -> do
      e1' <- findFreshInExp e1 defenv funenv constrenv
      e2' <- findFreshInExp e2 defenv funenv constrenv
      e3' <- findFreshInExp e3 defenv funenv constrenv
      return $ FoldE (v1, d1, e1') (v2, d2, e2') e3'
    Ext ext -> case ext of
      LambdaE args bod -> do
        bod' <- findFreshInExp bod defenv funenv constrenv
        return $ Ext $ LambdaE args bod'
      PolyAppE a b -> do
        return $ Ext $ PolyAppE a b
      FunRefE tyapps f -> do
        return $ Ext $ FunRefE tyapps f
      BenchE fn tyapps args b -> do
        args' <- mapM (\arg -> findFreshInExp arg defenv funenv constrenv) args
        return $ Ext $ BenchE fn tyapps args' b
      ParE0 ls -> do
        ls' <- mapM (\l -> findFreshInExp l defenv funenv constrenv) ls
        return $ Ext $ ParE0 ls'
      PrintPacked ty arg -> do
        let ty' = findFreshInTy ty defenv
        arg' <- findFreshInExp arg defenv funenv constrenv
        return $ Ext $ PrintPacked ty' arg'
      CopyPacked ty arg -> do
        let ty' = findFreshInTy ty defenv
        arg' <- findFreshInExp arg defenv funenv constrenv
        return $ Ext $ CopyPacked ty' arg'
      TravPacked ty arg -> do
        let ty' = findFreshInTy ty defenv
        arg' <- findFreshInExp arg defenv funenv constrenv
        return $ Ext $ TravPacked ty' arg'
      L p e -> do
        e' <- findFreshInExp e defenv funenv constrenv
        return $ Ext $ L p e'
      LinearExt a -> do
        return $ Ext $ LinearExt a

-- parse import header and map legal references to unique names
getImportedEnv :: ProgModule0 -> ImportDecl SrcSpanInfo -> VarEnv -> VarEnv -> VarEnv -> (VarEnv, VarEnv, VarEnv)
getImportedEnv (ProgModule _ (Prog defs funs _) _) imp uniquedefenv uniquefunenv uniqueconstrenv = do
    let ImportDecl _ (ModuleName _ impname) qual _ _ _ as specs = imp
    let impname' = toVar (impname ++ ".")
    let qualname = case as of
                      Just (ModuleName _ n) -> toVar $ n ++ "."
                      Nothing -> toVar $ impname ++ "."
    let constrs = L.map (\(constrName, _) -> toVar constrName) 
                    (foldr (\(DDef _ _ dataCons) acc -> acc ++ dataCons) [] (M.elems defs))
    let impenv :: (VarEnv, VarEnv, VarEnv)
        impenv = case specs of
          Just (ImportSpecList _ _ speclist) -> do
            let specednames = foldr (\v acc -> (parseSpec v) ++ acc) [] speclist
            let funs' = foldr (\k acc -> case M.lookup (varAppend impname' k) uniquefunenv of
                                  Nothing -> acc
                                  Just found -> do 
                                      let acc' = M.insert (varAppend qualname k) found acc
                                      if qual then acc'
                                      else M.insert k found acc'
                              ) M.empty specednames
            let defs' = foldr (\k acc -> case M.lookup (varAppend impname' k) uniquedefenv of
                                  Nothing -> acc
                                  Just found -> do 
                                      let acc' = M.insert (varAppend qualname k) found acc
                                      if qual then acc'
                                      else M.insert k found acc'
                              ) M.empty specednames
            let constrs' = foldr (\k acc -> case M.lookup (varAppend impname' k) uniqueconstrenv of
                                  Nothing -> acc
                                  Just found -> do 
                                      let acc' = M.insert (varAppend qualname k) found acc
                                      if qual then acc'
                                      else M.insert k found acc'
                              ) M.empty specednames
            (defs', funs', constrs')
          Nothing -> do
            let funs' = foldr (\k acc -> do
                                let found = findFreshedName (varAppend impname' k) uniquefunenv
                                let acc' = M.insert (varAppend qualname k) found acc
                                if qual then acc'
                                else M.insert k found acc'
                              ) M.empty (M.keys funs)
            let defs' = foldr (\k acc -> do
                                let found = findFreshedName (varAppend impname' k) uniquedefenv
                                let acc' = M.insert (varAppend qualname k) found acc
                                if qual then acc'
                                else M.insert k found acc'
                              ) M.empty (M.keys defs)
            let constrs' = foldr (\k acc -> case M.lookup (varAppend impname' k) uniqueconstrenv of
                                  Nothing -> acc
                                  Just found -> do 
                                      let acc' = M.insert (varAppend qualname k) found acc
                                      if qual then acc'
                                      else M.insert k found acc'
                              ) M.empty constrs
            (defs', funs', constrs')
    impenv

-- simple helper functions to convert `Name`s and `CNames`s  to Vars
name2var :: Name SrcSpanInfo -> Var
name2var name = case name of 
  Ident l str -> toVar str
  Symbol l str -> toVar str
cname2var :: CName SrcSpanInfo -> Var
cname2var name = case name of 
  VarName l str -> name2var str
  ConName l str -> name2var str

-- parse the import header speclist
parseSpec :: ImportSpec SrcSpanInfo -> [Var]
parseSpec imp = 
  case imp of 
    -- imported a variable
    IVar l nm -> [name2var nm]
    -- a class, datatype, or type
    IAbs l nmspc nm -> [name2var nm]
    -- a class with all it's methods, or a datatype with all it's constructors
    IThingAll l nm -> [name2var nm]
    -- a class with some of it's methods, or a datatype with some of it's constructors
    IThingWith l nm thgs -> [name2var nm] ++ map cname2var thgs

  
-- construct global registry of uniques
-- returns Map {qualified name} => {globally unique name}
buildGlobalEnv :: ProgBundle0 -> PassM (VarEnv, VarEnv, VarEnv)
buildGlobalEnv (ProgBundle modules main) = do
    (ddefenv, fdefenv, constrenv) <- _buildGlobalEnv main -- generate uniques in main module
    names <- mapM _buildGlobalEnv modules -- generate uniques in imported modules
    pure $ foldr (\(ddefs, fdefs, constrs) (dacc, facc, cacc) -> (M.union ddefs dacc, M.union fdefs facc, M.union constrs cacc)) (ddefenv, fdefenv, constrenv) names -- union

-- generate map of qualified names to uniques for a module
_buildGlobalEnv :: ProgModule0 -> PassM (VarEnv, VarEnv, VarEnv)
_buildGlobalEnv (ProgModule modname (Prog ddefs fdefs _) _) =
    do
        freshfdefs <- mapM gensym fdefs' -- generate uniques
        freshddefs <- mapM gensym ddefs'
        freshconstrs <- mapM gensym constrs'
        let fdefenv = M.fromList $ zip (L.map (\v -> varAppend modname' v) fdefs') freshfdefs -- map qualified names to uniques
        let ddefenv = M.fromList $ zip (L.map (\v -> varAppend modname' v) ddefs') freshddefs
        let constrenv = M.fromList $ zip (L.map (\v -> varAppend modname' v) constrs') freshconstrs
        pure (ddefenv, fdefenv, constrenv)
    where
        modname' = toVar (modname ++ ".")
        constrs = L.map (\(constrName, _) -> toVar constrName) 
                    (foldr (\(DDef _ _ dataCons) acc -> acc ++ dataCons) [] (M.elems ddefs))
        fdefs' = M.keys fdefs -- create qualified names
        ddefs' = M.keys ddefs
        constrs' = constrs

-- helper functions
-- try to find the name, but don't cry if you can't,,, used for VarEs
tryToFindFreshedName :: Var -> VarEnv -> Var
tryToFindFreshedName name e =
  do case M.lookup name e of
      Just freshname -> freshname
      Nothing -> name

-- map a legal reference to a unique name
findFreshedName :: HasCallStack => Var -> VarEnv -> Var
findFreshedName name e = 
  case M.lookup name e of
      Just freshname -> freshname
      Nothing -> error $ "could not find name: " ++ (fromVar name) ++ "\n in env: " ++ (show e)
