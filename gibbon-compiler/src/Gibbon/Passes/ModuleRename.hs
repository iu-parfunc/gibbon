{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}

-- | Unique names.

module Gibbon.Passes.ModuleRename (moduleRename) where

import           Gibbon.Pretty

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
    do --main' <- case main of
       --           Nothing -> return Nothing
       --           Just (m,ty) -> do m' <- freshExp M.empty M.empty m
       --                             return $ Just (m',ty)
       --defs' <- traverse freshDDef defs
       --funs' <- *traverse map*  resolveFunModuleNames funs
      renamedFuns <- mapM (\k -> (gensym k)) (M.keys funs)
      let nameMap = M.fromList $ zip (M.keys funs) renamedFuns
      let init :: VarEnv = M.empty 
      let (env, _) = M.mapAccumWithKey buildEnv init nameMap
      --let funs' = M.mapKeys (\k -> resolveNameInEnv () env) funs
      let _ = dbgPrintLn 2 "hello?"
      let transformKey :: Var -> Var
          transformKey k = do
            let (mod, name) = parseOutMod k
            resolveNameInEnv mod name env
      funs' <- traverse (\v -> resolveModsInFuns v defs env) funs
      let funs'' = M.mapKeys transformKey funs'
      return $ Prog defs funs'' main

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

resolveNameInEnv :: Maybe Var -> Var -> VarEnv -> Var
resolveNameInEnv mod name e = 
  do case (M.lookup name e) of
      Just modspace -> case mod of
        Just m -> case (M.lookup m modspace) of
                    Just n -> n
                    Nothing -> error $ "can't find function " ++ (fromVar name) ++ " in module " ++ (fromVar m)
        Nothing -> if(M.size modspace == 1) then head $ M.elems modspace
                   else error $ "can't find function " ++ (fromVar name)
      Nothing -> error $ "can't find function " ++ (fromVar name)

freshDDef :: DDef Ty0 -> PassM (DDef Ty0)
freshDDef DDef{tyName,tyArgs,dataCons} = 
  do pure $ DDef tyName tyArgs dataCons

--funRename :: FunDef Exp0 -> FunDef Exp0
--funRename (FunDef nam nargs funty bod meta) = FunDef (gensym nam) nargs funty bod meta

resolveModsInFuns :: FunDef Exp0 -> DDefs Ty0 -> VarEnv -> PassM (FunDef Exp0)
resolveModsInFuns (FunDef nam nargs funty bod meta) defs env =
    do 
      let (mod, name) = parseOutMod nam
      bod' <- resolveModInBod bod env
      pure $ FunDef (resolveNameInEnv mod name env) nargs funty bod' meta

resolveModInBod :: Exp0 -> VarEnv -> PassM Exp0
resolveModInBod exp env =
  case exp of
    LitE i    -> return $ LitE i
    CharE c   -> return $ CharE c
    FloatE i  -> return $ FloatE i
    LitSymE v -> return $ LitSymE v
    --VarE v -> return $ VarE (varAppend (toVar "seen-") v)
    VarE v -> return $ VarE v

    AppE v locs ls -> do
      let (mod, fun) = parseOutMod v
      let v' = resolveNameInEnv mod fun env
      return $ AppE v' locs ls

    PrimAppE p es -> return $ PrimAppE p es

    LetE (v,_locs,ty, e1) e2 -> do
      e1' <- resolveModInBod e1 env
      e2' <- resolveModInBod e2 env
      return $ LetE (v, [], ty, e1') e2'

    IfE e1 e2 e3 -> return $ IfE e1 e2 e3
    ProjE i e -> return $ ProjE i e
    MkProdE es -> return $ MkProdE es

    CaseE e mp -> do
      e' <- resolveModInBod e env
      mp' <- mapM (\(c,prs,ae) -> do
                     ae' <- resolveModInBod ae env
                     return (c, prs, ae')) mp
      return $ CaseE e' mp'

    DataConE loc c es -> do
      es' <- traverse (\v -> resolveModInBod v env) es
      return $ DataConE loc c es'

    TimeIt e t b -> return $ TimeIt e t b
    WithArenaE v e -> return $ WithArenaE v e
    SpawnE v locs ls -> return $ SpawnE v locs ls
    SyncE -> return $ SyncE
    MapE v e -> return $ MapE v e
    FoldE e1 e2 e3 -> return $ FoldE e1 e2 e3
    Ext ext -> return $ Ext ext

parseOutMod :: Var -> (Maybe Var, Var)
parseOutMod v = do
  let str = (fromVar v)
  case (L.elemIndices '.' str) of
    [] -> (Nothing, v)
    x -> (Just (toVar (L.take (L.last x) str)), (toVar (L.drop ((L.last x)+1) str)))