{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}

module Gibbon.Passes.FreshConstructors (freshConstructors) where 

import qualified Data.List as L
import qualified Data.Map as M

import           Gibbon.Common
import           Gibbon.L0.Syntax

type VarEnv     = M.Map Var Var

freshConstructors :: Prog0 -> PassM Prog0
freshConstructors (Prog defs funs main) = do
  let constrs :: [Var] = (L.map (\(constrName, _) -> 
                          (toVar constrName)) (foldr (\(DDef _ _ dataCons) acc -> 
                            acc ++ dataCons
                          ) [] (M.elems defs))
                        )
  freshNames <- (mapM (\v -> gensym v) constrs)
  let constrenv :: VarEnv = M.fromList $ zip constrs freshNames

  main' <- case main of
                 Nothing -> return Nothing
                 Just (m,ty) -> do --error $ "fun env: " ++ (show funenv)
                                   m' <- freshConstrsInExp m constrenv
                                   return $ Just (m',ty)
  defs' <- traverse (\v -> freshConstrsInDef v constrenv) defs
  funs' <- traverse (\v -> freshConstrsInFun v constrenv) funs

  return $ Prog defs' funs' main'

freshConstrsInDef :: DDef Ty0 -> VarEnv -> PassM (DDef Ty0)
freshConstrsInDef (DDef tyName tyArgs dataCons) constrenv =
    do 
      let dataCons' = map (\(dataCon, vs) -> 
                              case (M.lookup (toVar dataCon) constrenv) of
                                  Just dataCon' -> ((fromVar dataCon'), vs)
                                  Nothing -> error $ "shouldn't get here"
                          ) dataCons
      pure $ DDef tyName tyArgs dataCons'

--

freshConstrsInFun :: FunDef Exp0 -> VarEnv -> PassM (FunDef Exp0)
freshConstrsInFun (FunDef nam nargs funty bod meta) constrenv =
    do 
      bod' <- freshConstrsInExp bod constrenv
      pure $ FunDef nam nargs funty bod' meta

freshConstrsInExp :: Exp0 -> VarEnv -> PassM Exp0
freshConstrsInExp exp constrenv =
  case exp of
    LitE i    -> return $ LitE i
    CharE c   -> return $ CharE c
    FloatE i  -> return $ FloatE i
    LitSymE v -> return $ LitSymE v
    --VarE v -> return $ VarE (varAppend (toVar "seen-") v)
    VarE v -> return $ VarE v

    AppE v locs ls -> do
      ls' <- traverse (\v -> freshConstrsInExp v constrenv) ls
      return $ AppE v locs ls'

    PrimAppE p es -> do
      es' <- traverse (\v -> freshConstrsInExp v constrenv) es
      return $ PrimAppE p es'

    LetE (v,_locs,ty, e1) e2 -> do
      e1' <- freshConstrsInExp e1 constrenv
      e2' <- freshConstrsInExp e2 constrenv
      return $ LetE (v, [], ty, e1') e2'

    IfE e1 e2 e3 -> do
      e1' <- freshConstrsInExp e1 constrenv
      e2' <- freshConstrsInExp e2 constrenv
      e3' <- freshConstrsInExp e3 constrenv
      return $ IfE e1' e2' e3'

    ProjE i e -> do
      e' <- freshConstrsInExp e constrenv
      return $ ProjE i e'

    MkProdE es -> do
      es' <- traverse (\v -> freshConstrsInExp v constrenv) es
      return $ MkProdE es'

    CaseE e mp -> do
      e' <- freshConstrsInExp e constrenv
      mp' <- mapM (\(c,prs,ae) -> do
                     let c' = (fromVar (resolveNameInEnv (toVar c) constrenv))
                     ae' <- freshConstrsInExp ae constrenv
                     return (c', prs, ae')) mp
      return $ CaseE e' mp'

    DataConE loc c es -> do
      let c' = (fromVar (resolveNameInEnv (toVar c) constrenv))
      es' <- traverse (\v -> freshConstrsInExp v constrenv) es
      return $ DataConE loc c' es'

    TimeIt e t b -> do
      e' <- freshConstrsInExp e constrenv
      return $ TimeIt e' t b
    WithArenaE v e -> do
      e' <- freshConstrsInExp e constrenv
      return $ WithArenaE v e'
    SpawnE v locs ls -> do
      ls' <- traverse (\v -> freshConstrsInExp v constrenv) ls
      return $ SpawnE v locs ls'
    SyncE -> return $ SyncE

    MapE (v, d, ve) e -> do
      e' <- freshConstrsInExp e constrenv
      ve' <- freshConstrsInExp ve constrenv
      return $ MapE (v, d, ve') e'
    FoldE (v1, d1, e1) (v2, d2, e2) e3 -> do
      e1' <- freshConstrsInExp e1 constrenv
      e2' <- freshConstrsInExp e2 constrenv
      e3' <- freshConstrsInExp e3 constrenv
      return $ FoldE (v1, d1, e1') (v2, d2, e2') e3'
    Ext ext -> case ext of
      LambdaE args bod -> do
        bod' <- freshConstrsInExp bod constrenv
        return $ Ext $ LambdaE args bod'
      PolyAppE a b -> do
        return $ Ext $ PolyAppE a b
      FunRefE tyapps f -> do
        return $ Ext $ FunRefE tyapps f
      BenchE fn tyapps args b -> do
        args' <- mapM (\arg -> freshConstrsInExp arg constrenv) args
        return $ Ext $ BenchE fn tyapps args' b
      ParE0 ls -> do
        ls' <- mapM (\l -> freshConstrsInExp l constrenv) ls
        return $ Ext $ ParE0 ls'
      PrintPacked ty arg -> do
        arg' <- freshConstrsInExp arg constrenv
        return $ Ext $ PrintPacked ty arg'
      CopyPacked ty arg -> do
        arg' <- freshConstrsInExp arg constrenv
        return $ Ext $ CopyPacked ty arg'
      TravPacked ty arg -> do
        arg' <- freshConstrsInExp arg constrenv
        return $ Ext $ TravPacked ty arg'
      L p e -> do
        e' <- freshConstrsInExp e constrenv
        return $ Ext $ L p e'
      LinearExt a -> do
        return $ Ext $ LinearExt a

-- 

resolveNameInEnv :: Var -> VarEnv -> Var
resolveNameInEnv name e = 
  do case (M.lookup name e) of
      --Just freshName -> (toVar ((fromVar freshName) ++ "*"))
      Just freshName -> freshName
      Nothing -> error $ "can't find " ++ (fromVar name) ++ " in constructor env: " ++ (show e)