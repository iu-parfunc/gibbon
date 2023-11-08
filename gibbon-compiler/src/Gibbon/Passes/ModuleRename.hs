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

type VarEnv     = M.Map Var Var
type TyVarEnv t = M.Map TyVar t

moduleRename :: Prog0 -> PassM Prog0
moduleRename (Prog defs funs main) =
    let _ = dbgPrintLn 2 (show defs)
    in
    do --main' <- case main of
       --           Nothing -> return Nothing
       --           Just (m,ty) -> do m' <- freshExp M.empty M.empty m
       --                             return $ Just (m',ty)
       --defs' <- traverse freshDDef defs
       --funs' <- *traverse map*  resolveFunModuleNames funs
       funs' <- traverse (\v -> resolveModsInFuns v defs funs) funs
       return $ Prog defs funs' main

freshDDef :: DDef Ty0 -> PassM (DDef Ty0)
freshDDef DDef{tyName,tyArgs,dataCons} = 
  do pure $ DDef tyName tyArgs dataCons

resolveModsInFuns :: FunDef Exp0 -> DDefs Ty0 -> FunDefs Exp0 -> PassM (FunDef Exp0)
resolveModsInFuns (FunDef nam nargs funty bod meta) defs funs =
    do 
      bod' <- resolveModInBod funs bod
      pure $ FunDef nam nargs funty bod' meta

resolveModInBod :: FunDefs Exp0 -> Exp0 -> PassM Exp0
resolveModInBod funs exp =
  case exp of
    LitE i    -> return $ LitE i
    CharE c   -> return $ CharE c
    FloatE i  -> return $ FloatE i
    LitSymE v -> return $ LitSymE v
    --VarE v -> return $ VarE (varAppend (toVar "seen-") v)
    VarE v -> return $ VarE v
    AppE v locs ls -> return $ AppE (varAppend (toVar "timmy-") v) locs ls
    PrimAppE p es -> return $ PrimAppE p es

    LetE (v,_locs,ty, e1) e2 -> do
      e1' <- resolveModInBod funs e1
      e2' <- resolveModInBod funs e2
      return $ LetE (v, [], ty, e1') e2'

    IfE e1 e2 e3 -> return $ IfE e1 e2 e3
    ProjE i e -> return $ ProjE i e
    MkProdE es -> return $ MkProdE es

    CaseE e mp -> do
      e' <- resolveModInBod funs e
      mp' <- mapM (\(c,prs,ae) -> do
                     ae' <- resolveModInBod funs ae
                     return (c, prs, ae')) mp
      return $ CaseE e' mp'

    DataConE loc c es -> do
      es' <- traverse (\v -> resolveModInBod funs v) es
      return $ DataConE loc c es'

    TimeIt e t b -> return $ TimeIt e t b
    WithArenaE v e -> return $ WithArenaE v e
    SpawnE v locs ls -> return $ SpawnE v locs ls
    SyncE -> return $ SyncE
    MapE v e -> return $ MapE v e
    FoldE e1 e2 e3 -> return $ FoldE e1 e2 e3
    Ext ext -> return $ Ext ext
