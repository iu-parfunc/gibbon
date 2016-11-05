-- | Unique names.

module Packed.FirstOrder.Passes.Freshen (freshNames) where

import Packed.FirstOrder.Common
import qualified Packed.FirstOrder.L1_Source as L1
    
-- | Rename all local variables
freshNames :: L1.Prog -> SyM L1.Prog
freshNames (L1.Prog defs funs main) =
    do main' <- case main of
                  Nothing -> return Nothing
                  Just m -> do m' <- freshExp [] m
                               return $ Just m'
       funs' <- freshFuns funs
       return $ L1.Prog defs funs' main'
    where freshFuns = mapM freshFun
          freshFun (FunDef nam (narg,targ) ty bod) =
              do narg' <- gensym narg
                 bod' <- freshExp [(narg,narg')] bod
                 return $ FunDef nam (narg',targ) ty bod'

          freshExp :: [(Var,Var)] -> L1.Exp -> SyM L1.Exp
          freshExp vs (L1.VarE v) =
              case lookup v vs of
                Nothing -> return $ L1.VarE v
                Just v' -> return $ L1.VarE v'
          freshExp _ (L1.LitE i) =
              return $ L1.LitE i
          freshExp vs (L1.AppE v e) =
              do e' <- freshExp vs e
                 return $ L1.AppE v e'
          freshExp vs (L1.PrimAppE p es) =
              do es' <- mapM (freshExp vs) es
                 return $ L1.PrimAppE p es'
          freshExp vs (L1.LetE (v,t,e1) e2) =
              do e1' <- freshExp vs e1
                 v' <- gensym v
                 e2' <- freshExp ((v,v'):vs) e2
                 return $ L1.LetE (v',t,e1') e2'
          freshExp vs (L1.IfE e1 e2 e3) =
              do e1' <- freshExp vs e1
                 e2' <- freshExp vs e2
                 e3' <- freshExp vs e3
                 return $ L1.IfE e1' e2' e3'
          freshExp vs (L1.ProjE i e) =
              do e' <- freshExp vs e
                 return $ L1.ProjE i e'
          freshExp vs (L1.MkProdE es) =
              do es' <- mapM (freshExp vs) es
                 return $ L1.MkProdE es'
          freshExp vs (L1.CaseE e mp) =
              do e' <- freshExp vs e
                 mp' <- mapM (\(c,args,ae) -> do
                                args' <- mapM gensym args
                                let vs' = (zip args args') ++ vs
                                ae' <- freshExp vs' ae
                                return (c,args',ae')) mp
                 return $ L1.CaseE e' mp'
          freshExp vs (L1.MkPackedE c es) =
              do es' <- mapM (freshExp vs) es
                 return $ L1.MkPackedE c es'
          freshExp vs (L1.TimeIt e t) =
              do e' <- freshExp vs e
                 return $ L1.TimeIt e' t
          freshExp vs (L1.MapE (v,t,b) e) =
              do b' <- freshExp vs b
                 e' <- freshExp vs e
                 return $ L1.MapE (v,t,b') e'
          freshExp vs (L1.FoldE (v1,t1,e1) (v2,t2,e2) e3) =
              do e1' <- freshExp vs e1
                 e2' <- freshExp vs e2
                 e3' <- freshExp vs e3
                 return $ L1.FoldE (v1,t1,e1') (v2,t2,e2') e3'

