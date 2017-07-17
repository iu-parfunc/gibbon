-- {-# LANGUAGE OverloadedStrings #-}

-- | Unique names.

module Packed.FirstOrder.Passes.Freshen (freshNames) where

import Packed.FirstOrder.Common
import Control.Exception
import Packed.FirstOrder.L1.Syntax as L1
import qualified Data.Map as M
import qualified Data.List as L


-- FIXME: Naughty to use lists as maps.  Use something with O(N)
-- lookup.  We should standardize on a fast symbol-map.

-- | Rename all local variables.
freshNames :: L1.Prog -> SyM L1.Prog
freshNames (L1.Prog defs funs main) =
    do main' <- case main of
                  Nothing -> return Nothing
                  Just m -> do m' <- freshExp [] m
                               return $ Just m'
       funs' <- freshFuns funs
       return $ L1.Prog defs funs' main' 
    where freshFuns m = M.fromList <$> mapM freshFun (M.toList m)
          freshFun (nam, FunDef _ (narg,targ) ty bod) =
              do narg' <- gensym narg
                 bod' <- freshExp [(narg,narg')] bod
                 let nam' = cleanFunName nam
                 return (nam', FunDef nam' (narg',targ) ty bod')

          freshExp :: [(Var,Var)] -> PreExp () () Ty -> SyM L1.Exp
          freshExp _ (L1.Ext ()) = return (L1.Ext ())

          freshExp vs (L1.VarE v) =
              case lookup v vs of
                Nothing -> return $ L1.VarE v
                Just v' -> return $ L1.VarE v'
          freshExp _ (L1.LitE i) =
              return $ L1.LitE i
          freshExp _ (L1.LitSymE v) =
              return $ L1.LitSymE v
          freshExp vs (L1.AppE v ls e) = assert ([] == ls) $ 
              do e' <- freshExp vs e
                 return $ L1.AppE (cleanFunName v) [] e'
          freshExp _ (L1.PrimAppE L1.Gensym []) =
              do v <- gensym (toVar "gensym")
                 return $ L1.LitSymE v
          freshExp vs (L1.PrimAppE p es) =
              do es' <- mapM (freshExp vs) es
                 return $ L1.PrimAppE p es'
          freshExp vs (L1.LetE (v,ls,t, e1) e2) = assert ([]==ls) $ 
              do e1' <- freshExp vs e1
                 v' <- gensym v
                 e2' <- freshExp ((v,v'):vs) e2
                 return $ L1.LetE (v',[],t,e1') e2'
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
                 -- Here we freshen locations:
                 mp' <- mapM (\(c,prs,ae) ->
                              let (args,_) = unzip prs in
                              do
                                args' <- mapM gensym args
                                let vs' = (zip args args') ++ vs
                                ae' <- freshExp vs' ae
                                return (c, L.map (,()) args', ae')) mp
                 return $ L1.CaseE e' mp'
          freshExp vs (L1.DataConE () c es) =
              do es' <- mapM (freshExp vs) es
                 return $ L1.DataConE () c es'
          freshExp vs (L1.TimeIt e t b) =
              do e' <- freshExp vs e
                 return $ L1.TimeIt e' t b
          freshExp vs (L1.MapE (v,t,b) e) =
              do b' <- freshExp vs b
                 e' <- freshExp vs e
                 return $ L1.MapE (v,t,b') e'
          freshExp vs (L1.FoldE (v1,t1,e1) (v2,t2,e2) e3) =
              do e1' <- freshExp vs e1
                 e2' <- freshExp vs e2
                 e3' <- freshExp vs e3
                 return $ L1.FoldE (v1,t1,e1') (v2,t2,e2') e3'
