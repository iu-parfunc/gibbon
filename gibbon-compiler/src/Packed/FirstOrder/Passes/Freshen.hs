-- {-# LANGUAGE OverloadedStrings #-}

-- | Unique names.

module Packed.FirstOrder.Passes.Freshen (freshNames) where

import Packed.FirstOrder.Common
import Control.Exception
import Packed.FirstOrder.L1_Source as L1
import qualified Data.Map as M
import qualified Data.List as L

genLocVar :: String -> SyM LocVar
genLocVar s = gensym (toVar ("l"++s))


-- FIXME: Naughty to use lists as maps.  Use something with O(N)
-- lookup.  We should standardize on a fast symbol-map.

-- | Rename all local variables.  Rename all dummy locations.
freshNames :: L1.Prog -> SyM L1.Prog
freshNames (L1.Prog defs funs main cstrs) =
    do main' <- case main of
                  Nothing -> return Nothing
                  Just (E1 m) -> do m' <- freshExp [] m
                                    return $ Just m'
       funs' <- freshFuns funs
       return $ L1.Prog defs funs' main' cstrs
    where freshFuns m = M.fromList <$> mapM freshFun (M.toList m)
          freshFun (nam, FunDef _ (narg,targ) ty (E1 bod)) =
              do narg' <- gensym narg
                 bod' <- freshExp [(narg,narg')] bod
                 let nam' = cleanFunName nam
                 return (nam', FunDef nam' (narg',targ) ty bod')

          freshExp :: [(Var,Var)] -> PreExp () Ty (E1 Ty) -> SyM L1.Exp
          freshExp _ (L1.RetE l v) = return (L1.E1 (L1.RetE l v))
              
          freshExp vs (L1.VarE v) =
              case lookup v vs of
                Nothing -> return $ L1.E1 $  L1.VarE v
                Just v' -> return $ L1.E1 $ L1.VarE v'
          freshExp _ (L1.LitE i) =
              return $ L1.E1 $ L1.LitE i
          freshExp _ (L1.LitSymE v) =
              return $ L1.E1 $ L1.LitSymE v
          freshExp vs (L1.AppE v ls (E1 e)) = assert ([] == ls) $ 
              do e' <- freshExp vs e
                 return $ L1.E1 $ L1.AppE (cleanFunName v) [] e'
          freshExp _ (L1.PrimAppE L1.Gensym []) =
              do v <- gensym (toVar "gensym")
                 return $ L1.E1 $ L1.LitSymE v
          freshExp vs (L1.PrimAppE p es) =
              do es' <- mapM (freshExp vs . fromE1) es
                 return $ L1.E1 $ L1.PrimAppE p es'
          freshExp vs (L1.LetE (v,ls,t, (E1 e1)) (E1 e2)) = assert ([]==ls) $ 
              do e1' <- freshExp vs e1
                 v' <- gensym v
                 e2' <- freshExp ((v,v'):vs) e2
                 return $ L1.E1 $ L1.LetE (v',[],t,e1') e2'
          freshExp vs (L1.IfE (E1 e1) (E1 e2) (E1 e3)) =
              do e1' <- freshExp vs e1
                 e2' <- freshExp vs e2
                 e3' <- freshExp vs e3
                 return $ L1.E1 $ L1.IfE e1' e2' e3'
          freshExp vs (L1.ProjE i (E1 e)) =
              do e' <- freshExp vs e
                 return $ L1.E1 $ L1.ProjE i e'
          freshExp vs (L1.MkProdE es) =
              do es' <- mapM (freshExp vs . fromE1) es
                 return $ L1.E1 $ L1.MkProdE es'
          freshExp vs (L1.CaseE (E1 e) mp) =
              do e' <- freshExp vs e
                 -- Here we freshen locations:
                 mp' <- mapM (\(c,prs,E1 ae) ->
                              let (args,locs) = unzip prs in
                              let [l] = L.nub locs in
                              assert (l == dummyLoc) $ do
                                locs' <- mapM (\_ -> genLocVar "") locs
                                args' <- mapM gensym args
                                let vs' = (zip args args') ++ vs
                                ae' <- freshExp vs' ae
                                return (c,zip args' locs',ae')) mp
                 return $ L1.E1 $ L1.CaseE e' mp'
          freshExp vs (L1.MkPackedE () c d es) =
              do es' <- mapM (freshExp vs . fromE1) es
                 case d of
                   Nothing -> return ()
                   Just x | x==dummyLoc -> return ()
                          | otherwise -> error$ "freshExp: expects only dummyLoc on input forms, found: "++show d
                 loc <- genLocVar ""
                 return $ L1.E1 $ L1.MkPackedE () c (fmap (\_ -> loc) d) es'
          freshExp vs (L1.TimeIt (E1 e) t b) =
              do e' <- freshExp vs e
                 return $ L1.E1 $ L1.TimeIt e' t b
          freshExp vs (L1.MapE (v,t,(E1 b)) (E1 e)) =
              do b' <- freshExp vs b
                 e' <- freshExp vs e
                 return $ L1.E1 $ L1.MapE (v,t,b') e'
          freshExp vs (L1.FoldE (v1,t1,E1 e1) (v2,t2,E1 e2) (E1 e3)) =
              do e1' <- freshExp vs e1
                 e2' <- freshExp vs e2
                 e3' <- freshExp vs e3
                 return $ L1.E1 $ L1.FoldE (v1,t1,e1') (v2,t2,e2') e3'
