{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Packed.FirstOrder.Passes.RouteEnds2
    ( routeEnds ) where

import Packed.FirstOrder.Common
import Packed.FirstOrder.L2.Syntax as L2
import qualified Packed.FirstOrder.L1.Syntax as L1
import Data.Set as S
import Data.Map as M
import Data.List as L
import Control.Monad 

-- | Chatter level for this module:
lvl :: Int
lvl = 4


data EndOfRel = EndOfRel { endOf :: M.Map LocVar LocVar, equivTo :: M.Map LocVar LocVar }

emptyRel :: EndOfRel
emptyRel = EndOfRel M.empty M.empty

mkEqual :: LocVar -> LocVar -> EndOfRel -> EndOfRel
mkEqual l1 l2 EndOfRel{endOf,equivTo} =
    EndOfRel{endOf=endOf,equivTo=M.insert l1 l2 equivTo}

mkEnd :: LocVar -> LocVar -> EndOfRel -> EndOfRel
mkEnd l1 l2 EndOfRel{endOf,equivTo} =
    EndOfRel{endOf=M.insert l1 l2 endOf,equivTo=equivTo}

findEnd :: LocVar -> EndOfRel -> LocVar
findEnd l EndOfRel{endOf,equivTo} =
    case M.lookup l endOf of
      Nothing -> case M.lookup l equivTo of
                   Nothing -> error $ "Failed finding the end of " ++ (show l)
                   Just leq -> findEnd leq EndOfRel{endOf,equivTo}
      Just lend -> lend

routeEnds :: Prog -> SyM Prog
routeEnds Prog{ddefs,fundefs,mainExp} = do
  fds' <- mapM fd $ M.elems fundefs
  let fundefs' = M.fromList $ L.map (\f -> (funname f,f)) fds'
  mainExp' <- case mainExp of
                Nothing -> return Nothing
                Just (e,t) -> do e' <- exp fundefs' [] emptyRel M.empty M.empty e
                                 return $ Just (e',t)
  return $ Prog ddefs fundefs' mainExp'

  where
    
    fd :: L2.FunDef -> SyM L2.FunDef 
    fd = undefined


    exp :: NewFuns -> [LocVar] -> EndOfRel -> M.Map Var LocVar -> M.Map LocVar LocVar -> Exp2 -> SyM Exp2
    exp fns retlocs eor lenv afterenv e =
        case e of
          
          VarE v -> mkRet retlocs $ VarE v

          LitE i -> return $ LitE i

          LitSymE v -> return $ LitSymE v

          AppE v ls e -> do
                 v' <- gensym "tailapp"
                 let ty = arrOut $ funtype v
                     e' = LetE (v',[],ty,AppE v ls e) (VarE v')
                 exp fns retlocs eor lenv afterenv e'
                 
          PrimAppE _pr _es -> error $ "Found complex expression in tail: " ++ (show e)

          LetE (v,_ls,ty,(AppE f lsin e1)) e2 -> do
                 let fty = funtype v
                     rets = S.fromList $ locRets fty
                     travlist = zip lsin $ L.map (\l -> S.member (EndOf l) rets)  (locVars fty)
                     lenv' = case ty of
                               PackedTy _n l -> M.insert v l lenv
                               _ -> lenv

                 let handleTravList lst (_l,False) = return lst
                     handleTravList lst (l,True) = gensym "endof" >>= \l' -> return $ (l,l'):lst

                 let mkEor (l1,l2) eor = mkEnd l1 l2 eor

                 newls <- foldM handleTravList [] travlist
                 let eor' = L.foldr mkEor eor newls
                 let outlocs = L.map snd newls
                 e2' <- exp fns retlocs eor' lenv' afterenv e2
                 return $ LetE (v,outlocs,ty,AppE f lsin e1) e2'

          LetE (v,ls,PackedTy n l,e1) e2 -> do
                 e2' <- exp fns retlocs eor (M.insert v l lenv) afterenv e2
                 return $ LetE (v,ls,PackedTy n l,e1) e2'

          LetE (v,ls,ty,e1) e2 -> do
                 e2' <- exp fns retlocs eor lenv afterenv e2
                 return $ LetE (v,ls,ty,e1) e2'

          IfE e1 e2 e3 -> do
                 e2' <- exp fns retlocs eor lenv afterenv e2
                 e3' <- exp fns retlocs eor lenv afterenv e3
                 return $ IfE e1 e2' e3'

          MkProdE _es -> error $ "Found complex expression in tail: " ++ (show e)

          ProjE _i _e -> error $ "Found complex expression in tail: " ++ (show e)

          CaseE (VarE x) brs -> do
                 brs' <- forM brs $ \(dc, vls, e) -> do
                                          let need = snd $ last vls
                                              lx = case M.lookup x lenv of
                                                     Nothing -> error $ "Failed to find " ++ (show x)
                                                     Just l -> l
                                              eor' = mkEqual lx need eor
                                              f (l1,l2) env = M.insert l1 l2 env
                                              afterenv' = L.foldr f afterenv $ zip (L.map snd vls) (tail $ L.map snd vls)
                                          e' <- exp fns retlocs eor' lenv afterenv' e
                                          return (dc, vls, e')
                 return $ CaseE (VarE x) brs'

          DataConE _l _dc _es -> error $ "Found complex expression in tail: " ++ (show e)

          TimeIt e ty b -> do
                 e' <- exp fns retlocs eor lenv afterenv e
                 return $ TimeIt e' ty b

          Ext (LetRegionE r e) -> do
                 e' <- exp fns retlocs eor lenv afterenv e
                 return $ Ext (LetRegionE r e')

          Ext (LetLocE v (StartOfC l r) e) -> do
                 e' <- exp fns retlocs eor lenv afterenv e
                 return $ Ext (LetLocE v (StartOfC l r) e')

          Ext (LetLocE v (AfterConstantC i l1 l2) e) -> do
                 e' <- exp fns retlocs eor lenv afterenv e
                 return $ Ext (LetLocE v (AfterConstantC i l1 l2) e')

          Ext (LetLocE v (AfterVariableC x l1 l2) e) -> do
                 e' <- exp fns retlocs eor lenv afterenv e
                 return $ Ext (LetLocE v (AfterVariableC x l1 l2) e')

          _ -> error $ "Unsupported expression: " ++ (show e)


        where  mkRet :: [LocVar] -> Exp2 -> SyM Exp2
               mkRet ls (VarE v) = let ends = L.map (\l -> findEnd l eor) ls
                                   in return $ Ext (RetE ends v)
               mkRet _ e = error $ "Expected variable reference in tail call, got " ++ (show e)

               funtype :: Var -> ArrowTy Ty2
               funtype v = case M.lookup v fns of
                             Nothing -> error $ "Function " ++ (show v) ++ " not found"
                             Just fundef -> funty fundef

ddtree :: DDefs Ty
ddtree = (fromListDD [DDef (toVar "Tree") 
                              [ ("Leaf",[(False,IntTy)])
                              , ("Node",[(False,PackedTy "Tree" "l")
                                        ,(False,PackedTy "Tree" "l")])]])

tester' :: Exp -> Prog
tester' e =
    let ddfs = ddtree
        funs = M.empty
    in Prog ddfs funs (Just (e,IntTy))

tester :: Exp -> Exp
tester e =
    let p = tester' e
    in case mainExp $ fst $ runSyM 1 $ routeEnds p of
         Nothing -> error "tester"
         Just e -> fst e
