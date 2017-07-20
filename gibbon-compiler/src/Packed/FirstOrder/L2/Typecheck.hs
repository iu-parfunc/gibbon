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
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Packed.FirstOrder.L2.Typecheck
    ( tcExp, tcProg )
    where

import Control.DeepSeq
import Packed.FirstOrder.Common
import Packed.FirstOrder.L2.Syntax as L2
import qualified Packed.FirstOrder.L1.Syntax as L1
import Data.Set as S
import Data.Map as M
import Data.List as L
import Text.PrettyPrint.GenericPretty
import Control.Monad.Except

newtype ConstraintSet = ConstraintSet { constraintSet :: S.Set LocExp }
    
type Aliased = Bool
    
newtype LocationTypeState = LocationTypeState { tsmap :: M.Map LocVar (Modality,Aliased) }
    deriving (Read,Show,Eq,Ord, Generic, NFData)
    
newtype RegionSet = RegionSet { regSet :: S.Set Region }


data TCError = GenericTC String Exp
             | VarNotFoundTC Var Exp
             | UnsupportedExpTC Exp
             | DivergingEffectsTC Exp LocationTypeState LocationTypeState
             | LocationTC String Exp LocVar LocVar 
               deriving (Read,Show,Eq,Ord, Generic, NFData)
    
type TcM a = Except TCError a


tcExp :: DDefs Ty -> Env2 Ty -> NewFuns
      -> ConstraintSet -> RegionSet -> LocationTypeState -> Exp
      -> TcM (Ty, LocationTypeState)
tcExp ddfs env funs constrs regs tstatein exp =
    case exp of
      VarE v -> do
               ty <- lookupVar env v exp
               return (ty, tstatein)
                      
      LitE i -> return (IntTy, tstatein)
                
      LitSymE v -> return (IntTy, tstatein) -- SymTy
                   
      AppE v ls e ->
          do let (ArrowTy locVars arrIn arrEffs arrOut locRets) = getFunTy funs v
             (ty,tstate) <- recur tstatein e
             ensureEqualTy exp ty arrIn
             -- TODO: update tstate with traversals
             return (arrOut,tstate)
                    
      PrimAppE pr es -> do
               (tys,tstate) <- tcExps ddfs env funs constrs regs tstatein es
               -- TODO: check argument length
               case pr of
                 L1.AddP -> do ensureEqualTy exp IntTy (tys !! 0)
                               ensureEqualTy exp IntTy (tys !! 1)
                               return $ (IntTy,tstate)
                 L1.SubP -> do ensureEqualTy exp IntTy (tys !! 0)
                               ensureEqualTy exp IntTy (tys !! 1)
                               return $ (IntTy,tstate)
                 L1.MulP -> do ensureEqualTy exp IntTy (tys !! 0)
                               ensureEqualTy exp IntTy (tys !! 1)
                               return $ (IntTy,tstate)
                 L1.EqSymP -> do ensureEqualTy exp IntTy (tys !! 0)
                                 ensureEqualTy exp IntTy (tys !! 1)
                                 return $ (IntTy,tstate)
                 L1.EqIntP -> do ensureEqualTy exp IntTy (tys !! 0)
                                 ensureEqualTy exp IntTy (tys !! 1)
                                 return $ (IntTy,tstate)
                 L1.MkTrue -> return $ (BoolTy,tstate)
                 L1.MkFalse -> return $ (BoolTy,tstate)
                 -- TODO: add rest of primops
                 _ -> throwError $ UnsupportedExpTC exp
                      
      LetE (v,_ls,ty,e1) e2 -> do
               (ty1,tstate1) <- recur tstatein e1
               ensureEqualTy exp ty1 ty
               let env' = extendEnv env v ty
               tcExp ddfs env' funs constrs regs tstate1 e2
                     
      IfE e1 e2 e3 -> do
               (ty1,tstate1) <- recur tstatein e1
               ensureEqualTy exp ty1 BoolTy
               (ty2,tstate2) <- recur tstate1 e2
               (ty3,tstate3) <- recur tstate1 e3
               tstate <- combineTStates exp tstate2 tstate3
               ensureEqualTy exp ty2 ty3
               return (ty2,tstate)
               
      MkProdE es -> do
               (tys,tstate) <- tcExps ddfs env funs constrs regs tstatein es
               return (ProdTy tys,tstate)
                      
      ProjE i e -> do
               (ty,tstate) <- recur tstatein e
               tyi <- tcProj exp i ty
               return (tyi, tstate)
                      
      CaseE e brs -> do
               (ty,tstate) <- recur tstatein e
               ensureMatchCases ddfs exp ty brs
               (tys,tstate') <- tcCases ddfs env funs constrs regs tstate brs
               foldM_ (ensureEqualTy exp) (tys !! 0) (tail tys)
               return (tys !! 0,tstate')
                      
      DataConE l dc es -> do
               (tys,tstate1) <- tcExps ddfs env funs constrs regs tstatein es
               let dcty = getTyOfDataCon ddfs dc
               let args = lookupDataCon ddfs dc
               if length args /= length es
               then throwError $ GenericTC "Invalid argument length" exp
               else do
                 sequence_ [ ensureEqualTyNoLoc exp ty1 ty2
                           | (ty1,ty2) <- zip args tys ]
                 ensureDataCon exp l tys constrs 
                 tstate2 <- switchOutLoc exp tstate1 l
                 return (PackedTy dcty l, tstate2)
                        
      TimeIt e _ty _b -> do
               (ty1,tstate1) <- recur tstatein e
               -- ensureEqualTy exp ty ty1
               return (ty1,tstate1)
                      
      MapE _ _ -> throwError $ UnsupportedExpTC exp
                  
      FoldE _ _ _ -> throwError $ UnsupportedExpTC exp
                     
      Ext (LetRegionE r e) -> do
               regs' <- regionInsert exp r regs
               (ty,tstate) <- tcExp ddfs env funs constrs regs' tstatein e
               case ty of
                 PackedTy _con l -> do
                                r <- getRegion exp constrs l
                                if hasRegion r regs
                                then throwError $ GenericTC ("Escaping region " ++ (show r)) exp
                                else return (ty,tstate)
                 _ -> return (ty,tstate)
                      
      Ext (LetLocE v c e) -> do
               case c of
                 
                 StartOfC l r -> do
                               if l /= v then throwError $ GenericTC "Invalid location binding" exp
                               else do 
                                 ensureRegion exp r regs
                                 absentStart exp constrs l
                                 let tstate1 = extendTS v (Output,False) tstatein
                                 let constrs1 = extendConstrs (StartOfC v r) $
                                                extendConstrs (InRegionC l r) constrs
                                 (ty,tstate2) <- tcExp ddfs env funs constrs1 regs tstate1 e
                                 tstate3 <- removeLoc exp tstate2 v
                                 return (ty,tstate3)
                                        
                 AfterConstantC i l1 l2 -> do
                               if l2 /= v then throwError $ GenericTC "Invalid location binding" exp
                               else do 
                                 r <- getRegion exp constrs l1
                                 absentStart exp constrs v
                                 let tstate1 = extendTS v (Output,True) tstatein
                                 let constrs1 = extendConstrs (InRegionC l2 r) $
                                                extendConstrs (AfterConstantC i l1 l2) constrs
                                 (ty,tstate2) <- tcExp ddfs env funs constrs1 regs tstate1 e
                                 tstate3 <- removeLoc exp tstate2 v
                                 return (ty,tstate3)
                                        
                 AfterVariableC x l1 l2 -> do
                               if l2 /= v then throwError $ GenericTC "Invalid location binding" exp
                               else do
                                 r <- getRegion exp constrs l1
                                 absentStart exp constrs v
                                 (xty,tstate1) <- tcExp ddfs env funs constrs regs tstatein $ VarE x
                                 ensurePackedLoc exp xty l1
                                 let tstate2 = extendTS v (Output,True) tstate1
                                 let constrs1 = extendConstrs (InRegionC l2 r) $
                                                extendConstrs (AfterVariableC x l1 l2) constrs
                                 (ty,tstate3) <- tcExp ddfs env funs constrs1 regs tstate2 e
                                 tstate4 <- removeLoc exp tstate3 v
                                 return (ty,tstate4)
                                        
                 _ -> throwError $ GenericTC "Invalid letloc form" exp
                 
      Ext (RetE _ls v) -> do
               -- skip returned locations for now
               recur tstatein $ VarE v

    where recur ts e = tcExp ddfs env funs constrs regs ts e

tcCases :: DDefs Ty -> Env2 Ty -> NewFuns
        -> ConstraintSet -> RegionSet -> LocationTypeState -> [(DataCon, [(Var,LocVar)], Exp)]
        -> TcM ([Ty], LocationTypeState)
tcCases ddfs env funs constrs regs tstatein ((dc, vs, e):cases) = undefined -- TODO: Typecheck cases
tcCases _ _ _ _ _ ts [] = return ([],ts)
         
tcProj :: Exp -> Int -> Ty -> TcM Ty
tcProj _ i (ProdTy tys) = return $ tys !! i
tcProj e i ty = throwError $ GenericTC ("Projection from non-tuple type " ++ (show ty)) e

tcExps :: DDefs Ty -> Env2 Ty -> NewFuns
      -> ConstraintSet -> RegionSet -> LocationTypeState -> [Exp]
      -> TcM ([Ty], LocationTypeState)
tcExps ddfs env funs constrs regs tstatein (exp:exps) =
    do (ty,ts) <- tcExp ddfs env funs constrs regs tstatein exp
       (tys,ts') <- tcExps ddfs env funs constrs regs ts exps
       return (ty:tys,ts')
tcExps _ _ _ _ _ ts [] = return ([],ts)

tcProg = undefined

--------------------------------------------------------------------------------------------

regionInsert :: Exp -> Region -> RegionSet -> TcM RegionSet
regionInsert e r (RegionSet regSet) = do
  if (S.member r regSet)
  then throwError $ GenericTC "Shadowed regions not allowed" e
  else return $ RegionSet (S.insert r regSet)

hasRegion :: Region -> RegionSet -> Bool
hasRegion r (RegionSet regSet) = S.member r regSet

ensureRegion :: Exp -> Region -> RegionSet -> TcM ()
ensureRegion exp r (RegionSet regSet) =
    if S.member r regSet then return ()
    else throwError $ GenericTC ("Region " ++ (show r) ++ " not in scope") exp

getRegion :: Exp -> ConstraintSet -> LocVar -> TcM Region
getRegion exp (ConstraintSet cs) l = go $ S.toList cs
    where go ((InRegionC l1 r):cs) = if l1 == l then return r
                                     else go cs
          go (_:cs) = go cs
          go [] = throwError $ GenericTC ("Location " ++ (show l) ++ " has no region") exp


lookupVar :: Env2 Ty -> Var -> Exp -> TcM Ty
lookupVar env var exp =
    case M.lookup var $ vEnv env of
      Nothing -> throwError $ VarNotFoundTC var exp
      Just ty -> return ty

combineTStates :: Exp -> LocationTypeState -> LocationTypeState -> TcM LocationTypeState
combineTStates exp ts1 ts2 = if ts1 == ts2 then return ts1
                             else throwError $ DivergingEffectsTC exp ts1 ts2

ensureEqual :: Eq a => Exp -> String -> a -> a -> TcM a
ensureEqual exp str a b = if a == b then return a else throwError $ GenericTC str exp

ensureEqualTy :: Exp -> Ty -> Ty -> TcM Ty
ensureEqualTy exp a b = ensureEqual exp ("Expected these types to be the same: "
                                         ++ (show a) ++ ", " ++ (show b)) a b

ensureEqualTyNoLoc :: Exp -> Ty -> Ty -> TcM Ty
ensureEqualTyNoLoc exp (PackedTy dc1 ty1) (PackedTy dc2 ty2) =
    if dc1 == dc2 then return (PackedTy dc1 ty1)
    else ensureEqualTy exp (PackedTy dc1 ty1) (PackedTy dc2 ty2)
ensureEqualTyNoLoc exp ty1 ty2 = ensureEqualTy exp ty1 ty2

ensureMatchCases :: DDefs Ty -> Exp -> Ty -> [(DataCon, [(Var,LocVar)], Exp)] -> TcM ()
ensureMatchCases ddfs exp ty cs = do
  case ty of
    PackedTy tc _l -> do
            let cons = S.fromList $ L.map fst $ dataCons $ lookupDDef ddfs $ toVar tc
            forM cs $ \(dc,_,_) ->
                do if S.member dc cons
                   then return ()
                   else throwError $ GenericTC "Invalid case statement" exp 
            return ()
    _ -> throwError $ GenericTC "Cannot case on non-packed type" exp

ensurePackedLoc :: Exp -> Ty -> LocVar -> TcM ()
ensurePackedLoc exp ty l =
    case ty of
      PackedTy _ l1 -> if l1 == l then return ()
                       else throwError $ GenericTC ("Wrong location in type " ++ (show ty)) exp
      _ -> throwError $ GenericTC "Expected a packed type" exp

ensureDataCon :: Exp -> LocVar -> [Ty] -> ConstraintSet -> TcM ()
ensureDataCon exp linit tys cs = go Nothing linit tys
    where go Nothing linit ((PackedTy dc l):tys) = do
            ensureAfterConstant exp cs linit l
            go (Just (PackedTy dc l)) l tys
          go Nothing linit (ty:tys) = go Nothing linit tys
          go (Just (PackedTy dc1 l1)) linit ((PackedTy dc2 l2):tys) = do
            ensureAfterPacked exp cs l1 l2 
            go (Just (PackedTy dc2 l2)) l2 tys
          go (Just (PackedTy dc l1)) linit (ty:tys) =
              go Nothing linit tys
          go _ _ [] = return ()
            

ensureAfterConstant :: Exp -> ConstraintSet -> LocVar -> LocVar -> TcM ()
ensureAfterConstant exp (ConstraintSet cs) l1 l2 =
    if L.any f $ S.toList cs then return ()
    else throwError $ LocationTC "Expected after relationship" exp l1 l2 
    where f (AfterConstantC _i l1' l2') = l1' == l1 && l2' == l2
          f _ = False

ensureAfterPacked :: Exp -> ConstraintSet -> LocVar -> LocVar -> TcM ()
ensureAfterPacked  exp (ConstraintSet cs) l1 l2 =
    if L.any f $ S.toList cs then return ()
    else throwError $ LocationTC "Expected after relationship" exp l1 l2 
    where f (AfterVariableC _v l1' l2') = l1' == l1 && l2' == l2
          f _ = False
                                               
extendEnv :: Env2 Ty -> Var -> Ty -> Env2 Ty
extendEnv (Env2 vEnv fEnv) v ty = Env2 (M.insert v ty vEnv) fEnv

extendTS
  :: LocVar
     -> (Modality, Aliased) -> LocationTypeState -> LocationTypeState
extendTS v d (LocationTypeState ls) = LocationTypeState $ M.insert v d ls
                                      
extendConstrs :: LocExp -> ConstraintSet -> ConstraintSet
extendConstrs c (ConstraintSet cs) = ConstraintSet $ S.insert c cs

switchOutLoc :: Exp -> LocationTypeState -> LocVar -> TcM LocationTypeState
switchOutLoc exp (LocationTypeState ls) l =
    case M.lookup l ls of
      Nothing -> throwError $ GenericTC ("Unknown location " ++ (show l)) exp
      Just (Output,a) -> return $ LocationTypeState $ M.update (\_ -> Just (Input,a)) l ls
      Just (Input,_a) -> throwError $ GenericTC ("Expected output location " ++ (show l)) exp

absentAfter :: Exp -> LocationTypeState -> LocVar -> TcM ()
absentAfter exp (LocationTypeState ls) l =
    case M.lookup l ls of
      Nothing -> throwError $ GenericTC ("Unknown location " ++ (show l)) exp
      Just (_m,False) -> return ()
      Just (_m,True) -> throwError $ GenericTC ("Alias of location " ++ (show l)) exp

absentStart :: Exp -> ConstraintSet -> LocVar -> TcM ()
absentStart exp (ConstraintSet cs) l = go $ S.toList cs
    where go ((StartOfC l1 r):cs) =
              if l1 == l
              then throwError $ GenericTC ("Repeated start of " ++ (show r)) exp
              else go cs
          go (_:cs) = go cs
          go [] = return ()

removeLoc :: Exp -> LocationTypeState -> LocVar -> TcM LocationTypeState
removeLoc exp (LocationTypeState ls) l =
    if M.member l ls
    then return $ LocationTypeState $ M.delete l ls
    else throwError $ GenericTC ("Cannot remove location " ++ (show l)) exp


    
--------------------------------------------------------------------------------------------

ddtree :: DDefs Ty
ddtree = (fromListDD [DDef (toVar "Tree") 
                              [ ("Leaf",[(False,IntTy)])
                              , ("Node",[(False,PackedTy "Tree" "l")
                                        ,(False,PackedTy "Tree" "l")])]])

tester' =
    let ddfs = ddtree
        env = Env2 M.empty M.empty
        funs = M.empty
        constrs = ConstraintSet $ S.empty
        regs = RegionSet $ S.empty
        tstate = LocationTypeState $ M.empty
    in tcExp ddfs env funs constrs regs tstate 

tester = runExcept . tester'

testerout e = 
    case tester $ e of
      Left err -> putStrLn (show err)
      Right (ty,tstate) -> putStrLn (show ty) >> putStrLn (show tstate)

test1 = testerout $ LitE 1 

test2 = testerout $ LetE ("a",[],IntTy,LitE 1) (PrimAppE L1.AddP [VarE "a",VarE "a"])
        
test3 = testerout $ Ext $ LetRegionE (VarR "r") $ Ext $ LetLocE "l" (StartOfC "l" (VarR "r")) $ LitE 1

test4 = testerout $ Ext $ LetRegionE (VarR "r") $ Ext $ LetLocE "l" (StartOfC "l" (VarR "r")) $
        LetE ("throwaway", [], PackedTy "Tree" "l", DataConE "l" "Leaf" [LitE 1]) $ LitE 2

test4bad1 = testerout $ Ext $ LetRegionE (VarR "r") $ Ext $ LetLocE "l" (StartOfC "l" (VarR "r1")) $
            LetE ("throwaway", [], PackedTy "Tree" "l", DataConE "l" "Leaf" [LitE 1]) $ LitE 2

test4bad2 = testerout $ Ext $ LetRegionE (VarR "r") $ Ext $ LetLocE "l" (StartOfC "l" (VarR "r")) $
            LetE ("throwaway", [], PackedTy "Tree" "l1", DataConE "l1" "Leaf" [LitE 1]) $ LitE 2

test5 = testerout $ Ext $ LetRegionE (VarR "r") $ Ext $ LetLocE "l" (StartOfC "l" (VarR "r")) $
        Ext $ LetLocE "l1" (AfterConstantC 1 "l" "l1") $ 
        LetE ("x", [], PackedTy "Tree" "l1", DataConE "l1" "Leaf" [LitE 1]) $
        Ext $ LetLocE "l2" (AfterVariableC "x" "l1" "l2") $
        LetE ("y", [], PackedTy "Tree" "l2", DataConE "l2" "Leaf" [LitE 2]) $
        LetE ("z", [], PackedTy "Tree" "l", DataConE "l" "Node" [VarE "x", VarE "y"]) $
        LitE 1

test5bad1 = testerout $ Ext $ LetRegionE (VarR "r") $ Ext $ LetLocE "l" (StartOfC "l" (VarR "r")) $
        Ext $ LetLocE "l1" (AfterConstantC 1 "l" "l1") $ 
        LetE ("x", [], PackedTy "Tree" "l1", DataConE "l1" "Leaf" [LitE 1]) $
        Ext $ LetLocE "l2" (AfterVariableC "x" "l1" "l2") $
        LetE ("y", [], PackedTy "Tree" "l2", DataConE "l2" "Leaf" [LitE 2]) $
        LetE ("z", [], PackedTy "Tree" "l", DataConE "l" "Node" [VarE "y", VarE "x"]) $
        LitE 1
-- GenericTC "Expected Var \"l2\" after Var \"l\"" (DataConE (Var "l") "Node" [VarE (Var "y"),VarE (Var "x")])
