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
    ( tcExp, tcProg, TCError(..), RegionSet(..), LocationTypeState(..), ConstraintSet(..), Aliased, TcM )
    where

import Control.DeepSeq
import Packed.FirstOrder.Common
import Packed.FirstOrder.L2.Syntax as L2
import qualified Packed.FirstOrder.L1.Syntax as L1
import Data.Set as S
import Data.Map as M
import Data.List as L
import Data.Maybe as Maybe
import Text.PrettyPrint.GenericPretty
import Control.Monad.Except
import Debug.Trace

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
             | ModalityTC String Exp LocVar LocationTypeState
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
             case ty of
               PackedTy _ tyl -> if S.member tyl $ S.fromList ls
                                 then return ()
                                 else throwError $ GenericTC ("Packed argument location expected: " ++ show tyl) exp
               _ -> return ()
             ensureEqualTyNoLoc exp ty arrIn
             let handleTS ts (l,Output) =  switchOutLoc exp ts l
                 handleTS ts _ = return ts
             tstate' <- foldM handleTS tstate $ zip ls $ L.map (\(LRM _ _ m) -> m) locVars
             return (arrOut,tstate')
                    
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
               ensureEqualTyNoLoc exp ty1 ty
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
               let PackedTy _dc lin = ty
               ensureMatchCases ddfs exp ty brs
               (tys,tstate') <- tcCases ddfs env funs constrs regs tstate lin brs
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
                                 let tstate1 = extendTS v (Output,True) $ setAfter l1 tstatein
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
                                 let tstate2 = extendTS v (Output,True) $ setAfter l1 tstate1
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
        -> ConstraintSet -> RegionSet -> LocationTypeState -> LocVar
        -> [(DataCon, [(Var,LocVar)], Exp)]
        -> TcM ([Ty], LocationTypeState)
tcCases ddfs env funs constrs regs tstatein lin ((dc, vs, e):cases) = do
  let argtys = zip vs $ lookupDataCon ddfs dc
      pairwise = zip argtys $ Nothing : (L.map Just argtys)
      genConstrs (((v1,l1),PackedTy _ _),Nothing) (lin,lst) =
          (l1,(AfterConstantC 1 lin l1) : lst)
      genConstrs (((v1,l1),PackedTy _ _),Just ((v2,l2),PackedTy _ _)) (lin,lst) =
          (l1,(AfterVariableC v2 l2 l1) : lst)
      genConstrs (((v1,l1),PackedTy _ _),Just _) (lin,lst) =
          (l1,(AfterConstantC undefined lin l1) : lst)
      genConstrs (_,_) (lin,lst) = (lin,lst)
      genTS ((v,l),PackedTy _ _) ts = extendTS l (Input,False) ts
      genTS _ ts = ts
      genEnv ((v,l),PackedTy dc l') env = extendEnv env v $ PackedTy dc l
      genEnv ((v,l),ty) env = extendEnv env v ty
      remTS ((v,l),PackedTy _ _) ts = removeTS l ts
      remTS _ ts = ts
                 
      constrs1 = L.foldr extendConstrs constrs $ snd $ L.foldr genConstrs (lin,[]) pairwise
      tstate1 = L.foldr genTS tstatein argtys
      env1 = L.foldr genEnv env argtys
  (ty1,tstate2) <- tcExp ddfs env1 funs constrs1 regs tstate1 e
  (tyRest,tstateRest) <- recur
  tstateCombine <- combineTStates e tstate2 tstateRest
  let tstatee' = L.foldr remTS tstateCombine argtys
  return (ty1:tyRest,tstatee')

    where recur = do
            (tys,tstate2) <- tcCases ddfs env funs constrs regs tstatein lin cases
            return (tys,tstate2)

tcCases _ _ _ _ _ ts _ [] = return ([],ts)
         
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

tcProg :: Prog -> SyM Prog
tcProg Prog{ddefs,fundefs,mainExp} = do

  mapM_ fd $ M.elems fundefs
  case mainExp of
    Nothing -> return ()
    Just (e,t) -> let res = runExcept $ tcExp ddefs (Env2 M.empty M.empty) fundefs
                            (ConstraintSet $ S.empty) (RegionSet $ S.empty)
                            (LocationTypeState $ M.empty) e
                  in case res of
                       Left err -> error $ show err
                       Right (t',_ts) -> if t' == t
                                         then return ()
                                         else error $ "Expected type " ++ (show t) ++ " and got type " ++ (show t')
  return $ Prog ddefs fundefs mainExp

  where

    fd :: L2.FunDef -> SyM ()
    fd L2.FunDef{funty,funarg,funbod} = do
        let env = extendEnv (Env2 M.empty M.empty) funarg (arrIn funty)
            constrs = funConstrs (locVars funty)
            regs = funRegs (locVars funty)
            tstate = funTState (locVars funty)
            res = runExcept $ tcExp ddefs env fundefs constrs regs tstate funbod
        case res of
          Left err -> error $ show err
          Right (ty,_) -> if ty == (arrOut funty)
                          then return ()
                          else error $ "Expected type " ++ (show (arrOut funty)) ++ " and got type " ++ (show ty)


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

funRegs :: [LRM] -> RegionSet
funRegs ((LRM _l r _m):lrms) =
    let (RegionSet rs) = funRegs lrms
    in RegionSet $ S.insert r rs
funRegs [] = RegionSet $ S.empty

funConstrs :: [LRM] -> ConstraintSet
funConstrs ((LRM l r _m):lrms) =
    extendConstrs (InRegionC l r) $ funConstrs lrms
funConstrs [] = ConstraintSet $ S.empty

funTState :: [LRM] -> LocationTypeState
funTState ((LRM l _r m):lrms) =
    extendTS l (m,False) $ funTState lrms
funTState [] = LocationTypeState $ M.empty

lookupVar :: Env2 Ty -> Var -> Exp -> TcM Ty
lookupVar env var exp =
    case M.lookup var $ vEnv env of
      Nothing -> throwError $ VarNotFoundTC var exp
      Just ty -> return ty

combineTStates :: Exp -> LocationTypeState -> LocationTypeState -> TcM LocationTypeState
combineTStates exp (LocationTypeState ts1) (LocationTypeState ts2) = return $ LocationTypeState $ M.union ts1 ts2
    -- throwError $ DivergingEffectsTC exp ts1 ts2

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

removeTS :: LocVar -> LocationTypeState -> LocationTypeState
removeTS l (LocationTypeState ls) = LocationTypeState $ M.delete l ls

setAfter :: LocVar -> LocationTypeState -> LocationTypeState
setAfter l (LocationTypeState ls) = LocationTypeState $ M.adjust (\(m,_) -> (m,True)) l ls

lookupTS :: Exp -> LocVar -> LocationTypeState -> TcM (Modality,Bool)
lookupTS exp l (LocationTypeState ls) =
    case M.lookup l ls of
      Nothing -> throwError $ GenericTC ("Failed lookup of location " ++ (show l)) exp
      Just d -> return d
                                      
extendConstrs :: LocExp -> ConstraintSet -> ConstraintSet
extendConstrs c (ConstraintSet cs) = ConstraintSet $ S.insert c cs

switchOutLoc :: Exp -> LocationTypeState -> LocVar -> TcM LocationTypeState
switchOutLoc exp (LocationTypeState ls) l =
    case M.lookup l ls of
      Nothing -> throwError $ GenericTC ("Unknown location " ++ (show l)) exp
      Just (Output,a) -> return $ LocationTypeState $ M.update (\_ -> Just (Input,a)) l ls
      Just (Input,_a) -> throwError $ ModalityTC "Expected output location" exp l $ LocationTypeState ls

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

test6 = testerout $ Ext $ LetRegionE (VarR "r") $ Ext $ LetLocE "l" (StartOfC "l" (VarR "r")) $
        Ext $ LetLocE "l1" (AfterConstantC 1 "l" "l1") $ 
        LetE ("x", [], PackedTy "Tree" "l1", DataConE "l1" "Leaf" [LitE 1]) $
        Ext $ LetLocE "l2" (AfterVariableC "x" "l1" "l2") $
        LetE ("y", [], PackedTy "Tree" "l2", DataConE "l2" "Leaf" [LitE 2]) $
        LetE ("z", [], PackedTy "Tree" "l", DataConE "l" "Node" [VarE "x", VarE "y"]) $
        CaseE (VarE "z") [ ("Leaf",[("num","lnum")], VarE "num")
                         , ("Node",[("x","lnodex"),("y","lnodey")], LitE 0)]


exadd1 :: L2.FunDef
exadd1 = L2.FunDef "add1" exadd1ty "tr" exadd1bod

exadd1ty :: ArrowTy Ty2
exadd1ty = (ArrowTy
            [LRM "lin" (VarR "r1") Input, LRM "lout" (VarR "r1") Output]
            (PackedTy "Tree" "lin")
            (S.fromList [Traverse "lin"])
            (PackedTy "Tree" "lout")
            [EndOf $ LRM "lin" (VarR "r1") Input])

exadd1bod :: Exp2
exadd1bod =
    CaseE (VarE "tr") $
      [ ("Leaf", [("n","l0")],
         LetE ("v",[],IntTy,PrimAppE L1.AddP [VarE "n", LitE 1]) $
         LetE ("lf",[],PackedTy "Tree" "lout", DataConE "lout" "Leaf" [VarE "v"]) $
         VarE "lf")
      , ("Node", [("x","l1"),("y","l2")],
         Ext $ LetLocE "lout1" (AfterConstantC 1 "lout" "lout1") $
         LetE ("x1",[],PackedTy "Tree" "lout1", AppE "add1" ["l1","lout1"] (VarE "x")) $
         Ext $ LetLocE "lout2" (AfterVariableC "x1" "lout1" "lout2") $
         LetE ("y1",[],PackedTy "Tree" "lout2", AppE "add1" ["l2","lout2"] (VarE "y")) $
         LetE ("z",[],PackedTy "Tree" "lout", 
                  DataConE "lout" "Node" [ VarE "x1" , VarE "y1"]) $
         VarE "z")
      ]

test7main = Ext $ LetRegionE (VarR "r") $ Ext $ LetLocE "l" (StartOfC "l" (VarR "r")) $
            Ext $ LetLocE "l1" (AfterConstantC 1 "l" "l1") $ 
            LetE ("x", [], PackedTy "Tree" "l1", DataConE "l1" "Leaf" [LitE 1]) $
            Ext $ LetLocE "l2" (AfterVariableC "x" "l1" "l2") $
            LetE ("y", [], PackedTy "Tree" "l2", DataConE "l2" "Leaf" [LitE 1]) $
            LetE ("z", [], PackedTy "Tree" "l", DataConE "l" "Node" [VarE "x", VarE "y"]) $
            Ext $ LetRegionE (VarR "rtest") $
            Ext $ LetLocE "testout" (StartOfC "testout" (VarR "rtest")) $
            LetE ("a", [], PackedTy "Tree" "testout", AppE "add1" ["l","testout"] (VarE "z")) $
            CaseE (VarE "a") [ ("Leaf",[("num","lnum")], VarE "num")
                             , ("Node",[("x","lnodex"),("y","lnodey")], LitE 0)]

test7prog = Prog ddtree (M.singleton "add1" exadd1) (Just (test7main,IntTy))

test7 = fst $ runSyM 0 $ tcProg test7prog

