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
import Data.Either
import Text.PrettyPrint.GenericPretty
import Control.Monad.Except

-- data Constr = StartOfC Loc Reg
--             | After1C Int Loc Loc
--             | AfterXC Var Loc Loc
--             | InC Loc Reg
--               deriving (Read,Show,Eq,Ord, Generic, NFData)
                       
-- data LocState = InLS Loc
--               | OutLS Loc
--               | AfterLS Loc
--               | StartS Reg
--                 deriving (Read,Show,Eq,Ord, Generic, NFData)

-- --- TODO: finish typechecker
-- --- does it need to be monadic, or can it be pure?
-- typeofE :: DDefs Ptype -> (Map Var Ftype) -> (Set Constr) -> (Set Reg) -> (Set LocState) -> Exp ->
--            (Ftype,Set LocState)
-- typeofE dd g c r ls exp =

newtype ConstraintSet = ConstraintSet { constraintSet :: S.Set LocExp }
    
type Aliased = Bool
    
newtype LocationTypeState = LocationTypeState { tsmap :: M.Map LocVar (Modality,Aliased) }
    deriving (Read,Show,Eq,Ord, Generic, NFData)
    
newtype RegionSet = RegionSet { regSet :: S.Set Region }

regionInsert :: Exp -> Region -> RegionSet -> TcM RegionSet
regionInsert e r (RegionSet regSet) = do
  if (S.member r regSet)
  then throwError $ GenericTC "Shadowed regions not allowed" e
  else return $ RegionSet (S.insert r regSet)

data TCError = GenericTC String Exp
             | VarNotFoundTC Var Exp
             | UnsupportedExpTC Exp
             | DivergingEffectsTC Exp LocationTypeState LocationTypeState
               deriving (Read,Show,Eq,Ord, Generic, NFData)
    
type TcM a = ExceptT TCError SyM a

lookupVar :: Env2 Ty -> Var -> Exp -> TcM Ty
lookupVar env var exp =
    case M.lookup var $ vEnv env of
      Nothing -> throwError $ VarNotFoundTC var exp
      Just ty -> return ty

combineTStates :: Exp -> LocationTypeState -> LocationTypeState -> TcM LocationTypeState
combineTStates exp ts1 ts2 = if ts1 == ts2 then return ts1 -- TODO: is this right?
                             else throwError $ DivergingEffectsTC exp ts1 ts2

ensureEqual :: Eq a => Exp -> String -> a -> a -> TcM a
ensureEqual exp str a b = if a == b then return a else throwError $ GenericTC str exp

ensureEqualTy :: Exp -> Ty -> Ty -> TcM Ty
ensureEqualTy exp a b = ensureEqual exp ("Expected these types to be the same: "
                                         ++ (show a) ++ ", " ++ (show b)) a b

extendEnv :: Env2 Ty -> Var -> Ty -> Env2 Ty
extendEnv (Env2 vEnv fEnv) v ty = Env2 (M.insert v ty vEnv) fEnv

type NewFuns = M.Map Var L2.FunDef

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
          do let (ArrowTy locVars arrIn arrEffs arrOut) = getFunTy funs v
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
               (tys,tstate') <- tcCases ddfs env funs constrs regs tstate brs
               foldM_ (ensureEqualTy exp) (tys !! 0) (tail tys)
               return (tys !! 0,tstate')
      DataConE l dc es -> do
               (tys,tstate) <- tcExps ddfs env funs constrs regs tstatein es
               let dcty = getTyOfDataCon ddfs dc
               let args = lookupDataCon ddfs dc
               if length args /= length es
               then throwError $ GenericTC "Invalid argument length" exp
               else do
                 sequence_ [ ensureEqualTy exp ty1 ty2
                           | (ty1,ty2) <- zip args tys ]
                 -- TODO: update tstate
                 -- TODO: ensure correct locations on arguments
                 return (PackedTy dcty l, undefined)
      TimeIt e ty b -> do
               (ty1,tstate1) <- recur tstatein e
               -- ensureEqualTy exp ty ty1
               return (ty1,tstate1)
      MapE _ _ -> throwError $ UnsupportedExpTC exp 
      FoldE _ _ _ -> throwError $ UnsupportedExpTC exp 
      Ext (LetRegionE r e) -> do
               regs' <- regionInsert exp r regs
               tcExp ddfs env funs constrs regs' tstatein e
      Ext (LetLocE v c e) -> _
      Ext (RetE ls v) -> do
               -- skip returned locations for now
               recur tstatein $ VarE v

    where recur ts e = tcExp ddfs env funs constrs regs ts e

tcCases :: DDefs Ty -> Env2 Ty -> NewFuns
        -> ConstraintSet -> RegionSet -> LocationTypeState -> [(DataCon, [(Var,LocVar)], Exp)]
        -> TcM ([Ty], LocationTypeState)
tcCases ddfs env funs constrs regs tstatein ((dc, vs, e):cases) = undefined
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

tester' =
    let ddfs = M.empty
        env = Env2 M.empty M.empty
        funs = M.empty
        constrs = ConstraintSet $ S.empty
        regs = RegionSet $ S.empty
        tstate = LocationTypeState $ M.empty
    in tcExp ddfs env funs constrs regs tstate 

tester = fst . runSyM 0 . runExceptT . tester'

test1 = case tester $ LitE 1 of
          Left err -> putStrLn (show err)
          Right (ty,tstate) -> putStrLn (show ty)

test2 = case tester $ LetE ("a",[],IntTy,LitE 1) (PrimAppE L1.AddP [VarE "a",VarE "a"]) of
          Left err -> putStrLn (show err)
          Right (ty,tstate) -> putStrLn (show ty)
