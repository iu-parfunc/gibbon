{-# LANGUAGE OverloadedStrings #-}

module Packed.FirstOrder.L1.Typecheck
  ( tcProg ) where


import Control.Monad.Except
import Data.Loc
import Data.Map as M

import Debug.Trace

import Packed.FirstOrder.Common
import Packed.FirstOrder.L1.Syntax as L1

--------------------------------------------------------------------------------

type Exp = L Exp1


-- | Typecheck a L1 expression
--
tcExp :: DDefs Ty1 -> FunDefs Ty1 Exp -> Env2 Ty1 -> Exp -> TcM Ty1
tcExp ddfs funs env exp@(L p ex) =
  case ex of
    VarE v -> do
      ty <- lookupVar env v exp
      return ty

    LitE _    -> return IntTy
    LitSymE _ -> return IntTy

    AppE v ls e ->
      let fty = getFunTy funs v
      in
        __

    PrimAppE pr es -> do
      let len2 = checkLen exp pr 2 es
          len0 = checkLen exp pr 0 es

      tys <- mapM (tcExp ddfs funs env) es
      case pr of
        AddP -> do
          len2
          _ <- ensureEqualTy exp IntTy (tys !! 0)
          _ <- ensureEqualTy exp IntTy (tys !! 1)
          return IntTy

        MulP -> do
          len2
          _ <- ensureEqualTy exp IntTy (tys !! 0)
          _ <- ensureEqualTy exp IntTy (tys !! 1)
          return IntTy

    ProjE i e -> do
      ty  <- go e
      tyi <- tcProj exp i ty
      return tyi

    oth -> error $ "L1.tcExp : TODO " ++ show oth

  where
    go e = tcExp ddfs funs env e


-- | Typecheck a L1 program
--
tcProg :: Prog -> SyM Prog
tcProg prg@Prog{ddefs,fundefs,mainExp} = do

  -- Handle functions
  mapM_ fd $ M.elems fundefs

  -- Handle main expression
  case mainExp of
    Nothing -> return ()
    Just e  ->
      let res = runExcept $ tcExp ddefs fundefs (Env2 M.empty M.empty) e
      in case res of
        Left err -> error $ show err
        Right _ -> return ()

  -- Identity function for now.
  return prg

  where
    fd :: FunDef Ty1 Exp -> SyM ()
    fd FunDef{funArg,funRetTy,funBody} = do
      let (arg,argTy) = funArg
          env = extendEnv (Env2 M.empty M.empty) arg argTy
          res = runExcept $ tcExp ddefs fundefs env funBody
      case res of
        Left err -> error $ show err
        Right ty -> if ty == funRetTy
                    then return ()
                    else error $ "Expected type " ++ (show funRetTy)
                         ++ " and got type " ++ (show ty)

      return ()

--------------------------------------------------------------------------------

-- TODO(cskksc): share this with L2.Typecheck In fact, almost all of tcProg
-- is the same too. Maybe add a Typecheck class ?

-- TODO(cskksc): Move this in L1
data ArrowTy t = ArrowTy { arrIn :: t,
                           arrOut :: t
                         }

getFunTy fns f = case M.lookup f fns of
                   Nothing -> error $ "L1.getFunTy: function not found" ++
                              show f
                   -- Just (FunDef{funTy}) -> funty

data TCError = GenericTC String Exp
             | VarNotFoundTC Var Exp
             | UnsupportedExpTC Exp
  deriving (Show, Eq, Ord)

type TcM a = Except TCError a


extendEnv :: Env2 Ty1 -> Var -> Ty1 -> Env2 Ty1
extendEnv (Env2 vEnv fEnv) v ty = Env2 (M.insert v ty vEnv) fEnv


lookupVar :: Env2 Ty1 -> Var -> Exp -> TcM Ty1
lookupVar env var exp =
    case M.lookup var $ vEnv env of
      Nothing -> throwError $ VarNotFoundTC var exp
      Just ty -> return ty

tcProj :: Exp -> Int -> Ty1 -> TcM Ty1
tcProj _ i (ProdTy tys) = return $ tys !! i
tcProj e _i ty = throwError $ GenericTC ("Projection from non-tuple type " ++ (show ty)) e


checkLen :: (Show op, Show arg) => Exp -> op -> Int -> [arg] -> TcM ()
checkLen expr pr n ls =
  if length ls == n
  then return ()
  else throwError $ GenericTC ("Wrong number of arguments to "++show pr++
                               ".\nExpected "++show n++", received "
                                ++show (length ls)++":\n  "++show ls) expr

-- | Ensure that two things are equal.
-- Includes an expression for error reporting.
ensureEqual :: Eq a => Exp -> String -> a -> a -> TcM a
ensureEqual exp str a b = if a == b
                          then return a
                          else throwError $ GenericTC str exp


-- | Ensure that two types are equal.
-- Includes an expression for error reporting.
ensureEqualTy :: Exp -> Ty1 -> Ty1 -> TcM Ty1
ensureEqualTy exp a b = ensureEqual exp ("Expected these types to be the same: "
                                         ++ (show a) ++ ", " ++ (show b)) a b


--------------------------------------------------------------------------------

test1 = runSyM 0 $ tcProg t1

t1 :: Prog
t1 =
  Prog {ddefs = M.fromList [],
        fundefs = M.fromList
                  [("mul2",
                    FunDef {funName = "mul2",
                            funArg = ("x_y1", ProdTy [IntTy,IntTy]),
                            funRetTy = IntTy,
                            funBody = L NoLoc $ PrimAppE MulP
                                      [L NoLoc $ ProjE 0 (L NoLoc $ VarE "x_y1"),
                                       L NoLoc $ ProjE 1 (L NoLoc $ VarE "x_y1")]}),
                   ("add2",
                    FunDef {funName = "add2",
                            funArg = ("x_y0", ProdTy [IntTy,IntTy]),
                            funRetTy = IntTy,
                            funBody = L NoLoc $ PrimAppE AddP
                                      [L NoLoc $  ProjE 0 (L NoLoc $ VarE "x_y0"),
                                       L NoLoc $ ProjE 1 (L NoLoc $ VarE "x_y0")]})],
        mainExp = Just $ L NoLoc $ AppE "mul2"
                  []
                  (L NoLoc $ MkProdE [L NoLoc $ LitE 10, L NoLoc $ AppE "add2" [] (L NoLoc $ MkProdE [L NoLoc $ LitE 40, L NoLoc $ LitE 2])])}
