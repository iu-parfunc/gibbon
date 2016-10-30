{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lang.Source where

import GHC.Generics
import Data.Maybe

-- This is a quick sketch of a simple source language. Functions can only be defined at the
-- top level, and we need to specify all types at the top level. It's assuming ANF, and all
-- variable names should be unique. 

-- * Names are strings
type Name = String
type TyName = Name

-- * Types:
--   - arrow types (functions... shouldn't be higher order)
--   - product types (tuples, arbitrary length)
--   - sum types (also arbitrary number, pairs of name for constructor and type)
--   - integer types
data Ty = ArrTy [Ty] Ty
        | ProdTy [Ty]
        | SumTy [(TyName,Ty)]
        | IntTy
        | BoolTy
        | SymTy
        | SymMapTy Ty
        | TyCon TyName
  deriving (Read,Show,Eq,Ord,Generic)

-- * A type environment is a mapping from names to types. For now we have one living in
--   the top-level.
data TyEnv = TyEnv [(TyName,Ty)]
  deriving (Read,Show,Eq,Ord,Generic)

-- * A top level environment has a type environment, a series of functions,
--   and an expression.
data TopLevel = TopLevel TyEnv [FunDecl] Expr
  deriving (Read,Show,Eq,Ord,Generic)

-- * A function has a name, some parameter names, and an expression.
data FunDecl = FunDecl Name [Name] Expr
  deriving (Read,Show,Eq,Ord,Generic)

-- * Expression language is very simple. Case statements, let expressions,
--   application of constructors, primitive operations, etc.
data Expr = VarE Name -- * variable reference
          | CaseE Name [(Name,Name,Expr)] -- * each case pattern has name, binding, expr
          | LetE [(Name,Expr)] Expr -- * let expression with multiple bindings
          | ConstrPE TyName [Name] -- * create a product
          | ConstrSE TyName Name -- * create a sum
          | ProjE TyName Name Int -- * project the nth field out of a product 
          | PrimOpE Prim [Name] -- * apply primitive operation (punting on this)
          | IfE Name Expr Expr -- * conditional expression
          | AppE Name [Name] -- * function application (must be names)
          | IntE Int -- * int literals
          | BoolE Bool -- * bool literals
  deriving (Read,Show,Eq,Ord,Generic)

-- * Primitive operations.
data Prim = PlusP | SubP | MulP
  deriving (Read,Show,Eq,Ord,Generic)

lookupType :: TyName -> [(TyName,Ty)] -> Maybe Ty
lookupType n tenv =
  case lookup n tenv of
    Nothing -> Nothing
    Just (TyCon n) -> lookupType n tenv
    Just t -> Just t

tc :: TopLevel -> Bool
tc (TopLevel tenv funs e) = (all (tcFun tenv) funs) && tcExpr tenv e

tcFun :: TyEnv -> FunDecl -> Bool
tcFun (TyEnv tenv) (FunDecl _nam args e) =
    let tenv' = tenv ++ (zip args $ map fromJust $ map (\n -> lookup n tenv) args)
    in tcExpr (TyEnv tenv') e

tcExpr :: TyEnv -> Expr -> Bool
tcExpr te@(TyEnv tenv) e =
    case e of
      VarE n ->
          case lookupType n tenv of
            Nothing -> False
            Just _ -> True
      CaseE n ls -> 
          (all (\(_,_,e) -> tcExpr te e) ls) 
      LetE ls e1 -> 
          (tcExpr te e1) &&
                 (isJust $ do ts1 <- mapM (\(n,_) -> lookup n tenv) ls
                              ts2 <- mapM (\(_,e) -> typeof te e) ls
                              Just $ all id $ zipWith (==) ts1 ts2)
      ConstrPE tn ns -> -- True
           let ns' = catMaybes $ map (\n -> lookup n tenv) ns
           in case lookupType tn tenv of
                Just (ProdTy ts) -> ts == ns'
                _ -> False
      ConstrSE tn n ->
          case lookup n tenv of
            Nothing -> False
            Just tt@(TyCon t) ->
              case lookupType t tenv of
                Nothing -> False
                Just s -> case lookupType tn tenv of
                            Just (SumTy st) -> any (\(_,x) -> x == tt) st
                            _ -> False
      ProjE tn n i ->
          True
      PrimOpE p ns -> 
          let ns' = catMaybes $ map (\n -> lookup n tenv) ns
          in (length ns' == length ns) && all (== IntTy) ns' -- TODO: non-int args
      IfE n e1 e2 ->
          let res = do
                t <- lookup n tenv
                t1 <- typeof te e1
                t2 <- typeof te e2
                return (t,t1,t2)
          in case res of
               Nothing -> False
               Just (t,t1,t2) -> t == BoolTy && t1 == t2 &&
                                 tcExpr te e1 && tcExpr te e2
      AppE n1 ns ->
          let mTy = do n1' <- lookupType n1 tenv
                       ns' <- mapM (\n -> lookupType n tenv) ns
                       return (n1', ns')
          in case mTy of
               Nothing -> False
               Just (ArrTy args _rt, ts) -> args == ts
               Just _ -> False
      IntE _ -> True
      BoolE _ -> True

typeof :: TyEnv -> Expr -> Maybe Ty
typeof te@(TyEnv tenv) e =
    case e of
      VarE n -> lookupType n tenv
      CaseE n ls ->
          do ts <- mapM (\(_,_,e) -> typeof te e) ls
             return $ head ts
      LetE ls e1 -> typeof te e1
      ConstrPE tn ns -> lookupType tn tenv
      ConstrSE tn n -> lookupType tn tenv
      ProjE tn n i ->
          do t <- lookupType tn tenv
             case t of
               ProdTy ls -> return $ ls !! i
               _ -> Nothing
      PrimOpE p ns -> Just IntTy -- TODO
      IfE n e1 e2 -> typeof te e1
      AppE n1 ns ->
          do t1 <- lookupType n1 tenv
             case t1 of
               ArrTy _ t -> return t
               _ -> Nothing
      IntE _ -> Just IntTy
      BoolE _ -> Just BoolTy
      

exadd1 :: TopLevel
exadd1 =
  TopLevel
  (TyEnv [("Tree", SumTy [("LeafP", TyCon "LeafP"),("NodeP", TyCon "NodeP")]),
          ("NodeP", ProdTy [TyCon "Tree", TyCon "Tree"]),
          ("LeafP", ProdTy [IntTy]),
          ("add1", ArrTy [TyCon "Tree"] (TyCon "Tree")),
          -- should write code to infer these: 
          ("t", TyCon "Tree"),
          ("xl", TyCon "LeafP"),
          ("xn", TyCon "NodeP"),
          ("v1", IntTy),
          ("v2", IntTy),
          ("v3", IntTy),
          ("x1", TyCon "Tree"),
          ("x2", TyCon "Tree"),
          ("y1", TyCon "Tree"),
          ("y2", TyCon "Tree"),
          ("y3", TyCon "NodeP"),
          ("v4", TyCon "LeafP")])
  [(FunDecl "add1" ["t"]
     (CaseE "t" [
         ("Leaf", "xl",
          (LetE [("v1",(IntE 1)),("v2",(ProjE "LeafP" "x" 0))]
           (LetE [("v3",(PrimOpE PlusP ["v1","v2"]))]
            (LetE [("v4",(ConstrPE "LeafP" ["v3"]))]
             (ConstrSE "Tree" "v4"))))),
         ("Node", "xn",
          (LetE [("x1",(ProjE "NodeP" "x" 0)),("x2",(ProjE "NodeP" "x" 1))]
           (LetE [("y1",(AppE "add1" ["x1"])),("y2",(AppE "add1" ["x2"]))]
            (LetE [("y3", (ConstrPE "NodeP" ["y1","y2"]))]
             (ConstrSE "Tree" "y3")))))
         ]))
  ]
  (IntE 0)
  
