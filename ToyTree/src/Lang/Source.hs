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
        | SumTy [(TyName,[Ty])]
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
          | ConstrE TyName [Name] -- * constructor application (eg to create products/sums)
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
          case lookup n tenv of
            Nothing -> False
            Just _ -> True
      CaseE n ls -> undefined
      LetE ls e1 -> undefined
      ConstrE tn ns -> undefined
      ProjE tn n i -> undefined
      PrimOpE p ns -> undefined
      IfE n e1 e2 ->
          let res = do t <- lookup n tenv
                       t1 <- typeof te e1
                       t2 <- typeof te e2
                       return (t,t1,t2)
          in case res of
               Nothing -> False
               Just (t,t1,t2) -> t == BoolTy && t1 == t2
      AppE n1 ns ->
          let mTy = do n1' <- lookup n1 tenv
                       ns' <- mapM (\n -> lookup n tenv) ns
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
      VarE n -> lookup n tenv
      CaseE n ls -> undefined
      LetE ls e1 -> undefined
      ConstrE tn ns -> undefined
      ProjE tn n i -> undefined
      PrimOpE p ns -> undefined
      IfE n e1 e2 -> undefined
      AppE n1 ns ->
          do t1 <- lookup n1 tenv
             case t1 of
               ArrTy _ t -> return t
               _ -> Nothing
      IntE _ -> Just IntTy
      BoolE _ -> Just BoolTy
      

exadd1 :: TopLevel
exadd1 =
  TopLevel
  (TyEnv [("Tree", SumTy [("Leaf",[IntTy]),("Node",[TyCon "Tree", TyCon "Tree"])]),
          ("add1", ArrTy [TyCon "Tree"] (TyCon "Tree"))])
  [(FunDecl "add1" ["t"]
     (CaseE "t" [
         ("Leaf", "x",
          (LetE [("v1",(IntE 1)),("v2",(ProjE "Leaf" "x" 0))]
            (PrimOpE PlusP ["v1","v2"]))),
         ("Node", "x",
          (LetE [("x1",(ProjE "Node" "x" 0)),("x2",(ProjE "Node" "x" 1))]
            (LetE [("y1",(AppE "add1" ["x1"])),("y2",(AppE "add1" ["x2"]))]
              (ConstrE "Node" ["y1","y2"]))))
         ]))
  ]
  (IntE 0)
  
