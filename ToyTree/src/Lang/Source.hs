{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Lang.Source where

import GHC.Generics

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
        | VarTy TyName
  deriving (Read,Show,Eq,Ord,Generic)

-- * A type environment is a mapping from names to types. For now we have one living in
--   the top-level.
data TyEnv = TyEnv [(Name,Ty)]
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
          | ConstrE Name [Name] -- * constructor application
          | ProjE TyName Name Int -- * project the nth field out of a structure 
          | PrimOpE Prim [Name] -- * apply primitive operation (punting on this)
          | IfE Name Expr Expr -- * conditional expression
          | AppE Name Name -- * function application (must be names)
          | IntE Int -- * int literals
          | BoolE Bool -- * bool literals
  deriving (Read,Show,Eq,Ord,Generic)

-- * Primitive operations.
--   I'm already bored, might not do this. Maye just make Plus an expr. 
data Prim = PlusP | SubP | MulP
  deriving (Read,Show,Eq,Ord,Generic)

exadd1 :: TopLevel
exadd1 =
  TopLevel
  (TyEnv [("Tree", SumTy [("Leaf",[IntTy]),("Node",[VarTy "Tree", VarTy "Tree"])]),
          ("add1", ArrTy [VarTy "Tree"] (VarTy "Tree"))])
  [(FunDecl "add1" ["t"]
     (CaseE "t" [
         ("Leaf", "x",
          (LetE [("v1",(IntE 1)),("v2",(ProjE "Leaf" "x" 0))]
            (PrimOpE PlusP ["v1","v2"]))),
         ("Node", "x",
          (LetE [("x1",(ProjE "Node" "x" 0)),("x2",(ProjE "Node" "x" 1))]
            (LetE [("y1",(AppE "add1" "x1")),("y2",(AppE "add1" "x2"))]
              (ConstrE "Node" ["y1","y2"]))))
         ]))
  ]
  (IntE 0)
  
