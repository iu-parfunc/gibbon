{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lang.Regions where

import qualified Lang.Source as L1
import GHC.Generics

type Name = L1.Name
type TyName = L1.TyName
    
-- * Types for the region language are similar to the source language
--   Cursors have a region in their type.
--   New experiment: associate a region with each type by making
--   RTy and Ty mutually recursive data types.
data RTy = ArrTy [Region] [Ty] Ty [Region]
         | PackProdTy [Ty]
         | TupleTy [Ty]
         | SumTy [(TyName,Ty)]
         | IntTy 
         | BoolTy 
         | VarTy TyName
         | Cursor Region
         | SymTy
         | SymMapTy Ty
         | TyCon TyName
  deriving (Read,Show,Eq,Ord,Generic)

data Ty = Ty RTy Region
  deriving (Read,Show,Eq,Ord,Generic)

-- * Region types
data Region = Reg Name
            | RegZero
            | RegHole Name
            | RegAfter Region
  deriving (Read,Show,Eq,Ord,Generic)

-- * Relations on regions. Constraints? Don't know what to call these.
--   Associations between names and regions go here, rather than in the AST,
--   since we're assuming all names are unique and using ANF. 
data Rel = RelInside Region Region
         | RelAt Name Region
         | RelEquiv Region Region
 deriving (Read,Show,Eq,Ord,Generic)
           
-- * Region type environments also have a list of things we know about the regions
data TyEnv = TyEnv [(TyName,Ty)] [Rel]
  deriving (Read,Show,Eq,Ord,Generic)

data TopLevel = TopLevel TyEnv [FunDecl] Expr
  deriving (Read,Show,Eq,Ord,Generic)

data FunDecl = FunDecl Name [Name] Expr
  deriving (Read,Show,Eq,Ord,Generic)
    
-- * Expressions in the region language
--   Here the application form is replaced with a letcall form, which
--   is like a limited form of let-values, and a return form, which is
--   like a limited form of values.
data Expr = VarE Name
          | CaseE [(Name,Name,Expr)]
          | LetValE [(Name,Expr)] Expr
          | LetCallE Name [Name] [Region] Name [Region] Expr
          | ConstrE TyName [Name]
          | ProjE TyName Name Int
          | PrimOpE L1.Prim [Name]
          | IfE Name Expr Expr
          | IntE Int
          | BoolE Bool
          | Return Name [Region]
          | LetRegion Region Expr
          | Traverse Region Name Region -- consume a variable somehow
  deriving (Read,Show,Eq,Ord,Generic)

