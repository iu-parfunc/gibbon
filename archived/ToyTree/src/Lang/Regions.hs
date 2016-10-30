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

type Ty = (RTy,Region)

-- * Region types
data Region = Reg Name
            | RegZero
            | RegHole -- Name
            | RegAfter Region
  deriving (Read,Show,Eq,Ord,Generic)

-- * Relations on regions. Constraints? Don't know what to call these.
data Rel = RelInside Region Region
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
          | CaseE Name [(Name,Name,Expr)]
          | LetValE [(Name,Expr)] Expr
          | LetCallE Name [Name] [Region] Name [Region] Expr
          | ConstrPE TyName [Name]
          | ConstrSE TyName Name
          | ProjE TyName Name Int
          | PrimOpE L1.Prim [Name]
          | IfE Name Expr Expr
          | IntE Int
          | BoolE Bool
          | Return Name [Region]
          | LetRegion Region Expr
          | Traverse Region Name Region -- consume a variable somehow
  deriving (Read,Show,Eq,Ord,Generic)

exadd1 :: TopLevel
exadd1 =
  TopLevel
  (TyEnv [("Tree", (SumTy [("LeafP", (TyCon "LeafP",RegHole)),
                           ("NodeP", (TyCon "NodeP",RegHole))],RegHole)),
          ("NodeP", (PackProdTy [(TyCon "Tree",RegHole),(TyCon "Tree",RegHole)],RegHole)),
          ("LeafP", (PackProdTy [(IntTy,RegHole)],RegHole)),
          ("add1", ((ArrTy [RegHole,RegHole]
                     [(TyCon "Tree",RegHole)]
                     ((TyCon "Tree"),RegHole)
                     [RegHole,RegHole]
                    ),RegHole)),
          ("t", ((TyCon "Tree"),RegHole)),
          ("xl", ((TyCon "LeafP"),RegHole)),
          ("xn", ((TyCon "NodeP"),RegHole)),
          ("v1", ((IntTy),RegHole)),
          ("v2", ((IntTy),RegHole)),
          ("v3", ((IntTy),RegHole)),
          ("x1", ((TyCon "Tree"),RegHole)),
          ("x2", ((TyCon "Tree"),RegHole)),
          ("y1", ((TyCon "Tree"),RegHole)),
          ("y2", ((TyCon "Tree"),RegHole)),
          ("y3", ((TyCon "NodeP"),RegHole)),
          ("v4", ((TyCon "LeafP"),RegHole))]
  [])
  [(FunDecl "add1" ["t"]
     (CaseE "t" [
         ("Leaf", "xl",
          (LetValE [("v1",(IntE 1)),("v2",(ProjE "LeafP" "x" 0))]
           (LetValE [("v3",(PrimOpE L1.PlusP ["v1","v2"]))]
            (LetValE [("v4",(ConstrPE "LeafP" ["v3"]))]
             (ConstrSE "Tree" "v4"))))),
         ("Node", "xn",
          (LetValE [("x1",(ProjE "NodeP" "x" 0)),("x2",(ProjE "NodeP" "x" 1))]
           (LetCallE "add1" ["x1"] [RegHole,RegHole] "y1" [RegHole,RegHole]
            (LetCallE "add1" ["x2"] [RegHole,RegHole] "y2" [RegHole,RegHole]
             (LetValE [("y3", (ConstrPE "NodeP" ["y1","y2"]))]
              (ConstrSE "Tree" "y3"))))))
         ]))]
  (IntE 0)

