{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Lang.Regions where

import qualified Lang.Source as L1
import GHC.Generics

type Name = L1.Name
type TyName = L1.TyName
    
-- * Types for the region language are similar to the source language
--   Cursors have a region in their type.
data RTy = RArrTy [Region] [RTy] RTy [Region]
         | RPackProdTy [RTy]
         | RTupleTy [RTy]
         | RSumTy [(TyName,[RTy])]
         | RIntTy 
         | RBoolTy 
         | RVarTy TyName
         | RCursor Region
  deriving (Read,Show,Eq,Ord,Generic)
           
-- * Region types are either a region parameter or region zero
data Region = RegionParam Name
            | RegionZero
  deriving (Read,Show,Eq,Ord,Generic)

-- * Relations on regions. Constraints? Don't know what to call these.
--   Associations between names and regions go here, rather than in the AST,
--   since we're assuming all names are unique and using ANF. 
data RRel = After Region Region
          | Inside Region Region
          | At Name Region
 deriving (Read,Show,Eq,Ord,Generic)
           
-- * Region type environments also have a list of things we know about the regions
data RTyEnv = RTyEnv [(TyName,RTy)] [RRel]
  deriving (Read,Show,Eq,Ord,Generic)

data RTopLevel = RTopLevel RTyEnv [RFunDecl] RExpr
  deriving (Read,Show,Eq,Ord,Generic)

data RFunDecl = RFunDecl Name [Name] RExpr
  deriving (Read,Show,Eq,Ord,Generic)

type RName = Name
    
-- * Expressions in the region language
--   Here the application form is replaced with a letcall form, which
--   is like a limited form of let-values, and a yield form, which is
--   like a limited form of values.
data RExpr = RVarE Name
           | RCaseE [(Name,Name,RExpr)]
           | RLetValE [(Name,RExpr)] RExpr
           | RLetCallE (Name,[Name],[RName]) Name [RName] RExpr
           | RConstrE TyName [Name]
           | RProjE TyName Name Int
           | RPrimOpE L1.Prim [Name]
           | RIfE Name RExpr RExpr
           | RIntE Int
           | RBoolE Bool
           | RReturn Name [RName]
           | LetRegion RName RExpr
  deriving (Read,Show,Eq,Ord,Generic)

