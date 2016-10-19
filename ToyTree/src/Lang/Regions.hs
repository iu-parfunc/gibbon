{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Lang.Regions where

import qualified Lang.Source as L1
import GHC.Generics

type Name = L1.Name
type TyName = L1.TyName

-- * Types for the region language are similar to the source language
--   Functions are parameterized over some regions.
--   I'm not sure whether to explicitly put regions in these types or
--   to have an `At` relation to associate `Name`s to `Region`s, but I'm
--   leaning toward the latter because it's more flexible.
data RTy = RArrTy [Region] [RTy] RTy
         | RProdTy [RTy]
         | RSumTy [(TyName,[RTy])]
         | RIntTy 
         | RBoolTy 
         | RVarTy TyName
  deriving (Read,Show,Eq,Ord,Generic)
           
-- * Region types are either a region parameter or region zero
data Region = RegionParam Name
            | RegionZero
  deriving (Read,Show,Eq,Ord,Generic)

-- * Relations on regions. Constraints? Don't know what to call these.
--   Associations between names and regions go here, rather than in the AST,
--   since we're assuming all names are unique and using ANF. 
data RRel = After Region Region
          | End Region
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

data RExpr = RVarE Name
           | RCaseE [(Name,Name,Expr)]
           | RLetE [(Name,RExpr)] RExpr
           | RLetRegE Name RExpr
           | RPrimOpE L1.Prim [Name]
           | RIfE Name RExpr RExpr
           | RAppE Name Name
           | RIntE Int
           | RBoolE Bool
  deriving (Read,Show,Eq,Ord,Generic)

regionsOfTy :: RTy -> [Region]
regionsOfTy (RArrTy rs _ _) = rs
regionsOfTy (RProdTy _ r) = [r]
regionsOfTy (RTag _ r) = [r]
regionsOfTy (RIntTy) = [RegionZero]
regionsOfTy (RBoolTy) = [RegionZero]
regionsOfTy (RVarTy _ r) = [r]
regionsOfTy (RBlock ls _) = map snd ls
