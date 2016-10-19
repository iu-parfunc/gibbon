{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Lang.Regions where

import qualified Lang.Source as L1
import GHC.Generics

type Name = L1.Name
type TyName = L1.TyName

-- * Types for the region language are similar to the source language,
--   but with region annotations and a type for enforcing constructors are
--   created correctly
data RTy = RArrTy [Region] [RTy] RTy
         | RProdTy [RTy] Region
         | RTag TyName Region
         | RIntTy 
         | RBoolTy 
         | RVarTy TyName Region
         | RBlock [(Protocol,Region)] RTy
  deriving (Read,Show,Eq,Ord,Generic)
           
-- * Type-level specification of a series of types to be consumed
data Protocol = Needed RTy Protocol
              | Done
  deriving (Read,Show,Eq,Ord,Generic)

-- * Region types are either a region parameter or region zero
data Region = RegionParam TyName
            | RegionZero
  deriving (Read,Show,Eq,Ord,Generic)

-- * Relations on regions
data RRel = After Region Region
          | End Region
          | Inside Region Region
 deriving (Read,Show,Eq,Ord,Generic)
           
-- * Region type environments also have a list of relations
data RTyEnv = RTyEnv [(TyName,RTy)] [RRel]
  deriving (Read,Show,Eq,Ord,Generic)

data RTopLevel = RTopLevel RTyEnv [RFunDecl] RExpr
  deriving (Read,Show,Eq,Ord,Generic)

data RFunDecl = RFunDecl Name [Name] RBlock
  deriving (Read,Show,Eq,Ord,Generic)

data RBlock = RBind Name RAction RBlock
            | RReturn RExpr
  deriving (Read,Show,Eq,Ord,Generic)

data RAction = RRead Region
             | RWrite Name Region
             | RPure RExpr
  deriving (Read,Show,Eq,Ord,Generic)

data RExpr = RVarE Name
           | RSwitch [(Name,RBlock)]
           | RLetE [(Name,RExpr)] RExpr
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
