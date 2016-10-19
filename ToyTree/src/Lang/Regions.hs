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

-- * Expressions in the region language
--   How do we express the concept of threading regions through the computation?
--   We could do it explicitly in the AST with something like the commented out code
--   below, or we could take advantage of the fact that everything has a unique name
--   and use our region relations/constraints above.
--   For example, if we have:
--     case e of
--       A b c -> let x = f b
--                    y = f c
--                    z = A x y
--                in z
--   Here we can have relations like (At e p1) (At b p2) (At c p3) (Inside p1 p2)
--   and even (After p3 p2)...
--   But how do we say that doing `f b` should get us the region to read c?
--   If we had a notion of linearity for regions, we could say that having consumed
--   some name x where we know (At x p1) and (After p2 p1) we can then consume
--   some name y where (At y p2). 
data RExpr = RVarE Name
           | RCaseE [(Name,Name,RExpr)]
           | RLetE [(Name,RExpr)] RExpr 
           -- | RLetStepE [((Name,Region),RExpr)] (RExpr,Region)
           -- | RConstrE Name [(RExpr,Region)]
           | RConstrE Name [Name] -- params already have regions... is that enough?
           | RPrimOpE L1.Prim [Name]
           | RIfE Name RExpr RExpr
           | RAppE Name Name
           -- | RLetRegE Name RExpr              
           | RIntE Int
           | RBoolE Bool
  deriving (Read,Show,Eq,Ord,Generic)
