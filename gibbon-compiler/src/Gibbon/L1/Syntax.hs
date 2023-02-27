{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | The source language for recursive tree traversals.
--   This is a first-order language for the "closed world" scenario:
--   not integrating with a functional host language, but rather
--   genarating C code like a DSL.

module Gibbon.L1.Syntax
    (
      -- * Core types specific to L1
      Prog1, FunDef1, FunDefs1, DDef1, DDefs1, Exp1, Ty1, E1Ext(..), CFGfunctionMap, FieldMap

    , module Gibbon.Language
    ) where

import Control.DeepSeq ( NFData )
import qualified Data.Set as S
import Data.Graph as G
import Data.Map as M
import Prelude as P
import GHC.Generics
import Text.PrettyPrint.GenericPretty

import Gibbon.Language
import Gibbon.Common

--------------------------------------------------------------------------------

instance FunctionTy Ty1 where
  -- | At this stage, function types are just (in , out) tuples.
  type ArrowTy Ty1 = ([Ty1] , Ty1)
  inTys = fst
  outTy = snd

-- | A convenient, default instantiation of the L1 expression type.
type Exp1 = PreExp E1Ext () Ty1

-- | An L1 program.
type Prog1 = Prog Exp1

-- | Datatypes
type DDefs1 = DDefs Ty1
type DDef1  = DDef Ty1

-- | Function definition used in L1 programs.
type FunDef1 = FunDef Exp1

type FunDefs1 = FunDefs Exp1

-- | The type rperesentation used in L1.
type Ty1 = UrTy ()

{-
Type CFGfunctionMap: Mapping from function definition, to the control flow graph of the program. 
Edge : A tuple of expression and its likelihood. 
See Data.Graph in containers for more definitions. 
TODO: The functions for which the CFG should be annoted at the front-end level and they should be passable to this pass. 
Only generate CFG for functions which are annotated. 
-}
type CFGfunctionMap = M.Map FunDef1 (G.Graph, G.Vertex -> ( (Exp1, Integer), Integer, [Integer]), Integer -> Maybe G.Vertex)

{- Store the field graphs for each function-}

type DataConAccessMap = M.Map DataCon [ ((Integer, Integer) , Integer ) ]

type FieldMap = M.Map FunDef1 DataConAccessMap


--------------------------------------------------------------------------------

data E1Ext loc dec = BenchE Var [loc] [(PreExp E1Ext loc dec)] Bool
                   | AddFixed Var Int
  deriving (Show, Ord, Eq, Read, Generic, NFData, Out)

instance FreeVars (E1Ext l d) where
  gFreeVars e =
    case e of
      BenchE _ _ args _-> S.unions (P.map gFreeVars args)
      AddFixed v _ -> S.singleton v

instance (Show l, Show d, Out l, Out d) => Expression (E1Ext l d) where
  type TyOf  (E1Ext l d) = d
  type LocOf (E1Ext l d) = l
  isTrivial _ = False

instance (Show l, Show d, Out l, Out d) => Flattenable (E1Ext l d) where
  gFlattenGatherBinds _ddfs _env ex = return ([], ex)
  gFlattenExp _ddfs _env ex = return ex

instance HasSimplifiableExt E1Ext l d => SimplifiableExt (PreExp E1Ext l d) (E1Ext l d) where
  gInlineTrivExt _env ext = ext

instance HasSubstitutableExt E1Ext l d => SubstitutableExt (PreExp E1Ext l d) (E1Ext l d) where
  gSubstExt old new ext =
    case ext of
      BenchE fn tyapps args b -> BenchE fn tyapps (P.map (gSubst old new) args) b
      AddFixed v i -> if v == old
                      then case new of
                             (VarE v') -> AddFixed v' i
                             _oth -> error "Could not substitute non-variable in AddFixed"
                      else AddFixed v i

  gSubstEExt old new ext =
    case ext of
      BenchE fn tyapps args b -> BenchE fn tyapps (P.map (gSubstE old new) args) b
      AddFixed v i -> AddFixed v i

instance (Show l, Show d, Out l, Out d, FunctionTy d) => Typeable (E1Ext l d) where
  gRecoverType _ddefs env2 ext =
    case ext of
      BenchE fn _ _ _ -> outTy $ fEnv env2 # fn
      AddFixed{}      -> error "gRecoverType: AddFixed not handled."

instance Renamable () where
    gRename _ () = ()

instance HasRenamable E1Ext l d => Renamable (E1Ext l d) where
  gRename env ext =
    case ext of
      BenchE fn tyapps args b -> BenchE fn tyapps (P.map go args) b
      AddFixed v i -> AddFixed (go v) i
    where
      go :: forall a. Renamable a => a -> a
      go = gRename env
