{-# LANGUAGE DeriveAnyClass    #-}

-- | The higher-ordered surface language.

module Packed.FirstOrder.L0.Syntax
       ( -- Expressions
         Exp0, E0Ext(..), Ty0(..), Prog(..),
         VarDef(..)
       )
where
   
import Control.DeepSeq (NFData)
-- import Data.List as L
import Data.Loc
-- import Data.Map as M
-- import Data.Set as S
import GHC.Generics
-- import Text.PrettyPrint.GenericPretty

import Packed.FirstOrder.Common as C
-- import Packed.FirstOrder.GenericOps

import qualified Packed.FirstOrder.L1.Syntax as L1
    
-----------------------------------------------------------

data Prog = Prog { ddefs    :: DDefs Ty0
                 , fundefs  :: FunDefs Ty0 (L Exp0)
                 , mainExp  :: Maybe (L Exp0)
                 }
  deriving (Show, Eq, Ord, Generic, NFData)

-- TODO: Ty0 as Ty1 plus arrows?

type TyVar = Var

data Ty0 =
   IntTy
 | BoolTy
 | TyVar
 | ProdTy [Ty0]
 | SymDictTy Ty0 
 | ArrowTy Ty0 Ty0
 | PackedTy TyCon [TyVar]
 | ListTy Ty0
 deriving (Show, Read, Ord, Eq, Generic, NFData)

data Scheme = ForAll [TyVar] Ty0
 deriving(Show)

-- | Extended expressions, L0.
type Exp0 = E0 () Ty0

-- | The extension that turns L1 into L0.
data E0Ext loc dec =
   LambdaE Var (L (E0 loc dec)) 
 | PolyAppE (L (E0 loc dec)) -- ^ Operator
            (L (E0 loc dec)) -- ^ Operand
 deriving (Show, Ord, Eq, Read, Generic, NFData)
          
-- | L1 expressions extended with L0.  This is the polymorphic version.
-- Shorthand for recursions above.
type E0 l d = L1.PreExp E0Ext l d
 

data VarDef a ex = VarDef { varName :: Var
                          , varTy   :: a
                          , varBody :: ex }
  deriving (Show, Eq, Ord, Generic, NFData)

