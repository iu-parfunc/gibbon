{-# LANGUAGE DeriveAnyClass    #-}

-- | The higher-ordered surface language.

module Packed.FirstOrder.L0.Syntax
       (Exp0, E0Ext(..))
 where
   
import Control.DeepSeq (NFData, rnf)
import Data.List as L
import Data.Loc
import Data.Map as M
import Data.Set as S
import GHC.Generics
import Text.PrettyPrint.GenericPretty

import Packed.FirstOrder.Common as C
import Packed.FirstOrder.GenericOps

import qualified Packed.FirstOrder.L1.Syntax as L1
    
-----------------------------------------------------------

-- TODO: Ty0 as Ty1 plus arrows?

data Ty0 = IntTy | ArrowTy -- ...

-- | Extended expressions, L2.  Monomorphic.
type Exp0 = E0 LocVar Ty0

-- | The extension that turns L1 into L2.
data E0Ext loc dec =
   LambdaE Var (L (E0 loc dec)) 
 | PolyAppE (L (E0 loc dec)) -- ^ Operator
            (L (E0 loc dec)) -- ^ Operand
 deriving (Show, Ord, Eq, Read, Generic, NFData)
          
-- | L1 expressions extended with L2.  This is the polymorphic version.
-- Shorthand for recursions above.
type E0 l d = L1.PreExp E0Ext l d
 
