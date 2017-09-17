{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | An intermediate language which makes cursors explicit

module Packed.FirstOrder.L3.Syntax
  (
    -- * Extended language
    Exp3, E3Ext(..), Ty3
  , Prog(..), FunDef(..), FunDefs
  )
where

import Control.DeepSeq
import Data.Loc
import Data.Map as M
import Data.Set as S
import Text.PrettyPrint.GenericPretty

import Packed.FirstOrder.Common hiding (FunDef, FunDefs)
import Packed.FirstOrder.GenericOps
import qualified Packed.FirstOrder.L1.Syntax as L1
import qualified Packed.FirstOrder.L2.Syntax as L2

--------------------------------------------------------------------------------

type Exp3 = E3 LocVar Ty3

-- |
type Ty3 = L1.UrTy LocVar

-- | The extension that turns L1 into L3.
data E3Ext loc dec =
    ReadInt   LocVar                  -- ^ One cursor in, (int, cursor') out
  | WriteInt  LocVar (L (E3 loc dec)) -- ^ Write int at cursor, and return a cursor
  | AddCursor Int Var                 -- ^ Add a constant offset to a cursor variable
  | ReadTag   LocVar                  -- ^ One cursor in, (tag,cursor) out
  | WriteTag  DataCon LocVar          -- ^ Write Tag at Cursor, and return a cursor
  deriving (Show, Ord, Eq, Read, Generic, NFData)

-- | L1 expressions extended with L3.  This is the polymorphic version.
-- Shorthand for recursions above.
type E3 l d = L1.PreExp E3Ext l d

instance FreeVars (E3Ext l d) where
  gFreeVars  e =
    case e of
      ReadInt  v    -> S.singleton v
      WriteInt v ex -> S.insert v (gFreeVars ex)
      AddCursor _ v -> S.singleton v

instance (Out l, Out d) => Out (E3Ext l d)

instance (Out l, Out d, Show l, Show d) => Expression (E3Ext l d) where
  type LocOf (E3Ext l d) = l
  type TyOf  (E3Ext l d) = L1.UrTy l
  isTrivial e =
    case e of
      ReadInt{}   -> False
      WriteInt{}  -> False
      AddCursor{} -> False

data FunDef = FunDef { funname :: Var
                     , funty   :: L2.ArrowTy Ty3
                     , funarg  :: Var
                     , funbod  :: L Exp3
                     }
  deriving (Show, Ord, Eq, Generic, NFData)

instance Out FunDef

type FunDefs = M.Map Var FunDef

-- | Other than Exp3, everything is identical to L2
data Prog = Prog { ddefs   :: DDefs Ty3
                 , fundefs :: FunDefs
                 , mainExp :: Maybe (L Exp3, Ty3)
                 }
  deriving (Show, Ord, Eq, Generic, NFData)

instance Out Prog

--------------------------------------------------------------------------------
