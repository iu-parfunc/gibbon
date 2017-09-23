{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | An intermediate language which makes cursors explicit

module Packed.FirstOrder.L3.Syntax
  (
    -- * Extended language
    Exp3, E3Ext(..), Ty3
  , Prog(..), FunDef(..), FunDefs, ArrowTy(..)

    -- * Functions
  , eraseLocMarkers, stripTyLocs
  )
where

import Control.DeepSeq
import Data.Loc
import Data.Map as M
import Data.Set as S
import Data.List as L
import Text.PrettyPrint.GenericPretty

import Packed.FirstOrder.Common hiding (FunDef, FunDefs)
import Packed.FirstOrder.GenericOps
import Packed.FirstOrder.L1.Syntax (UrTy(..), PreExp(..))
import qualified Packed.FirstOrder.L2.Syntax as L2

--------------------------------------------------------------------------------

type Exp3 = E3 () Ty3

-- |
type Ty3 = UrTy ()

-- | The extension that turns L1 into L3.
data E3Ext loc dec =
    ReadInt   Var                  -- ^ One cursor in, (int, cursor') out
  | WriteInt  Var (L (E3 loc dec)) -- ^ Write int at cursor, and return a cursor
  | AddCursor Var Int              -- ^ Add a constant offset to a cursor variable
  | ReadTag   Var                  -- ^ One cursor in, (tag,cursor) out
  | WriteTag  DataCon Var          -- ^ Write Tag at Cursor, and return a cursor
  | NewBuffer                      -- ^ Create a new buffer, and return a cursor
  deriving (Show, Ord, Eq, Read, Generic, NFData)

-- | L1 expressions extended with L3.  This is the polymorphic version.
-- Shorthand for recursions above.
type E3 l d = PreExp E3Ext l d

instance FreeVars (E3Ext l d) where
  gFreeVars  e =
    case e of
      ReadInt  v    -> S.singleton v
      WriteInt v ex -> S.insert v (gFreeVars ex)
      AddCursor v _ -> S.singleton v
      ReadTag v     -> S.singleton v
      WriteTag _ v  -> S.singleton v
      NewBuffer     -> S.empty

instance (Out l, Out d) => Out (E3Ext l d)

instance (Out l, Out d, Show l, Show d) => Expression (E3Ext l d) where
  type LocOf (E3Ext l d) = l
  type TyOf  (E3Ext l d) = UrTy l
  isTrivial e =
    case e of
      ReadInt{}   -> False
      WriteInt{}  -> False
      AddCursor{} -> False
      ReadTag{}   -> False
      WriteTag{}  -> False
      NewBuffer   -> False

data ArrowTy t = ArrowTy { arrIn  :: t ,
                           arrOut :: t
                         }
  deriving (Show, Ord, Eq, Generic, NFData)

instance (Out t) => Out (ArrowTy t)

data FunDef = FunDef { funname :: Var
                     , funty   :: ArrowTy Ty3
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

-- | Erase LocVar markers from the data definition
eraseLocMarkers :: DDef L2.Ty2 -> DDef Ty3
eraseLocMarkers (DDef tyname ls) = DDef tyname $ L.map go ls
  where go :: (DataCon,[(IsBoxed,L2.Ty2)]) -> (DataCon,[(IsBoxed,Ty3)])
        go (dcon,ls') = (dcon, L.map (\(b,ty) -> (b,stripTyLocs ty)) ls')

-- | Remove the extra location annotations.
stripTyLocs :: L2.Ty2 -> Ty3
stripTyLocs ty =
  case ty of
    IntTy     -> IntTy
    BoolTy    -> BoolTy
    ProdTy ls -> ProdTy $ L.map stripTyLocs ls
    SymDictTy ty'    -> SymDictTy $ stripTyLocs ty'
    PackedTy tycon _ -> PackedTy tycon ()
    ListTy ty'       -> ListTy $ stripTyLocs ty'
    PtrTy    -> PtrTy
    CursorTy -> CursorTy


--------------------------------------------------------------------------------
