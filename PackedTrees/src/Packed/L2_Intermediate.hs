-- | An intermediate language with cursors but not explicit memory
-- representations.

module Packed.L2_Intermediate where

import Foreign.Ptr
import Foreign.Storable
import Packed.Common
import Data.Word
    
-- | A monadic intermediate language.  This hides the details of
-- packed-adt representation, but it exposes a "cursor" argument to
-- every tree constructor, and it makes constructing tree values an IO
-- action.
data L2 = Varref Var
        | App L2 L2
        | Lam (Var,T2) L2
        | CaseEither L2 L2 L2
        | CasePacked L2 [([Var], L2)]
        | Add L2 L2 -- One primitive.
        | Letrec [(Var,T2,L2)] L2
        | InL L2 | InR L2 | MkProd L2 L2

        -- NEW: Monadic operations:
        | Bind L2 L2
        | Return L2
        | IfEq (L2,L2) L2 L2 -- ^ For casing on numeric tags:           
        | NewBuf             -- ^ Allocate a new buffer (could take size)
        | MkPacked Constr Var [L2]
        -- ^ CHANGED: We have a required cursor parameter to every constructor:
        | Copy { src :: Var, dst:: Var }
           -- ^ A recursive, polymorphic copy operation on any Packed type.

  deriving (Read,Show,Ord,Eq)
          
data T2 = TInt | TArr T2 T2 | TyVar Var
        | Prod T2 T2 | Sum T2 T2
        | Packed Var [T2]
        -- NEW:
        | TIO T2 -- ^ an IO action.
  deriving (Read,Show,Ord,Eq)

-- | Complete programs include datatype definitions:
data P2 = P2 { defs :: [DDef T2]
             , mainProg :: L2
             , mainTy   :: T2 }
  deriving (Read,Show,Eq,Ord)

--------------------------------------------------------------------------------

interp :: P2 -> Value L2
interp = undefined
