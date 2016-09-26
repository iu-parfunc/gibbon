-- | 

module Packed.L2_Intermediate where

import Foreign.Ptr
import Foreign.Storable
import Packed.Common
import Data.Word
    
-- | The target language, a monadic one.   
data L2 = Varref Var
        | App L2 L2
        | Lam (Var,T2) L2
        | CaseEither L2 L2 L2
        | CasePacked L2 [([Var], L2)]
        | Add L2 L2 -- One primitive.
        | Letrec [(Var,T2,L2)] L2
        | InL L2 | InR L2 | MkProd L2 L2
        -- REMOVED: packed constructors.
        -- NEW:
        | Bind L2 L2
        | Return L2
        | NewPacked       -- ^ Allocate a new buffer
        | Copy Var Var    -- ^ Copy into the buffer from another packed.
        | WriteTag Word8  -- ^ Write a tag
        | WriteInt Int    -- ^ Write (leaf) data
        -- For casing on numeric tags: 
        | IfEq (L2,L2) L2 L2
          
data T2 = TInt | TArr T2 T2 | TyVar Var
        | Prod T2 T2 | Sum T2 T2
        | Packed Constr [T2]

        -- NEW:
        | TIO T2

data V2
          
--------------------------------------------------------------------------------

interp :: L2 -> String
interp = undefined
