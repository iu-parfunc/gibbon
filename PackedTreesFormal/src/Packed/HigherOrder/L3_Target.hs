-- | A final target language that directly exposes the representation
-- of buffers used to store trees.

module Packed.HigherOrder.L3_Target where

import Foreign.Ptr
import Foreign.Storable
import Packed.HigherOrder.Common
import Data.Word
    
-- | The target language, a monadic one.   
data L3 = Varref Var | Lit Int
        | App L3 L3
        | Lam (Var,T3) L3
        | CaseEither L3 L3 L3
        | CasePacked L3 [([Var], L3)]
        | Add L3 L3 -- One primitive.
        | Letrec [(Var,T3,L3)] L3
        | InL L3 | InR L3 | MkProd L3 L3
        -- REMOVED: packed constructors.
--        | Bind L3 L3
--        | Return L3
        | IfEq (L3,L3) L3 L3  -- ^ For casing on numeric tags:

        | NewBuf -- ^ Allocate a new buffer, return a cursor.
        -- NEW: raw buffer operations
        -- Cursors are always represented by variables, not arbitrary expressions:  
        | Copy Var Var    -- ^ Copy into the buffer from another packed.
        | WriteTag Var Word8  -- ^ Write a tag at a cursor.
        | WriteInt Var Int    -- ^ Write (leaf) data
        | ReadTag Var         -- ^ Read one byte from the cursor and advance it.
        | ReadInt Var         -- ^ Read an 8 byte Int from the cursor and advance.
          
data T3 = TInt | TArr T3 T3 | TyVar Var
        | Prod T3 T3 | Sum T3 T3
        -- Removed: Packed types.
        -- NEW:
        | TIO T3
        | TCursor -- ^ A pointer into a buffer.

data V3
          
--------------------------------------------------------------------------------

interp :: L3 -> String
interp = undefined
