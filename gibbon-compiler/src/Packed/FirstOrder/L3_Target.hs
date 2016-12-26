
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Defines the target language for first-order L1 language with C code
-- generator for it.

module Packed.FirstOrder.L3_Target
    ( Var, Tag, Tail(..), Triv(..), Ty(..), Prim(..), FunDecl(..),
      Alts(..), Prog(..), MainExp(..)
    ) where

import           Control.DeepSeq
import           Data.Int
import           Data.Maybe
import           Data.Word (Word8)
import           GHC.Generics (Generic)
import           Packed.FirstOrder.Common hiding (funBody)
import           Prelude hiding (init)
import           Text.PrettyPrint.GenericPretty (Out (..))

--------------------------------------------------------------------------------
-- * AST definition

data Prog = Prog
  { fundefs :: [FunDecl]
  , mainExp :: Maybe MainExp
  } deriving (Show, Ord, Eq, Generic, NFData, Out)

data MainExp
  = PrintExp Tail
      -- ^ Evaluate the expression and print the result. Type of the expression
      -- must be Int64.
  | RunWithRacketFile Var
     -- ^ Hack, expects a function from racket ast to int. 
  | RunRacketCorePass Var Var
      -- ^ Run the pass. First `Var` is a function for building initial trees,
      -- second `Var` is the function to benchmark. Return value of benchmark
      -- function is ignored.
  deriving (Show, Ord, Eq, Generic, NFData, Out)

type Tag = Word8

data Triv
    = VarTriv Var
    | IntTriv Int64
    | TagTriv Tag
  deriving (Show, Ord, Eq, Generic, NFData, Out)

-- | Switch alternatives.
data Alts
  = TagAlts [(Tag, Tail)]
      -- ^ Casing on tags.
  | IntAlts [(Int64, Tail)]
      -- ^ Casing on integers.
  deriving (Show, Ord, Eq, Generic, NFData, Out)

instance Out Int64 where
  doc w = doc (fromIntegral w :: Integer)
  docPrec n w = docPrec n (fromIntegral w :: Integer)
           
instance Out Word8 where
  doc w = doc (fromIntegral w :: Int)
  docPrec n w = docPrec n (fromIntegral w :: Int)

data Tail
    = RetValsT [Triv] -- ^ Only in tail position, for returning from a function.
    | AssnValsT [(Var,Ty,Triv)] -- ^ INTERNAL ONLY: used for assigning instead of returning.

    | LetCallT { binds :: [(Var,Ty)],
                 rator :: Var,
                 rands :: [Triv],
                 bod   :: Tail }
    | LetPrimCallT { binds :: [(Var,Ty)],
                     prim  :: Prim,
                     rands :: [Triv],
                     bod   :: Tail }

    -- Ugh, we should not strictly need this if we simplify everything in the right way:
    | LetTrivT { bnd :: (Var,Ty,Triv)
               , bod :: Tail }

    -- A control-flow join point; an If on the RHS of LeT:
    | LetIfT { binds :: [(Var,Ty)]
             , ife :: (Triv,Tail,Tail)
             , bod :: Tail
             }

    | LetUnpackT { binds :: [(Var,Ty)]
                 , ptr :: Var -- ^ Var pointing to a PtrTy
                 , bod :: Tail }
    -- ^ Unpack a struct pointer (variable of PtrTy) into local fields.

    | LetAllocT { lhs  :: Var -- ^ Var to bind to PtrTy
                , vals :: [(Ty,Triv)] -- ^ Fields of allocated struct
                , bod  :: Tail }
    -- ^ Allocate storage for a struct of the given type,
    --   Initialize all fields Return PtrTy.


    | IfT { tst :: Triv,
            con  :: Tail,
            els  :: Tail }
    | ErrT String

    | LetTimedT { isIter :: Bool
                , binds :: [(Var,Ty)]
                , timed :: Tail
                , bod :: Tail } -- ^ This is like a one-armed if.  It needs a struct return. 
      
    | Switch Triv Alts (Maybe Tail) -- TODO: remove maybe on default case
    -- ^ For casing on numeric tags or integers.
    | TailCall Var [Triv]
  deriving (Show, Ord, Eq, Generic, NFData, Out)

data Ty
    = IntTy
    | TagTyPacked  -- ^ A single byte / Word8.  Used in PACKED mode.
    | TagTyBoxed   -- ^ A tag used in the UNPACKED, boxed, pointer-based, graph-of-structs representation.
                   --   This can usually be the same as TagTy, but needn't necessarily be.

    | SymTy    -- ^ Symbols used in writing compiler passes.
               --   It's an alias for Int, an index into a symbol table.
    | CursorTy -- ^ A byte-indexing pointer.

    | PtrTy   -- ^ A machine word.  Same width as IntTy.  Untyped.

-- TODO: Make Ptrs more type safe like this:      
--    | StructPtrTy { fields :: [Ty] } -- ^ A pointer to a struct containing the given fields.

    | ProdTy [Ty]
    | SymDictTy Ty
      -- ^ We allow built-in dictionaries from symbols to a value type.
  deriving (Show, Ord, Eq, Generic, NFData, Out)

data Prim
    = AddP
    | SubP
    | MulP
    | EqP
    | SizeParam
    | DictInsertP Ty-- ^ takes k,v,dict
    | DictLookupP Ty -- ^ takes k,dict, errors if absent
    | DictEmptyP Ty
    | NewBuf
    -- ^ Allocate a new buffer, return a cursor.
    | ScopedBuf
    -- ^ Returns a pointer to a buffer, with the invariant that data written
    -- to this region is no longer used after the enclosing function returns.
    -- I.e. this can be stack allocated data.
      
    | WriteTag
    -- ^ Write a static tag value, takes a cursor to target.
    | WriteInt
    -- ^ Write (leaf) data, takes cursor,int
    | ReadTag
    -- ^ Read one byte from the cursor and advance it.
    | ReadInt
      -- ^ Read an 8 byte Int from the cursor and advance.

    | GetFirstWord -- ^ takes a PtrTy, returns IntTy containing the (first) word pointed to.

  deriving (Show, Ord, Eq, Generic, NFData, Out)

data FunDecl = FunDecl
  { funName  :: Var
  , funArgs  :: [(Var,Ty)]
  , funRetTy :: Ty
  , funBody  :: Tail
  } deriving (Show, Ord, Eq, Generic, NFData, Out)

