-- | 

module Packed.FirstOrder.Target where

data Triv = VarTriv Var | IntTriv Int
    
data Exp = VarE Var | Lit Int
         | PrimAppE Prim [Triv]
         | AppE Var [Triv]
           
         | MkTupleE [Triv]
         | ProjE Var Int
           
         | Let [(Var,Ty,Exp)] Exp

         | IfEqE (Triv,Triv) Exp Exp  -- ^ For casing on numeric tags:

         | NewBufE Int -- ^ Allocate a new buffer, return a cursor.

         | WriteTag Var Word8  -- ^ Write a tag at a cursor.
         | WriteInt Var Triv    -- ^ Write (leaf) data
         | ReadTag Var         -- ^ Read one byte from the cursor and advance it.
         | ReadInt Var         -- ^ Read an 8 byte Int from the cursor and advance.

data Ty = IntTy | SymTy -- Symbols used in writing compiler passes
        | CursorTy
        | ProdTy [Ty]
        | SymDictTy T1 -- We allow built-in dictionaries from symbols to a value type.
  deriving (Read,Show,Eq,Ord, Generic)

data Prim = AddP | SubP | MulP -- ^ Need more numeric primitives...
          | DictInsertP -- ^ takes k,v,dict
          | DictLookupP -- ^ takes k dict, errors if absent
  deriving (Read,Show,Eq,Ord, Generic)
           
