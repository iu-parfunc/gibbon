{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

-- |

module Packed.FirstOrder.Target where

--------------------------------------------------------------------------------

import Data.Word (Word8)
import Data.List (foldl1')

import Data.Loc (noLoc)
import Language.C.Quote.C (cexp, cstm)
import qualified Language.C.Quote.C as C
import qualified Language.C.Syntax as C

import Packed.HigherOrder.L1_Source (T1 (..))

--------------------------------------------------------------------------------
-- * AST definition

type Var = String

data Triv
  = VarTriv Var
  | IntTriv Int
  deriving (Show)

data Exp
  = TrivE Triv
  | PrimAppE Prim [Triv]
  | AppE Var [Triv]
  | MkTupleE [Triv]
  | ProjE Var Int
  | LetE [(Var,Ty,Exp)] Exp
  | IfEqE (Triv,Triv) Exp Exp
      -- ^ For casing on numeric tags.
  | NewBufE Int
      -- ^ Allocate a new buffer, return a cursor.
  | WriteTagE Var Word8
      -- ^ Write a tag at a cursor.
  | WriteIntE Var Triv
      -- ^ Write (leaf) data
  | ReadTagE Var
      -- ^ Read one byte from the cursor and advance it.
  | ReadIntE Var
      -- ^ Read an 8 byte Int from the cursor and advance.
  deriving (Show)

data Ty
  = IntTy
  | SymTy
      -- ^ Symbols used in writing compiler passes
  | CursorTy
  | ProdTy [Ty]
  | SymDictTy T1
      -- ^ We allow built-in dictionaries from symbols to a value type.
  deriving (Show)

data Prim
  = AddP
  | SubP
  | MulP
  | DictInsertP -- ^ takes k,v,dict
  | DictLookupP -- ^ takes k,dict, errors if absent
  deriving (Show)

--------------------------------------------------------------------------------
-- * C codegen

newtype Cg = Cg { unwrapCg :: () }

codegen_triv :: Triv -> C.Exp
codegen_triv (VarTriv v) = C.Var (C.toIdent v noLoc) noLoc
codegen_triv (IntTriv i) = [cexp| $i |]

codegen_exp
  :: Exp     -- ^ source expression
  -> C.Exp   -- ^ where to bind the result (must be a valid LHS)
  -> [C.BlockItem]

codegen_exp (TrivE triv) ret =
    [ C.BlockStm [cstm| $ret = $(codegen_triv triv); |] ]

codegen_exp (PrimAppE AddP as) ret = codegen_binop C.Add as ret
codegen_exp (PrimAppE SubP as) ret = codegen_binop C.Sub as ret
codegen_exp (PrimAppE MulP as) ret = codegen_binop C.Mul as ret

codegen_exp (PrimAppE DictInsertP [k, v, dict]) ret =
    [ C.BlockStm [cstm| $ret = dict_insert($dict', $k', $v'); |] ]
  where
    k' = codegen_triv k
    v' = codegen_triv v
    dict' = codegen_triv dict

codegen_exp (PrimAppE DictLookupP [k, dict]) ret =
    [ C.BlockStm [cstm| $ret = dict_lookup($dict', $k'); |] ]
  where
    k' = codegen_triv k
    dict' = codegen_triv dict

codegen_exp invalid@PrimAppE{} _ =
    error ("codgen_exp: Invalid PrimApp: " ++ show invalid)

codegen_exp (AppE v as) ret =
    [ C.BlockStm [cstm| $ret = $( C.FnCall fn args noLoc ); |] ]
  where
    fn = C.Var (C.toIdent v noLoc) noLoc
    args = map codegen_triv as

-- TODO: MkTupleE
-- TODO: ProjE

codegen_exp (LetE binds e) ret =
    concatMap codegen_binds binds ++ codegen_exp e ret

codegen_exp (IfEqE (t1, t2) then_ else_) ret =
    [ C.BlockStm $
      C.If [cexp| $t1' == $t2' |]
           (mkBlock (codegen_exp then_ ret))
           (Just (mkBlock (codegen_exp else_ ret)))
           noLoc ]
  where
    t1' = codegen_triv t1
    t2' = codegen_triv t2

codegen_exp (NewBufE size) ret =
    [ C.BlockStm [cstm| $ret = (char*)malloc($size); |] ]

codegen_exp (WriteTagE ptr w) _ =
    [ C.BlockStm [cstm| *$(cid ptr) = $w; |]
    , C.BlockStm [cstm| $(cid ptr)++;     |] ]

codegen_exp (WriteIntE ptr t) _ =
    [ C.BlockStm [cstm| *((int*)$(cid ptr)) = $(codegen_triv t); |]
    , C.BlockStm [cstm| $(cid ptr) += sizeof(int);               |] ]
  where
    ptr' = C.Var (C.toIdent ptr noLoc) noLoc

codegen_exp (ReadTagE ptr) ret =
    [ C.BlockStm [cstm| $ret = *$(cid ptr); |]
    , C.BlockStm [cstm| $(cid ptr)++;       |] ]

codegen_exp (ReadIntE ptr) ret =
    [ C.BlockStm [cstm| $ret = *((int*)$(cid ptr)); |]
    , C.BlockStm [cstm| $(cid ptr) += sizeof(int);  |] ]

codegen_binds :: (Var, Ty, Exp) -> [C.BlockItem]
codegen_binds (v, ty, e) =
    ( C.BlockDecl (C.AntiDecl (codegen_ty ty ++ " " ++ v ++ ";") noLoc)
    : codegen_exp e (cid v) )

codegen_binop :: C.BinOp -> [Triv] -> C.Exp -> [C.BlockItem]
codegen_binop op as ret =
    [ C.BlockStm
      [cstm| $ret = $(foldl1' (\e1 e2 -> C.BinOp op e1 e2 noLoc) (map codegen_triv as)); |] ]

codegen_ty :: Ty -> String
codegen_ty IntTy    = "int"
codegen_ty SymTy    = "sym"
codegen_ty CursorTy = "cursor"
codegen_ty ProdTy{} = undefined -- TODO: This is more general than we need I think
codegen_ty SymDictTy{} = undefined -- TODO: Not sure what this is

mkBlock :: [C.BlockItem] -> C.Stm
mkBlock ss = C.Block ss noLoc

cid :: Var -> C.Exp
cid v = C.Var (C.toIdent v noLoc) noLoc
