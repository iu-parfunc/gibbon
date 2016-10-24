{-# LANGUAGE OverloadedStrings #-}
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
type Tag = Word8

data Triv
    = VarTriv Var
    | IntTriv Int
    | TagTriv Tag
  deriving (Show)

data Tail
    = RetValsT [Triv] -- ^ Only in tail position, for returning from a function.
    | LetCallT { binds :: [(Var,Ty)],
                 rator :: Var,
                 rands :: [Triv],
                 bod   :: Tail }
    | LetPrimCallT { binds :: [(Var,Ty)],
                     prim  :: Prim,
                     rands :: [Triv],
                     bod   :: Tail }
    | Switch Triv [(Tag,Tail)] (Maybe Tail)
    -- ^ For casing on numeric tags.
    | TailCall Var [Triv]
  deriving (Show)

data Ty
    = IntTy
    | TagTy -- ^ A single byte.
    | SymTy -- ^ Symbols used in writing compiler passes.
            --   It's an alias for Int, an index into a symbol table.
    | CursorTy -- ^ A byte-indexing pointer.
    | ProdTy [Ty]
    | SymDictTy T1
      -- ^ We allow built-in dictionaries from symbols to a value type.
  deriving (Show,Eq,Ord)

data Prim
    = AddP
    | SubP
    | MulP
    | DictInsertP -- ^ takes k,v,dict
    | DictLookupP -- ^ takes k,dict, errors if absent
    | NewBuf 
    -- ^ Allocate a new buffer, return a cursor.
    | WriteTag
    -- ^ Write a static tag value, takes a cursor to target.
    | WriteInt
    -- ^ Write (leaf) data, takes cursor,int
    | ReadTag
    -- ^ Read one byte from the cursor and advance it.
    | ReadInt
      -- ^ Read an 8 byte Int from the cursor and advance.
  deriving (Show)

data FunDecl = FunDecl Var [(Var,Ty)] Ty Tail
  deriving (Show)

--------------------------------------------------------------------------------
-- * C codegen

newtype Cg = Cg { unwrapCg :: () }

codegenTriv :: Triv -> C.Exp
codegenTriv (VarTriv v) = C.Var (C.toIdent v noLoc) noLoc
codegenTriv (IntTriv i) = [cexp| $i |]
codegenTriv (TagTriv i) = [cexp| $i |]

codegenTail :: Tail -> C.Exp -> [C.BlockItem]
codegenTail (RetValsT [tr]) ret =
    let assn t = [ C.BlockStm [cstm| $ret = $t; |] ]
    in assn $ codegenTriv tr
codegenTail (RetValsT ts) ret =
    -- making stuff up: all fields of structs are named field_1, field_2, etc.
    let assn (t,f) = [ C.BlockStm [cstm| $(C.Member ret (C.Id f noLoc) noLoc) = $t; |] ]
    in concatMap assn $ zip (map codegenTriv ts) $ map (\n -> "field_" ++ (show n)) [1..]
codegenTail (LetCallT bnds rator rnds bod) ret = undefined
codegenTail (LetPrimCallT bnds prim rnds bod) ret = undefined
codegenTail (Switch tr cs def) ret = undefined
codegenTail (TailCall v ts) ret =
    [ C.BlockStm [cstm| $ret = $( C.FnCall (cid v) args noLoc); |] ]
    where args = map codegenTriv ts

-- codegen_exp
--   :: Exp     -- ^ source expression
--   -> C.Exp   -- ^ where to bind the result (must be a valid LHS)
--   -> [C.BlockItem]

-- codegen_exp (TrivE triv) ret =
--     [ C.BlockStm [cstm| $ret = $(codegen_triv triv); |] ]

-- codegen_exp (PrimAppE AddP as) ret = codegen_binop C.Add as ret
-- codegen_exp (PrimAppE SubP as) ret = codegen_binop C.Sub as ret
-- codegen_exp (PrimAppE MulP as) ret = codegen_binop C.Mul as ret

-- codegen_exp (PrimAppE DictInsertP [k, v, dict]) ret =
--     [ C.BlockStm [cstm| $ret = dict_insert($dict', $k', $v'); |] ]
--   where
--     k' = codegen_triv k
--     v' = codegen_triv v
--     dict' = codegen_triv dict

-- codegen_exp (PrimAppE DictLookupP [k, dict]) ret =
--     [ C.BlockStm [cstm| $ret = dict_lookup($dict', $k'); |] ]
--   where
--     k' = codegen_triv k
--     dict' = codegen_triv dict

-- codegen_exp invalid@PrimAppE{} _ =
--     error ("codgen_exp: Invalid PrimApp: " ++ show invalid)

-- codegen_exp (AppE v as) ret =
--     [ C.BlockStm [cstm| $ret = $( C.FnCall fn args noLoc ); |] ]
--   where
--     fn = C.Var (C.toIdent v noLoc) noLoc
--     args = map codegen_triv as

-- -- TODO: MkTupleE
-- -- TODO: ProjE

-- codegen_exp (LetE binds e) ret =
--     concatMap codegen_binds binds ++ codegen_exp e ret

-- codegen_exp (IfEqE (t1, t2) then_ else_) ret =
--     [ C.BlockStm $
--       C.If [cexp| $t1' == $t2' |]
--            (mkBlock (codegen_exp then_ ret))
--            (Just (mkBlock (codegen_exp else_ ret)))
--            noLoc ]
--   where
--     t1' = codegen_triv t1
--     t2' = codegen_triv t2

-- codegen_exp (NewBufE size) ret =
--     [ C.BlockStm [cstm| $ret = (char*)malloc($size); |] ]

-- codegen_exp (WriteTagE ptr w) _ =
--     [ C.BlockStm [cstm| *$(cid ptr) = $w; |]
--     , C.BlockStm [cstm| $(cid ptr)++;     |] ]

-- codegen_exp (WriteIntE ptr t) _ =
--     [ C.BlockStm [cstm| *((int*)$(cid ptr)) = $(codegen_triv t); |]
--     , C.BlockStm [cstm| $(cid ptr) += sizeof(int);               |] ]
--   where
--     ptr' = C.Var (C.toIdent ptr noLoc) noLoc

-- codegen_exp (ReadTagE ptr) ret =
--     [ C.BlockStm [cstm| $ret = *$(cid ptr); |]
--     , C.BlockStm [cstm| $(cid ptr)++;       |] ]

-- codegen_exp (ReadIntE ptr) ret =
--     [ C.BlockStm [cstm| $ret = *((int*)$(cid ptr)); |]
--     , C.BlockStm [cstm| $(cid ptr) += sizeof(int);  |] ]

-- codegen_binds :: (Var, Ty, Exp) -> [C.BlockItem]
-- codegen_binds (v, ty, e) =
--     ( C.BlockDecl (C.AntiDecl (codegen_ty ty ++ " " ++ v ++ ";") noLoc)
--     : codegen_exp e (cid v) )

-- codegen_binop :: C.BinOp -> [Triv] -> C.Exp -> [C.BlockItem]
-- codegen_binop op as ret =
--     [ C.BlockStm
--       [cstm| $ret = $(foldl1' (\e1 e2 -> C.BinOp op e1 e2 noLoc) (map codegen_triv as)); |] ]

-- codegen_ty :: Ty -> String
-- codegen_ty IntTy    = "int"
-- codegen_ty SymTy    = "sym"
-- codegen_ty CursorTy = "cursor"
-- codegen_ty ProdTy{} = undefined -- TODO: This is more general than we need I think
-- codegen_ty SymDictTy{} = undefined -- TODO: Not sure what this is

mkBlock :: [C.BlockItem] -> C.Stm
mkBlock ss = C.Block ss noLoc

cid :: Var -> C.Exp
cid v = C.Var (C.toIdent v noLoc) noLoc

leafTag :: Word8
leafTag = fromIntegral 0

nodeTag :: Word8
nodeTag = fromIntegral 1

exadd1 :: FunDecl
exadd1 = FunDecl "add1" [("t",CursorTy),("tout",CursorTy)] CursorTy exadd1Tail 

exadd1Tail :: Tail
exadd1Tail =
    LetPrimCallT [("ttag",TagTy),("t2",CursorTy)] ReadTag [VarTriv "t"]
  $ Switch (VarTriv "ttag")
           [(leafTag,leafCase),
            (nodeTag,nodeCase)]
           Nothing
    where leafCase =
              LetPrimCallT [("tout2",CursorTy)] WriteTag [TagTriv leafTag, VarTriv "tout"]
            $ LetPrimCallT [("n",IntTy),("t3",CursorTy)] ReadInt [VarTriv "t2"]
            $ LetPrimCallT [("tout3",CursorTy)] WriteInt [VarTriv "n", VarTriv "tout2"]
            $ RetValsT [VarTriv "t3", VarTriv "tout3"]
          nodeCase =
              LetPrimCallT [("tout2",CursorTy)] WriteTag [TagTriv nodeTag, VarTriv "tout"]
            $ LetCallT [("t3",CursorTy),("tout3",CursorTy)] "add1" [VarTriv "t2", VarTriv "tout2"]
            $ TailCall "add1" [VarTriv "t3", VarTriv "tout3"]
