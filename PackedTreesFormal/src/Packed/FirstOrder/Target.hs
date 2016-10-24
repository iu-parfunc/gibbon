{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

-- |

module Packed.FirstOrder.Target where

--------------------------------------------------------------------------------

import Data.Word (Word8)
import Data.List (foldl1',nub)

import Data.Loc (noLoc)
import Language.C.Quote.C (cexp, cstm, cinit, cdecl, cty, cfun, csdecl, cparam, cedecl)
import qualified Language.C.Quote.C as C
import qualified Language.C.Syntax as C
import Language.C.Pretty
import Text.PrettyPrint.Mainland
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
    | LetCallT { binds  :: [(Var,Ty)],
                 rator  :: Var,
                 rands  :: [Triv],
                 typ    :: Ty,
                 tmpNam :: Maybe Var,  -- hack, needed a name for the returned struct, should gensym
                 bod    :: Tail }
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

codegenFun :: FunDecl -> ([C.Definition])
codegenFun (FunDecl nam args typ tal) =
    let retTy   = codegenTy typ
        params  = map (\(v,t) -> [cparam| $ty:(codegenTy t) $id:v |]) args
        body    = codegenTail tal retTy
        structs = makeStructs $ nub $ typ : (map snd args)
        fun = [cfun| $ty:retTy $id:nam ($params:params) {
                  $items:body
              } |]
    in structs ++ [(C.FuncDef fun noLoc)]

codegenTriv :: Triv -> C.Exp
codegenTriv (VarTriv v) = C.Var (C.toIdent v noLoc) noLoc
codegenTriv (IntTriv i) = [cexp| $i |]
codegenTriv (TagTriv i) = [cexp| $i |]

codegenTail :: Tail -> C.Type -> [C.BlockItem]
codegenTail (RetValsT [tr]) _ty = [ C.BlockStm [cstm| return $(codegenTriv tr); |] ]
codegenTail (RetValsT ts) ty =
    [ C.BlockStm [cstm| return $(C.CompoundLit ty args noLoc); |] ]
    where args = map (\a -> (Nothing,C.ExpInitializer (codegenTriv a) noLoc)) ts
codegenTail (Switch _tr [] Nothing) _ty = []
codegenTail (Switch _tr [] (Just t)) ty =
    codegenTail t ty
codegenTail (Switch _tr [(_ctag,ctail)] Nothing) ty =
    codegenTail ctail ty
codegenTail (Switch tr ((ctag,ctail):cs) def) ty =
    [ C.BlockStm $ C.If comp thenTail elseTail noLoc ]
    where comp = [cexp| $(codegenTriv tr) == $ctag |]
          thenTail = mkBlock $ codegenTail ctail ty
          elseTail = if (null cs) then Nothing else Just $ mkBlock $ codegenTail (Switch tr cs def) ty
codegenTail (TailCall v ts) _ty =
    [ C.BlockStm [cstm| return $( C.FnCall (cid v) args noLoc); |] ]
    where args = map codegenTriv ts
codegenTail (LetCallT bnds rator rnds typ (Just nam) bod) ty =
    let init = [ C.BlockDecl [cdecl| $ty:(codegenTy typ) $id:nam = $(C.FnCall (cid rator) (map codegenTriv rnds) noLoc); |] ]
        assn t x y = C.BlockDecl [cdecl| $ty:t $id:x = $exp:y; |]
        bind (v,t) f = assn (codegenTy t) v (C.Member (cid nam) (C.toIdent f noLoc) noLoc)
        fields = map (\(_,i) -> "field" ++ (show i)) $ zip bnds [0..]
    in init ++ (zipWith bind bnds fields) ++ (codegenTail bod ty)
codegenTail (LetPrimCallT bnds prim rnds bod) ty =
    let bod' = codegenTail bod ty
        pre  = case prim of
                 AddP -> let [(outV,outT)] = bnds
                             [pleft,pright] = rnds
                         in [ C.BlockDecl [cdecl| $ty:(codegenTy outT) $id:outV = $(codegenTriv pleft) + $(codegenTriv pright); |] ]
                 SubP -> let (outV,outT) = head bnds
                             [pleft,pright] = rnds
                         in [ C.BlockDecl [cdecl| $ty:(codegenTy outT) $id:outV = $(codegenTriv pleft) - $(codegenTriv pright); |] ]
                 MulP -> let [(outV,outT)] = bnds
                             [pleft,pright] = rnds
                         in [ C.BlockDecl [cdecl| $ty:(codegenTy outT) $id:outV = $(codegenTriv pleft) * $(codegenTriv pright); |]]
                 DictInsertP -> undefined
                 DictLookupP -> undefined
                 NewBuf -> undefined
                 WriteTag -> let [(outV,CursorTy)] = bnds
                                 [(TagTriv tag),(VarTriv cur)] = rnds
                             in [ C.BlockStm [cstm| *($id:cur) = $tag; |]
                                , C.BlockDecl [cdecl| char* $id:outV = $id:cur + 1; |] ]
                 WriteInt -> let [(outV,CursorTy)] = bnds
                                 [val,(VarTriv cur)] = rnds
                             in [ C.BlockStm [cstm| *(int*)($id:cur) = $(codegenTriv val); |]
                                , C.BlockDecl [cdecl| char* $id:outV = ($id:cur) + sizeof(int); |] ]
                 ReadTag -> let [(tagV,TagTy),(curV,CursorTy)] = bnds
                                [(VarTriv cur)] = rnds
                            in [ C.BlockDecl [cdecl| $ty:(codegenTy TagTy) $id:tagV = *($id:cur); |]
                               , C.BlockDecl [cdecl| char* $id:curV = $id:cur + 1; |] ]
                 ReadInt -> let [(valV,IntTy),(curV,CursorTy)] = bnds
                                [(VarTriv cur)] = rnds
                            in [ C.BlockDecl [cdecl| int $id:valV = *(int*)($id:cur); |]
                               , C.BlockDecl [cdecl| char* $id:curV = ($id:cur) + sizeof(int); |] ]
    in pre ++ bod'

codegenTy :: Ty -> C.Type
codegenTy IntTy = [cty|int|]
codegenTy TagTy = [cty|int|]
codegenTy SymTy = [cty|int|]
codegenTy CursorTy = [cty|char*|]
codegenTy (ProdTy ts) = C.Type (C.DeclSpec [] [] (C.Tnamed (C.Id nam noLoc) [] noLoc) noLoc) (C.DeclRoot noLoc) noLoc 
    where nam = makeName ts
codegenTy (SymDictTy _t) = undefined

makeStructs :: [Ty] -> [C.Definition]
makeStructs [] = []
makeStructs ((ProdTy ts):ts') = 
    let d     = [cedecl| typedef struct $id:(nam ++ "_struct") { $sdecls:decls } $id:nam; |]
        nam   = makeName ts
        decls = map (\(t,n) -> [csdecl| $ty:(codegenTy t) $id:("field"++(show n)); |]) $ zip ts [0..]
    in d : (makeStructs ts')
makeStructs (_:ts) = makeStructs ts

makeName :: [Ty] -> String
makeName [] = "Prod"
makeName (IntTy:ts) = "Int" ++ makeName ts
makeName (CursorTy:ts) = "Cursor" ++ makeName ts
makeName _ = undefined
                                                        
mkBlock :: [C.BlockItem] -> C.Stm
mkBlock ss = C.Block ss noLoc

cid :: Var -> C.Exp
cid v = C.Var (C.toIdent v noLoc) noLoc

leafTag :: Word8
leafTag = fromIntegral 0

nodeTag :: Word8
nodeTag = fromIntegral 1

exadd1 :: FunDecl
exadd1 = FunDecl "add1" [("t",CursorTy),("tout",CursorTy)] (ProdTy [CursorTy,CursorTy]) exadd1Tail 

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
            $ LetPrimCallT [("n1",IntTy)] AddP [VarTriv "n", IntTriv 1]
            $ LetPrimCallT [("tout3",CursorTy)] WriteInt [VarTriv "n1", VarTriv "tout2"]
            $ RetValsT [VarTriv "t3", VarTriv "tout3"]
          nodeCase =
              LetPrimCallT [("tout2",CursorTy)] WriteTag [TagTriv nodeTag, VarTriv "tout"]
            $ LetCallT [("t3",CursorTy),("tout3",CursorTy)] "add1" [VarTriv "t2", VarTriv "tout2"] (ProdTy [CursorTy,CursorTy]) (Just "tmp1")
            $ TailCall "add1" [VarTriv "t3", VarTriv "tout3"]

add1C = mapM_ (\c -> putDocLn $ ppr c) (codegenFun exadd1)
