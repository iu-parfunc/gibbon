{-# LANGUAGE FlexibleInstances #-}
module Packed.FirstOrder.Passes.LLVM.Gibbon (
    addp, subp, mulp, eqp, callp
  , sizeParam, typeOf, printString, toIfPred
  , readTag, readInt, sizeof, convert
  , addStructs, structName, populateStruct, unpackStruct
) where

-- | standard library
import Data.Word (Word64)
import Control.Monad.State

-- | gibbon internals
import Packed.FirstOrder.L3_Target
import Packed.FirstOrder.Passes.Codegen (harvestStructTys, makeName)
import Packed.FirstOrder.Common (fromVar)
import Packed.FirstOrder.Passes.LLVM.Monad
import Packed.FirstOrder.Passes.LLVM.Instruction
import Packed.FirstOrder.Passes.LLVM.Terminator
import Packed.FirstOrder.Passes.LLVM.Type
import qualified Packed.FirstOrder.Passes.LLVM.Global as LG

-- | llvm-general
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Type as T
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.AST.AddrSpace as AS


-- | Allow results of LLVM operations to be assigned to variables, instead of unnames
--

gibbonOp :: (Maybe String -> [AST.Operand] -> CodeGen AST.Operand)
            -> [(Var,Ty)] -> [AST.Operand]
            -> CodeGen BlockState
gibbonOp op [] args = do
  _ <- op Nothing args
  return_

gibbonOp op [(v, _)] args = do
  let nm = fromVar v
  res   <- op (Just nm) args
  retval_ res

gibbonOp op bnds args = do
  struct <- op Nothing args
  unpackStruct Nothing struct bnds >>= retval_

gibbonOp op vars args = error $ "gibbonOp: Not implemented " ++ show vars

addp :: [(Var, Ty)] -> [AST.Operand] -> CodeGen BlockState
addp = gibbonOp add

mulp :: [(Var, Ty)] -> [AST.Operand] -> CodeGen BlockState
mulp = gibbonOp mul

subp :: [(Var, Ty)] -> [AST.Operand] -> CodeGen BlockState
subp = gibbonOp sub

eqp :: [(Var, Ty)] -> [AST.Operand] -> CodeGen BlockState
eqp  = gibbonOp eq

callp :: G.Global -> [(Var, Ty)] -> [AST.Operand] -> CodeGen BlockState
callp fn = gibbonOp (call fn)


-- | Gibbon SizeParam primitive
--
sizeParam :: [(Var,Ty)] -> CodeGen BlockState
sizeParam [(v,ty)] = do
  let nm = fromVar v
  _     <- load lty (Just nm) op
  return_
  where lty = typeOf ty
        op  = globalOp lty (AST.Name "global_size_param")


-- | Convert Gibbon types to LLVM types.
--
-- Should be defined in Type.hs. But this would be right place to
-- define it, if we ever convert this into an independent library
-- as an abstraction over llvm-general
--
instance TypeOf Ty where
  typeOf IntTy       = T.i64                 -- ^ long long
  typeOf TagTyPacked = T.i8                  -- ^ char
  typeOf TagTyBoxed  = typeOf IntTy          -- ^ long long
  typeOf SymTy       = T.i64                 -- ^ long long
  typeOf PtrTy       = toPtrTy T.i8          -- ^ char*
  typeOf CursorTy    = toPtrTy T.i8          -- ^ char*
  typeOf (ProdTy []) = T.VoidType            -- ^ void (pointers to void are invalid in LLVM)
  typeOf (ProdTy ts) = T.StructureType False $ map typeOf  ts
  typeOf (SymDictTy _t) = __

-- | struct types
instance TypeOf [Ty] where
  typeOf = T.NamedTypeReference . AST.Name . structName


-- | Gibbon PrintString
--
printString :: String -> CodeGen BlockState
printString s = do
  var <- allocate ty Nothing
  _   <- store var chars
  nm  <- gets next
  -- TODO(cskksc): figure out the -2. its probably because store doesn't assign
  -- anything to an unname
  _   <- getElemPtr True (localRef (toPtrTy ty) (AST.UnName (nm - 2))) idxs
  _   <- call LG.fputs Nothing [localRef (toPtrTy ty) (AST.UnName nm)]
  return_
    where (chars, len) = stringToChar s
          ty    = T.ArrayType len T.i8
          idx   = (constop_ . int_) 0
          idxs  = [idx, idx]


-- | Convert string to a char array in LLVM format
--
stringToChar :: String -> (AST.Operand, Word64)
stringToChar s = (constop_ $ string_ s', len)
  where len = (fromIntegral . length) s'
        s'  = s ++ ['\NUL']

-- | Generate the correct LLVM predicate
-- We implement the C notion of true/false i.e every value !=0 is truthy
--
toIfPred :: Triv -> CodeGen AST.Operand
toIfPred triv =
  let op0 = case triv of
              (IntTriv i) -> (constop_ . int_ . toInteger) i
              _ -> __
      z   = (constop_ . int_) 0
  in
    neq Nothing [op0,z]


-- | Read one byte from the cursor and advance it
--
-- TagTyPacked tagV = *(IntTy/TagTyPacked *) cur;
-- CursorTy curV = cur + 1;
--
readTag :: [(Var,Ty)] -> [AST.Operand] -> CodeGen BlockState
readTag [(tagV,TagTyPacked),(curV,CursorTy)] [cur] =
  readCursor [(tagV, TagTyPacked),(curV, CursorTy)] cur 1


-- | Read an 8 byte Int from the cursor and advance
--
-- (IntTy/TagTyPacked) valV = *(IntTy/TagTyPacked *) cur
-- CursorTy curV = cur + sizeof(IntTy)
--
readInt :: [(Var,Ty)] -> [AST.Operand] -> CodeGen BlockState
readInt [(valV,valTy),(curV,CursorTy)] [cur] =
  readCursor [(valV,valTy), (curV,CursorTy)] cur 8


readCursor :: [(Var, Ty)] -> AST.Operand -> Integer -> CodeGen BlockState
readCursor [(valV', valTy'), (curV', curTy')] cur' offset =
  let valTy = typeOf valTy'
      valV = fromVar valV'
      curTy = typeOf curTy'
      curV = fromVar curV'
  in do
    cur <- assign curTy Nothing cur'

    -- valTy valV = *cur
    valVV <- load valTy Nothing cur >>= convert valTy Nothing
    _ <- assign valTy (Just valV) valVV

    -- curTy curV = cur + offset;
    curVV <- getElemPtr True cur [constop_ $ int_ offset]
    _ <- assign curTy (Just curV) curVV
    return_


-- | Generate instructions to convert op from type-of-op -> toTy
--
convert :: T.Type -> Maybe String -> AST.Operand -> CodeGen AST.Operand
convert toTy nm op
  | intP toTy && intP fromTy = sext toTy nm op
  | otherwise                = bitcast toTy nm op
  where fromTy = typeOf op
        intP = (`elem` [T.i8, T.i32, T.i64])

-- |
sizeof :: T.Type -> CodeGen AST.Operand
sizeof ty = getElemPtr True (constop_ $ C.Null ty) [constop_ $ int_ 1] >>= ptrToInt Nothing


-- | Add all required structs
--
addStructs :: Prog -> CodeGen ()
addStructs prog = mapM_ (uncurry addTypeDef . defineStruct) $
                  harvestStructTys prog


-- | Generate LLVM type definitions (structs)
--
defineStruct :: [Ty] -> (String, AST.Definition)
defineStruct [] = __
defineStruct tys = (nm, AST.TypeDefinition (AST.Name nm) (Just $ T.StructureType False elememtTypes))
  where nm = structName tys
        elememtTypes = map typeOf tys


-- | Return a reference to the struct, with its fields assigned to triv's
--
populateStruct :: T.Type -> Maybe String -> [AST.Operand] -> CodeGen AST.Operand
populateStruct ty nm ts = do
  struct <- allocate ty nm
  forM_ (zip ts [0..]) $ \(triv,i) -> do
    -- When indexing into a (optionally packed) structure, only i32 integer
    -- constants are allowed
    field <- getElemPtr True struct [constop_ $ int32_ 0, constop_ $ int32_ i]
    store field triv
  return struct


-- | Store members of struct in bound vars (bnds). Reverse of populatestruct
--
unpackStruct :: Maybe String -> AST.Operand -> [(Var, Ty)] -> CodeGen AST.Operand
unpackStruct nm struct bnds = do
  structTy <- case bnds of
                [] -> return $ typeOf struct
                _  -> return $ typeOf $ map snd bnds
  struct' <- convert (toPtrTy structTy) Nothing struct
  forM_ (zip bnds [0..]) $ \((v,vty), i) -> do
    field <- getElemPtr True struct' [constop_ $ int32_ 0, constop_ $ int32_ i]
    field' <- load (typeOf vty) Nothing field
    assign (typeOf vty) (Just $ fromVar v) field'
  return struct'


-- |
structName :: [Ty] -> String
structName tys = "struct." ++ makeName tys
