module Packed.FirstOrder.Passes.LLVM.Gibbon (
    addp, subp, mulp, eqp, callp
  , addStructs, sizeParam, toLLVMTy, printString, toIfPred
  , readTag, readInt, sizeof, structName, structTy, toPtrTy, convert, assign
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
  var   <- op (Just nm) args
  retval_ var


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
  where lty = toLLVMTy ty
        op  = globalOp lty (AST.Name "global_size_param")


-- | Convert Gibbon types to LLVM types
--
toLLVMTy :: Ty -> T.Type
toLLVMTy IntTy       = T.i64                 -- ^ long long
toLLVMTy TagTyPacked = T.i8                  -- ^ char
toLLVMTy TagTyBoxed  = toLLVMTy IntTy        -- ^ long long
toLLVMTy SymTy       = T.i64                 -- ^ long long
toLLVMTy PtrTy       = toPtrTy T.i8          -- ^ char*
toLLVMTy CursorTy    = toPtrTy T.i8          -- ^ char*
toLLVMTy (ProdTy []) = toPtrTy T.VoidType    -- ^ void*
toLLVMTy (ProdTy ts) = T.StructureType False $ map toLLVMTy ts
toLLVMTy (SymDictTy _t) = __


-- | Convert the type to a pointer type
--
toPtrTy :: T.Type -> T.Type
toPtrTy ty = T.PointerType ty (AS.AddrSpace 0)


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
  _   <- call LG.puts Nothing [localRef (toPtrTy ty) (AST.UnName nm)]
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


-- | Add all required structs
--
addStructs :: Prog -> CodeGen ()
addStructs prog = mapM_ ((\(nm,d) -> addTypeDef nm d) . makeStruct) $
                  harvestStructTys prog


-- | Generate LLVM type definitions (structs)
--
makeStruct :: [Ty] -> (String, AST.Definition)
makeStruct [] = __
makeStruct tys = (nm, AST.TypeDefinition (AST.Name nm) (Just $ T.StructureType False elememtTypes))
  where nm = structName tys
        elememtTypes = map toLLVMTy tys


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
readCursor [(valV', valTy'), (curV', curTy')] cur offset =
  let valTy = toLLVMTy valTy'
      valV = fromVar valV'
      curTy = toLLVMTy curTy'
      curV = fromVar curV'
  in do
    cur' <- assign Nothing curTy cur

    -- valTy valV = *cur
    valVV <- load valTy Nothing cur' >>= convert valTy
    _ <- assign (Just valV) valTy valVV

    -- curTy curV = cur + offset;
    curVV <- getElemPtr True cur' [constop_ $ int_ offset]
    _ <- assign (Just curV) curTy curVV
    return_


-- | ty _var_ = val
--
assign :: Maybe String -> T.Type -> AST.Operand -> CodeGen AST.Operand
assign nm ty val = do
  x <- allocate ty nm
  _ <- store x val
  load ty nm x


-- | Generate instructions to convert op from type-of-op -> toTy
--
convert :: T.Type -> AST.Operand -> CodeGen AST.Operand
convert toTy op
  | intP toTy && intP fromTy = sext Nothing toTy op
  | otherwise                = bitcast Nothing toTy op
  where fromTy = getOpTy op
        intP = (`elem` [T.i8, T.i32, T.i64])


-- |
getOpTy :: AST.Operand -> T.Type
getOpTy (AST.LocalReference ty' _) = ty'
getOpTy (AST.ConstantOperand c) = getConstTy c
getOpTy op = error $ "getOpTy: Not implemented " ++ show op


-- |
getConstTy :: C.Constant -> T.Type
getConstTy (C.Int bits _) = case bits of
                                8  -> T.i8
                                32 -> T.i32
                                64 -> T.i64

-- |
sizeof :: T.Type -> CodeGen AST.Operand
sizeof ty = do
  a <- getElemPtr True (AST.ConstantOperand $ C.Null ty) [constop_ $ int_ 1]
  ptrToInt Nothing a


-- |
structName :: [Ty] -> String
structName tys = "struct." ++ makeName tys


-- |
structTy :: [Ty] -> T.Type
structTy = (T.NamedTypeReference . AST.Name . structName)
