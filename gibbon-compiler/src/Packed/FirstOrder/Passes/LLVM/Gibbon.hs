module Packed.FirstOrder.Passes.LLVM.Gibbon (
    addp, subp, mulp, eqp, callp
  , addStructs, sizeParam, toLLVMTy, printString, toIfPred
  , readTag, readInt, sizeof, structName, structTy, toPtrTy
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
addStructs prog = mapM_ ((\(nm,d) -> addDefinition nm d) . makeStruct) $
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
-- char tagV = *cur;
-- char *curV = cur + 1;
--
readTag :: [(Var,Ty)] -> [AST.Operand] -> CodeGen BlockState
readTag [(tagV,TagTyPacked),(curV,CursorTy)] [cur] =
  let tagTy = toLLVMTy TagTyPacked -- char
      tagVV = fromVar tagV
      curTy = toLLVMTy CursorTy -- char*
      curVV = fromVar curV
  in do
    -- store the input char* somewhere
    a <- allocate curTy Nothing
    _ <- store a cur
    b <- load curTy Nothing a

    -- char tagV = *cur;
    c <- load tagTy Nothing b
    tagV' <- allocate tagTy Nothing
    _ <- store tagV' c
    _ <- load tagTy (Just tagVV) tagV'

    -- char *curV = cur + 1
    _ <- load curTy Nothing b
    d <- getElemPtr True b [constop_ $ int_ 1]
    curV' <- allocate curTy Nothing
    _ <- store curV' d
    _ <- load curTy (Just curVV) curV'

    return_


-- | Read an 8 byte Int from the cursor and advance
--
-- long long valV = *(long long *) cur;
-- char *curV = cur + sizeof(long long);
--
readInt :: [(Var,Ty)] -> [AST.Operand] -> CodeGen BlockState
readInt [(valV,IntTy),(curV,CursorTy)] [cur] =
  let valTy = toLLVMTy IntTy
      valVV = fromVar valV
      curTy = toLLVMTy CursorTy
      curVV = fromVar curV
  in do
    -- store the input char* somewhere
    a <- allocate curTy Nothing
    _ <- store a cur
    c <- load curTy Nothing a -- %4

    -- long long valV = *(long long *) cur;
    d <- bitcast (toPtrTy valTy) c
    e <- load valTy Nothing d

    valV' <- allocate valTy Nothing
    _ <- store valV' e
    _ <- load valTy (Just valVV) valV'

    -- char *curV = cur + sizeof(long long);
    curV' <- allocate curTy Nothing
    -- TODO(cskksc): remove hard coded 8
    f <- getElemPtr True c [constop_ $ int_ 8]
    _ <- store curV' f
    _ <- load curTy (Just curVV) curV'
    return_


-- |
sizeof :: T.Type -> CodeGen AST.Operand
sizeof ty = do
  a <- getElemPtr True (AST.ConstantOperand $ C.Null ty) [constop_ $ int_ 1]
  ptrToInt Nothing [a]


-- |
structName :: [Ty] -> String
structName tys = "struct." ++ makeName tys


-- |
structTy :: [Ty] -> T.Type
structTy = (T.NamedTypeReference . AST.Name . structName)
