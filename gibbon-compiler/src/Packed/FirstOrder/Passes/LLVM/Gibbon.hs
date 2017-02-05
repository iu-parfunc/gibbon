module Packed.FirstOrder.Passes.LLVM.Gibbon (
  addp, subp, mulp, eqp
  , sizeParam, toLLVMTy, printString, genIfPred
) where

-- | standard library
import Data.Char (ord)
import Data.Word (Word64)
import Control.Monad.State

-- | gibbon internals
import Packed.FirstOrder.L3_Target
import Packed.FirstOrder.Common (fromVar)
import Packed.FirstOrder.Passes.LLVM.Monad
import Packed.FirstOrder.Passes.LLVM.Instructions
import Packed.FirstOrder.Passes.LLVM.Terminators
import qualified Packed.FirstOrder.Passes.LLVM.Global as LG

-- | llvm-general
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Type as T


-- | Gibbon binary operations
--

gibbonBinop :: (AST.Operand -> AST.Operand -> CodeGen AST.Operand)
            -> (String -> AST.Operand -> AST.Operand -> CodeGen AST.Operand)
            -> [(Var,Ty)] -> [AST.Operand]
            -> CodeGen BlockState
gibbonBinop op namedOp [] [x,y] = do
  _ <- op x y
  return_
gibbonBinop op namedOp [(v, _)] [x,y] = do
  let nm = fromVar v
  var   <- namedOp nm x y
  retval_ var


addp = gibbonBinop add namedAdd

mulp = gibbonBinop mul namedMul

subp = gibbonBinop sub namedSub

eqp  = gibbonBinop eq namedEq


-- | Gibbon SizeParam primitive
--
sizeParam :: [(Var,Ty)] -> CodeGen BlockState
sizeParam [(v,ty)] = do
  let nm = fromVar v
  _     <- namedLoad nm lty op
  return_
  where lty = toLLVMTy ty
        op  = globalOp lty (AST.Name "global_size_param")


-- | Convert Gibbon types to LLVM types
--
toLLVMTy :: Ty -> T.Type
toLLVMTy IntTy = T.i64
toLLVMTy _ = __


-- | Gibbon PrintString
--
printString :: String -> CodeGen BlockState
printString s = do
  var <- allocate ty
  _   <- store var chars
  nm  <- gets next
  -- TODO(cskksc): figure out the -2. its probably because store doesn't assign
  -- anything to an unname
  _   <- getElemPtr True (localRef (toPtrType ty) (AST.UnName (nm - 2))) idxs
  _   <- call LG.puts [localRef (toPtrType ty) (AST.UnName nm)]
  return_
    where (chars, len) = stringToChar s
          ty    = T.ArrayType len T.i8
          idx   = AST.ConstantOperand (C.Int 32 0)
          idxs  = [idx, idx]


-- | Convert string to a char array in LLVM format
--
stringToChar :: String -> (AST.Operand, Word64)
stringToChar s = (AST.ConstantOperand $ C.Array T.i8 chars, len)
  where len   = fromIntegral $ length chars
        chars = (++ [C.Int 8 0]) $ map (C.Int 8 . toInteger . ord) s

-- | Generate the correct LLVM predicate
-- We implement the C notion of true/false i.e every value !=0 is truthy
--
genIfPred :: Triv -> CodeGen AST.Operand
genIfPred triv =
  let op0 = case triv of
              (IntTriv i) -> AST.ConstantOperand $ C.Int 64 (toInteger i)
              _ -> __
      z   = AST.ConstantOperand $ C.Int 64 0
  in
    neq op0 z
