module Packed.FirstOrder.Passes.LLVM.Gibbon (
    addp, subp, mulp, eqp, callp
  , sizeParam, toLLVMTy, printString, toIfPred
  , addStructs
) where

-- | standard library
import Data.Char (ord)
import Data.Word (Word64)
import Data.Maybe (maybeToList)
import Control.Monad.State
import qualified Data.Set as S


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


-- | Allow named results for LLVM operations
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
  _   <- call LG.puts Nothing [localRef (toPtrType ty) (AST.UnName nm)]
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
makeStruct ts = (nm, AST.TypeDefinition (AST.Name nm) (Just $ T.StructureType False elememtTypes))
  where nm = "struct." ++ makeName ts
        elememtTypes = map toLLVMTy ts
