{-# LANGUAGE OverloadedStrings #-}
module Packed.FirstOrder.Passes.LLVM.Codegen where

import Packed.FirstOrder.Passes.LLVM.Monad
import Packed.FirstOrder.L3_Target
import Packed.FirstOrder.Common (Var(..), fromVar)
import Data.Char (ord)
import Data.Word
import qualified Packed.FirstOrder.Passes.LLVM.Global as LG

import Control.Monad.State
import Control.Monad.Except

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.Context as CTX
import qualified LLVM.General.Module as M
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Instruction as I
import qualified LLVM.General.AST.Linkage as L
import qualified LLVM.General.AST.Type as T


toLLVM :: AST.Module -> IO String
toLLVM m = CTX.withContext $ \ctx -> do
    errOrLLVM <- runExceptT $ M.withModuleFromAST ctx m M.moduleLLVMAssembly
    case errOrLLVM of
      Left err -> error $ "error: " ++ err
      Right llvm -> return llvm


-- | Generate LLVM instructions for Prog
--
codegenProg :: Prog -> IO String
codegenProg prog = do
  cg' <- return $ genModule $ codegenProg' prog
  llvm <-  toLLVM cg'
  putStrLn llvm
  return llvm

-- TODO(cskksc): abstract out main fn generation. it'll will help in generating
-- more fns. print_T needs it right now
codegenProg' :: Prog -> CodeGen ()
codegenProg' (Prog _ body) = do
  declare LG.puts
  declare LG.printInt
  declare LG.globalSizeParam
  declare (LG.mainFn mainBody)
    where
      mainBody :: [G.BasicBlock]
      mainBody = genBlocks $ do
        _ <- mainBody'
        createBlocks

      mainBody' :: CodeGen BlockState
      mainBody' = do
        entry <- newBlock "entry"
        setBlock entry
        case body of
          Just (PrintExp t) -> codegenTail t
          _ -> retval_ (AST.ConstantOperand (C.Int 8 8))

-- | Generate LLVM instructions for Tail
--
codegenTail :: Tail -> CodeGen BlockState
codegenTail (RetValsT []) = return_
codegenTail (RetValsT [t]) = do
  t' <- codegenTriv t
  retval_ t'

codegenTail (LetPrimCallT bnds prm rnds body) = do
  rnds' <- mapM codegenTriv rnds
  _     <- case prm of
             PrintInt -> do
               _ <- call L.External LG.printIntType (AST.Name "gibbon_print_int") rnds'
               return_
             PrintString s -> do
               _ <- printString s
               return_
             AddP -> do
               addp bnds rnds'
             SizeParam -> do
               sizeParam bnds
             _ -> __
  bod' <- codegenTail body
  return bod'

codegenTail _ = __

-- | Generate LLVM instructions for Triv
--
codegenTriv :: Triv -> CodeGen AST.Operand
codegenTriv (IntTriv i) = return $ AST.ConstantOperand $ C.Int 32 (toInteger i)
codegenTriv (VarTriv v) = do
  nm <- return $ fromVar v
  getvar nm
codegenTriv _ = __


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
  _   <- call L.External LG.printIntType (AST.Name "gibbon_fputs") [localRef (toPtrType ty) (AST.UnName nm)]
  return_
    where (chars, len) = stringToChar s
          ty    = T.ArrayType len T.i8
          idx   = AST.ConstantOperand (C.Int 32 0)
          idxs  = [idx, idx]


-- | Convert string to a char array in LLVM format
--
stringToChar :: String -> (AST.Operand, Word64)
stringToChar s = (AST.ConstantOperand $ C.Array ty chars, len)
  where len   = fromIntegral $ length chars
        chars = (++ [C.Int 8 0]) $ map (\x -> C.Int 8 (toInteger x)) $ map ord s
        ty    = T.IntegerType 8


-- | Gibbon AddP instruction
--
addp :: [(Var,Ty)] -> [AST.Operand] -> CodeGen BlockState
addp [] rnds = do
  add rnds
  return_
addp [(v, ty)] rnds = do
  nm  <- return $ fromVar v
  var <- namedAdd nm (toLLVMTy ty) rnds
  retval_ var

-- | Convert Gibbon types to LLVM types
--
toLLVMTy :: Ty -> T.Type
toLLVMTy IntTy = T.IntegerType 64
toLLVMTy _ = __


-- | Gibbon SizeParam instruction
--
sizeParam :: [(Var,Ty)] -> CodeGen BlockState
sizeParam [(v,ty)] = do
  nm <- return $ fromVar v
  _  <- namedLoad nm lty op
  return_
  where lty = (toLLVMTy ty)
        op = globalOp lty (AST.Name "global_size_param")

-- | tests
--
testprog0 = Prog {fundefs = [], mainExp = Nothing}
test0 = codegenProg testprog0
testprog1 = Prog {fundefs = [], mainExp = Just (PrintExp (LetPrimCallT {binds = [], prim = PrintInt, rands = [IntTriv 42], bod = LetPrimCallT {binds = [], prim = PrintString "\n", rands = [], bod = RetValsT []}}))}
test1 = codegenProg testprog1
testprog2 = Prog {fundefs = [], mainExp = Just (PrintExp (LetPrimCallT {binds = [(Var "flt0",IntTy)], prim = AddP, rands = [IntTriv 10,IntTriv 40], bod = LetPrimCallT {binds = [], prim = PrintInt, rands = [VarTriv (Var "flt0")], bod = LetPrimCallT {binds = [], prim = PrintString "\n", rands = [], bod = RetValsT []}}}))}
test2 = codegenProg testprog2

testprog3 = Prog {fundefs = [], mainExp = Just (PrintExp (LetPrimCallT {binds = [(Var "flt0",IntTy)], prim = SizeParam, rands = [], bod = LetPrimCallT {binds = [], prim = PrintInt, rands = [VarTriv (Var "flt0")], bod = LetPrimCallT {binds = [], prim = PrintString "\n", rands = [], bod = RetValsT []}}}))}
test3 = codegenProg testprog3
