module Packed.FirstOrder.Passes.LLVM.Codegen where

import Packed.FirstOrder.Passes.LLVM.Monad
import qualified Packed.FirstOrder.Passes.LLVM.C as LC
import Packed.FirstOrder.L3_Target

import Control.Monad.Except

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.Context as CTX
import qualified LLVM.General.Module as M
import qualified LLVM.General.AST.Constant as C


toLLVM :: AST.Module -> IO String
toLLVM m = CTX.withContext $ \ctx -> do
    errOrLLVM <- runExceptT $ M.withModuleFromAST ctx m M.moduleLLVMAssembly
    case errOrLLVM of
      Left err -> error $ "error: " ++ err
      Right llvm -> return llvm


codegenProg :: Prog -> CodeGen ()
codegenProg (Prog _ body) = do
  declare LC.puts
  declare (LC.mainFn mainBody)
    where
      mainBody :: [G.BasicBlock]
      mainBody = genBlocks $ do
        mainBody'
        createBlocks

      mainBody' :: CodeGen BlockState
      mainBody' = do
        entry <- newBlock "entry"
        setBlock entry
        case body of
          Just (PrintExp t) -> codegenTail t
          _ -> retval_ (AST.ConstantOperand (C.Int 8 8))


codegenTail :: Tail -> CodeGen BlockState
codegenTail (RetValsT []) = return_
codegenTail (RetValsT [t]) = retval_ $ codegenTriv t
codegenTail tail = __


codegenTriv :: Triv -> AST.Operand
codegenTriv (IntTriv i) = AST.ConstantOperand $ C.Int 32 (toInteger i)
codegenTriv t = __


testprog0 = Prog {fundefs = [], mainExp = Nothing}
testgen0 = genModule $ codegenProg testprog0
test0 = toLLVM testgen0 >>= \s -> putStrLn s
