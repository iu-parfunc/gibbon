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
import qualified LLVM.General.AST.Linkage as L
import qualified LLVM.General.AST.Type as T


toLLVM :: AST.Module -> IO String
toLLVM m = CTX.withContext $ \ctx -> do
    errOrLLVM <- runExceptT $ M.withModuleFromAST ctx m M.moduleLLVMAssembly
    case errOrLLVM of
      Left err -> error $ "error: " ++ err
      Right llvm -> return llvm

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
  declare LC.puts
  declare LC.printInt
  declare (LC.mainFn mainBody)
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


codegenTail :: Tail -> CodeGen BlockState
codegenTail (RetValsT []) = return_
codegenTail (RetValsT [t]) = retval_ $ codegenTriv t

codegenTail (LetPrimCallT bnds prm rnds body) = do
  rnds' <- return $ map codegenTriv rnds
  pre   <- case prm of
             PrintInt -> do
               _ <- call L.External LC.printIntType (AST.Name "print_int") rnds'
               return_
             PrintString s -> do
               _ <- printString s
               return_
             _ -> __
  bod' <- codegenTail body
  return bod'

codegenTail _ = __


codegenTriv :: Triv -> AST.Operand
codegenTriv (IntTriv i) = AST.ConstantOperand $ C.Int 32 (toInteger i)
codegenTriv _ = __


testprog0 = Prog {fundefs = [], mainExp = Nothing}
test0 = codegenProg testprog0

-- testprog1 = Prog {fundefs = [], mainExp = Just (PrintExp (LetPrimCallT {binds = [], prim = PrintInt, rands = [IntTriv 42], bod = LetPrimCallT {binds = [], prim = PrintInt, rands = [IntTriv 23], bod = RetValsT []}}))}
testprog1 = Prog {fundefs = [], mainExp = Just (PrintExp (LetPrimCallT {binds = [], prim = PrintInt, rands = [IntTriv 42], bod = LetPrimCallT {binds = [], prim = PrintString "\n", rands = [], bod = RetValsT []}}))}
test1 = codegenProg testprog1
