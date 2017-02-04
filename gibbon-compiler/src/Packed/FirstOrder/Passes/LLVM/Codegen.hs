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
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Instruction as I
import qualified LLVM.General.AST.Type as T
import qualified LLVM.General.AST.IntegerPredicate as IP
import qualified LLVM.General.Context as CTX
import qualified LLVM.General.Module as M


toLLVM :: AST.Module -> IO String
toLLVM m = CTX.withContext $ \ctx -> do
    errOrLLVM <- runExceptT $ M.withModuleFromAST ctx m M.moduleLLVMAssembly
    case errOrLLVM of
      Left err -> error $ "error: " ++ err
      Right llvm -> return llvm


-- | Generate LLVM instructions for Prog
--
codegenProg :: Bool -> Prog -> IO String
codegenProg _ prog = do
  cg' <- return $ genModule $ codegenProg' prog
  llvm <-  toLLVM cg'
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
               _ <- call LG.printInt rnds'
               return_
             PrintString s -> do
               _ <- printString s
               return_
             AddP -> do
               addp bnds rnds'
             SizeParam -> do
               sizeParam bnds
             EqP -> do
               eqp bnds rnds'
             _ -> __
  bod' <- codegenTail body
  return bod'

codegenTail (IfT test consq els') = do
  _ <- ifThenElse (genIfPred test) (codegenTail consq) (codegenTail els')
  return_

codegenTail _ = __

-- | Generate LLVM instructions for Triv
--
codegenTriv :: Triv -> CodeGen AST.Operand
codegenTriv (IntTriv i) = return $ AST.ConstantOperand $ C.Int 64 (toInteger i)
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
  _   <- call LG.puts [localRef (toPtrType ty) (AST.UnName nm)]
  return_
    where (chars, len) = stringToChar s
          ty    = T.ArrayType len T.i8
          idx   = AST.ConstantOperand (C.Int 32 0)
          idxs  = [idx, idx]


-- | Gibbon AddP primitive
--
addp :: [(Var,Ty)] -> [AST.Operand] -> CodeGen BlockState
addp [] [x,y] = do
  _ <- add x y
  return_
addp [(v, _)] [x,y] = do
  nm  <- return $ fromVar v
  var <- namedAdd nm x y
  retval_ var


-- | Gibbon EqP primitive
--
eqp :: [(Var, Ty)] -> [AST.Operand] -> CodeGen BlockState
eqp [] [x,y] = do
  _ <- eq x y
  return_
eqp [(v,_)] [x,y] = do
  nm  <- return $ fromVar v
  var <- namedEq nm x y
  retval_ var


-- | Gibbon SizeParam primitive
--
sizeParam :: [(Var,Ty)] -> CodeGen BlockState
sizeParam [(v,ty)] = do
  nm <- return $ fromVar v
  _  <- namedLoad nm lty op
  return_
  where lty = (toLLVMTy ty)
        op = globalOp lty (AST.Name "global_size_param")


-- | Convert Gibbon types to LLVM types
--
toLLVMTy :: Ty -> T.Type

toLLVMTy _ = __


-- | Convert string to a char array in LLVM format
--
stringToChar :: String -> (AST.Operand, Word64)
stringToChar s = (AST.ConstantOperand $ C.Array T.i8 chars, len)
  where len   = fromIntegral $ length chars
        chars = (++ [C.Int 8 0]) $ map (\x -> C.Int 8 (toInteger x)) $ map ord s


-- | Generate the correct LLVM predicate
-- We implement the C notion of true/false i.e every !=0 value is truthy
--
genIfPred :: Triv -> CodeGen AST.Operand
genIfPred triv =
  let op0 = case triv of
              (IntTriv i) -> AST.ConstantOperand $ C.Int 64 (toInteger i)
              _ -> __
      z   = AST.ConstantOperand $ C.Int 64 0
  in
    neq op0 z


-- | tests
--
testprog0 = Prog {fundefs = [], mainExp = Nothing}
test0 = codegenProg False testprog0
testprog1 = Prog {fundefs = [], mainExp = Just (PrintExp (LetPrimCallT {binds = [], prim = PrintInt, rands = [IntTriv 42], bod = LetPrimCallT {binds = [], prim = PrintString "\n", rands = [], bod = RetValsT []}}))}
test1 = codegenProg False testprog1
testprog2 = Prog {fundefs = [], mainExp = Just (PrintExp (LetPrimCallT {binds = [(Var "flt0",IntTy)], prim = AddP, rands = [IntTriv 10,IntTriv 40], bod = LetPrimCallT {binds = [], prim = PrintInt, rands = [VarTriv (Var "flt0")], bod = LetPrimCallT {binds = [], prim = PrintString "\n", rands = [], bod = RetValsT []}}}))}
test2 = codegenProg False testprog2
testprog3 = Prog {fundefs = [], mainExp = Just (PrintExp (LetPrimCallT {binds = [(Var "flt0",IntTy)], prim = SizeParam, rands = [], bod = LetPrimCallT {binds = [], prim = PrintInt, rands = [VarTriv (Var "flt0")], bod = LetPrimCallT {binds = [], prim = PrintString "\n", rands = [], bod = RetValsT []}}}))}
test3 = codegenProg False testprog3
testprog4 = Prog {fundefs = [], mainExp = Just (PrintExp (IfT {tst = IntTriv 1, con = LetPrimCallT {binds = [], prim = PrintString "#t", rands = [], bod = LetPrimCallT {binds = [], prim = PrintString "\n", rands = [], bod = RetValsT []}}, els = LetPrimCallT {binds = [], prim = PrintString "#f", rands = [], bod = LetPrimCallT {binds = [], prim = PrintString "\n", rands = [], bod = RetValsT []}}}))}
test4 = codegenProg False testprog4
