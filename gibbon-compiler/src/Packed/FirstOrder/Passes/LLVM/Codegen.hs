{-# LANGUAGE OverloadedStrings #-}
module Packed.FirstOrder.Passes.LLVM.Codegen where

-- | standard library
import Control.Monad.Except

-- | gibbon internals
import Packed.FirstOrder.L3_Target
import Packed.FirstOrder.Common (Var(..), fromVar)
import Packed.FirstOrder.Passes.LLVM.Monad
import Packed.FirstOrder.Passes.LLVM.Instruction
import Packed.FirstOrder.Passes.LLVM.Terminator
import Packed.FirstOrder.Passes.LLVM.Gibbon
import Packed.FirstOrder.Passes.LLVM.Global

-- | llvm-general
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Constant as C
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
  let cg' = genModule $ codegenProg' prog
  toLLVM cg'

-- TODO(cskksc): abstract out main fn generation. it'll will help in generating
-- more fns. print_T needs it right now
codegenProg' :: Prog -> CodeGen ()
codegenProg' (Prog _ body) = do
  declare puts
  declare printInt
  declare globalSizeParam
  declare (mainFn mainBody)
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
               _ <- call printInt rnds'
               return_
             PrintString s -> do
               _ <- printString s
               return_
             AddP -> addp bnds rnds'
             SubP -> subp bnds rnds'
             MulP -> mulp bnds rnds'
             EqP  -> eqp bnds rnds'
             SizeParam -> sizeParam bnds
             _ -> __
  codegenTail body

codegenTail (IfT test consq els') = do
  _ <- ifThenElse (genIfPred test) (codegenTail consq) (codegenTail els')
  return_

codegenTail (Switch trv alts def) =
  let (alts', def') = case def of
                        Nothing -> let (rest,lastone) = splitAlts alts
                                   in  (rest, altTail lastone)
                        Just d  -> (alts, d)
      alts'' = case alts' of
                 IntAlts xs -> xs

        -- | Split alts to return a default case, in case switch was not given one
      splitAlts :: Alts -> (Alts, Alts)
      splitAlts (TagAlts ls) = (TagAlts (init ls), TagAlts [last ls])
      splitAlts (IntAlts ls) = (IntAlts (init ls), IntAlts [last ls])

      -- | Take a "singleton" Alts and extract the Tail.
      altTail :: Alts -> Tail
      altTail (TagAlts [(_,t)]) = t
      altTail (IntAlts [(_,t)]) = t

      dests :: [(C.Constant, AST.Name)]
      dests = map (\((casei,_), i) -> (C.Int 64 (toInteger casei), AST.Name $ "switch" ++ show i ++ ".case")) $ zip alts'' [1..]

  in
    do
      switchDefault <- newBlock "switch.default"

      trv' <- codegenTriv trv
      _ <- switch trv' (blockLabel switchDefault) dests

      -- generate alt blocks
      _ <- mapM (\(_,t) -> do
                    x <- newBlock "switch.case"
                    setBlock x
                    codegenTail t)
           alts''

      -- generate the default block
      setBlock switchDefault
      codegenTail def'


codegenTail _ = __

-- | Generate LLVM instructions for Triv
--
codegenTriv :: Triv -> CodeGen AST.Operand
codegenTriv (IntTriv i) = return $ AST.ConstantOperand $ C.Int 64 (toInteger i)
codegenTriv (VarTriv v) = do
  let nm = fromVar v
  getvar nm
codegenTriv _ = __


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
testprog5 = Prog {fundefs = [], mainExp = Just (PrintExp (LetPrimCallT {binds = [(Var "fltIf0",IntTy)], prim = EqP, rands = [IntTriv 2,IntTriv 2], bod = Switch (VarTriv (Var "fltIf0")) (IntAlts [(0,LetPrimCallT {binds = [], prim = PrintInt, rands = [IntTriv 101], bod = LetPrimCallT {binds = [], prim = PrintString "\n", rands = [], bod = RetValsT []}})]) (Just (LetPrimCallT {binds = [], prim = PrintInt, rands = [IntTriv 99], bod = LetPrimCallT {binds = [], prim = PrintString "\n", rands = [], bod = RetValsT []}}))}))}
test5 = codegenProg False testprog5
testprog6 = Prog {fundefs = [], mainExp = Just (PrintExp (LetPrimCallT {binds = [(Var "fltPrm0",IntTy)], prim = MulP, rands = [IntTriv 3,IntTriv 4], bod = LetPrimCallT {binds = [(Var "fltPrm1",IntTy)], prim = SubP, rands = [IntTriv 8,IntTriv 9], bod = LetPrimCallT {binds = [(Var "flt2",IntTy)], prim = AddP, rands = [VarTriv (Var "fltPrm0"),VarTriv (Var "fltPrm1")], bod = LetPrimCallT {binds = [], prim = PrintInt, rands = [VarTriv (Var "flt2")], bod = LetPrimCallT {binds = [], prim = PrintString "\n", rands = [], bod = RetValsT []}}}}}))}
test6 = codegenProg False testprog6
