{-# LANGUAGE OverloadedStrings #-}
module Packed.FirstOrder.Passes.LLVM.Codegen where

-- | standard library
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map

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
import qualified LLVM.General.AST.Type as T
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


codegenProg' :: Prog -> CodeGen ()
codegenProg' prg@(Prog fns body) = do
  fns' <- mapM codegenFun fns
  _ <- addStructs prg
  let mainBody = genBlocks $ do
        -- TODO(cskksc): why does this work here ?
        mapM_ declare fns'
        entry <- newBlock "entry"
        setBlock entry
        _ <- case body of
          Just (PrintExp t) -> codegenTail t
          _ -> (retval_ . constop_ . int_) 0
        createBlocks

  declare puts
  declare printInt
  declare globalSizeParam
  declare (mainFn mainBody)


-- | Generate LLVM instructions for function definitions
--
codegenFun :: FunDecl -> CodeGen G.Global
codegenFun (FunDecl fnName args retTy tail) = do
  let fnName' = fromVar fnName
  fnBody <- do
    entry <- newBlock $ "fn." ++ fnName' ++ "entry"
    _     <- setBlock entry

    -- add all args to localVars
    forM_ args $ \(v,ty) -> do
      modify $ \s ->
        let nm  = fromVar v
            ty' = toLLVMTy ty
        in s { localVars = Map.insert nm (localRef ty' (AST.Name nm)) (localVars s)}
    _ <- codegenTail tail
    createBlocks

  -- add the function to globalTable
  let fn = G.functionDefaults
           { G.name        = AST.Name fnName'
           , G.parameters  = ([G.Parameter (toLLVMTy ty) (AST.Name $ fromVar v) []
                              | (v, ty) <- args],
                              False)
           , G.returnType  = (toLLVMTy retTy)
           , G.basicBlocks = fnBody
           }

  -- TODO(cskksc): doesn't work without this declaration here
  declare fn
  return fn


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
  _ <- ifThenElse (toIfPred test) (codegenTail consq) (codegenTail els')
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
      dests = map (\((casei,_), i) -> (int_ $ toInteger casei, AST.Name $ "switch" ++ show i ++ ".case")) $ zip alts'' [1..]

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

codegenTail (LetCallT bnds rator rnds body) = do
  rnds' <- mapM codegenTriv rnds
  gt <- gets globalTable
  fn <- case Map.lookup (fromVar rator) gt of
          Just x -> return x
          Nothing -> error $ "Function doesn't exist " ++ show gt
  _ <- gcall bnds fn rnds'
  codegenTail body

codegenTail _ = __

-- | Generate LLVM instructions for Triv
--
codegenTriv :: Triv -> CodeGen AST.Operand
codegenTriv (IntTriv i) = (return . constop_ . int_ . toInteger) i
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
testprog7 = Prog {fundefs = [FunDecl {funName = Var "add2", funArgs = [(Var "a", IntTy), (Var "b", IntTy)], funRetTy = IntTy, funBody = LetPrimCallT {binds = [(Var "res", IntTy)], prim = AddP, rands = [VarTriv (Var "a"), VarTriv (Var "b")], bod = LetPrimCallT {binds = [], prim = PrintInt, rands = [VarTriv (Var "res")], bod = RetValsT [VarTriv (Var "res")]}}}],
                   mainExp = Just (PrintExp (LetCallT {binds = [], rator = Var "add2", rands = [IntTriv 2,IntTriv 2], bod = LetPrimCallT {binds = [], prim = PrintString "\n", rands = [], bod = RetValsT []}}))}
test7 = codegenProg False testprog7
testprog8 = Prog {fundefs = [FunDecl {funName = Var "add2", funArgs = [(Var "a", IntTy), (Var "b", IntTy)], funRetTy = IntTy, funBody = LetPrimCallT {binds = [(Var "res", IntTy)], prim = AddP, rands = [VarTriv (Var "a"), VarTriv (Var "b")], bod = LetPrimCallT {binds = [], prim = PrintInt, rands = [VarTriv (Var "res")], bod = RetValsT [VarTriv (Var "res")]}}}],
                   mainExp = Just (PrintExp (RetValsT []))}
test8 = codegenProg False testprog8
testprog9 = Prog {fundefs = [FunDecl {funName = Var "add2", funArgs = [(Var "pvrtmp3",IntTy),(Var "pvrtmp4",IntTy)], funRetTy = IntTy, funBody = LetPrimCallT {binds = [(Var "flt5",IntTy)], prim = AddP, rands = [VarTriv (Var "pvrtmp3"),VarTriv (Var "pvrtmp4")], bod = RetValsT [VarTriv (Var "flt5")]}}], mainExp = Just (PrintExp (LetCallT {binds = [(Var "tctmp2",IntTy)], rator = Var "add2", rands = [IntTriv 40,IntTriv 2], bod = LetPrimCallT {binds = [], prim = PrintInt, rands = [VarTriv (Var "tctmp2")], bod = LetPrimCallT {binds = [], prim = PrintString "\n", rands = [], bod = RetValsT []}}}))}
test9 = codegenProg False testprog9
