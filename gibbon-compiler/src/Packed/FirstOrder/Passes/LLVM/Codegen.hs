{-# LANGUAGE OverloadedStrings #-}
module Packed.FirstOrder.Passes.LLVM.Codegen where

-- | standard library
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map

-- | gibbon internals
import Packed.FirstOrder.L3_Target
import Packed.FirstOrder.Common (fromVar)
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


codegenProg' :: Prog -> CodeGen ()
codegenProg' prg@(Prog fns body) = do
  fns' <- mapM codegenFun fns
  mapM_ declare fns'
  _ <- addStructs prg
  let mainBody = genBlocks $ do
        -- TODO(cskksc): why is this required ?
        _ <- addStructs prg
        mapM_ declare fns'

        entry <- newBlock "entry"
        setBlock entry
        _ <- case body of
          Just (PrintExp t) -> codegenTail t IntTy
          _ -> (retval_ . constop_ . int_) 0
        createBlocks

  declare puts
  declare printInt
  declare malloc
  declare globalSizeParam
  declare (mainFn mainBody)


-- | Generate LLVM instructions for function definitions
--
codegenFun :: FunDecl -> CodeGen G.Global
codegenFun (FunDecl fnName args retTy tail) = do
  let fnName' = fromVar fnName
  fnBody <- do
    entry <- newBlock $ "fn." ++ fnName' ++ ".entry"
    _     <- setBlock entry

    -- add all args to localVars
    forM_ args $ \(v,ty) ->
      modify $ \s ->
        let nm  = fromVar v
            ty' = typeOf ty
        in s { localVars = Map.insert nm (localRef ty' (AST.Name nm)) (localVars s)}
    _ <- codegenTail tail retTy
    createBlocks

  -- return the generated function
  return G.functionDefaults
         { G.name        = AST.Name fnName'
         , G.parameters  = ([G.Parameter (typeOf ty) (AST.Name $ fromVar v) []
                            | (v, ty) <- args],
                            False)
         , G.returnType  = typeOf retTy
         , G.basicBlocks = fnBody
         }


-- | Generate LLVM instructions for Tail
--
codegenTail :: Tail -> Ty -> CodeGen BlockState
codegenTail (RetValsT []) _ = return_
codegenTail (RetValsT [t]) _ = do
  t' <- codegenTriv t
  retval_ t'
codegenTail (RetValsT ts) (ProdTy tys) =
  let structTy = typeOf tys
  in do
    struct <- mapM codegenTriv ts >>= populateStruct structTy Nothing
    struct' <- convert (toPtrTy $ typeOf $ ProdTy tys) Nothing struct
    load (typeOf $ ProdTy tys) Nothing struct' >>= retval_

codegenTail (LetPrimCallT bnds prm rnds body) ty = do
  rnds' <- mapM codegenTriv rnds
  _     <- case prm of
             PrintInt -> do
               _ <- call printInt Nothing rnds'
               return_
             PrintString s -> do
               _ <- printString s
               return_
             AddP -> addp bnds rnds'
             SubP -> subp bnds rnds'
             MulP -> mulp bnds rnds'
             EqP  -> eqp bnds rnds'
             SizeParam -> sizeParam bnds
             ReadTag -> readTag bnds rnds'
             ReadInt -> readInt bnds rnds'
             _ -> error $ "Prim: Not implemented yet: " ++ show prm
  codegenTail body ty

codegenTail (IfT test consq els') ty = do
  _ <- ifThenElse (toIfPred test) (codegenTail consq ty) (codegenTail els' ty)
  return_

codegenTail (Switch trv alts def) ty =
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
      mapM_ (\(_,t) -> do
                    x <- newBlock "switch.case"
                    setBlock x
                    codegenTail t ty) alts''

      -- generate the default block
      setBlock switchDefault
      codegenTail def' ty

codegenTail (LetCallT bnds rator rnds body) ty = do
  rnds' <- mapM codegenTriv rnds
  gt <- gets globalFns
  let nm = fromVar rator
  fn <- case Map.lookup nm gt of
          Just x -> return x
          Nothing -> error $ "Function" ++ nm ++ " doesn't exist " ++ show gt
  _ <- callp fn bnds rnds'
  codegenTail body ty

codegenTail (LetAllocT lhs vals body) ty =
  let structTy = typeOf $ map fst vals
      lhsV      = fromVar lhs
  in do
    struct <- mapM (codegenTriv . snd) vals >>= populateStruct structTy Nothing
    _ <- convert (typeOf PtrTy) (Just lhsV) struct
    codegenTail body ty

codegenTail t _ = error $ "Tail: Not implemented yet: " ++ show t


-- | Generate LLVM instructions for Triv
--
codegenTriv :: Triv -> CodeGen AST.Operand
codegenTriv (IntTriv i) = (return . constop_ . int_ . toInteger) i
codegenTriv (VarTriv v) = do
  let nm = fromVar v
  getvar nm
codegenTriv t = error $ "Triv: Not implemented yet: " ++ show t
