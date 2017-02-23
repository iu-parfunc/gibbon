{-# LANGUAGE OverloadedStrings #-}
module Packed.FirstOrder.Passes.LLVM.Codegen where

-- | standard library
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map

-- | gibbon internals
import Packed.FirstOrder.L3_Target
import Packed.FirstOrder.Common (fromVar,toVar)
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
codegenProg _ prg@(Prog fns body) = (toLLVM . genModule) $ do
  -- declare helpers declared in lib.c
  declare printInt
  declare puts
  declare globalSizeParam

  -- generate structs and fns
  _ <- addStructs prg
  mapM_ codegenFunSig fns'
  mapM_ codegenFun fns'
  where expr = case body of
                     Just (PrintExp t) -> t
                     _ -> RetValsT []
        fns' =  fns ++ [FunDecl (toVar "__main_expr") [] (ProdTy []) expr]


-- | Add fn signatures to globalFns
--
-- This is intended to be used by the call fn (LetCallT).
-- Storing only the signature is important to allow recursive fn definitions
-- and forward references
--
codegenFunSig :: FunDecl -> CodeGen ()
codegenFunSig (FunDecl fnName args retTy _) =
  let fnName' = fromVar fnName
      fnsig = G.functionDefaults
                {  G.name        = AST.Name fnName'
                ,  G.parameters  = ([G.Parameter (typeOf ty) (AST.Name $ fromVar v) []
                                    | (v, ty) <- args],
                                    False)
                , G.returnType  = typeOf retTy
                }
  in declare fnsig


-- | Generate LLVM instructions for function definitions
--
codegenFun :: FunDecl -> CodeGen ()
codegenFun (FunDecl fnName args retTy tail) = do
  let fnName' = fromVar fnName
  fns <- gets globalFns
  fnsig <- getfn fnName'
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

  -- Now that we have the fn body, override the fn definition in globalFns
  declare $ fnsig {G.basicBlocks = fnBody}


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
  fn <- getfn nm
  _ <- callp fn bnds rnds'
  codegenTail body ty

codegenTail (LetAllocT lhs vals body) ty =
  let structTy = typeOf $ map fst vals
      lhsV     = fromVar lhs
  in do
    struct <- mapM (codegenTriv . snd) vals >>= populateStruct structTy Nothing
    _ <- convert (typeOf PtrTy) (Just lhsV) struct
    codegenTail body ty

codegenTail (LetUnpackT bnds ptr body) ty = do
  struct <- getvar (fromVar ptr)
  _ <- unpackStruct Nothing struct bnds
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
