{-# LANGUAGE RecordWildCards #-}

-- |
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--

module Packed.FirstOrder.Passes.LLVM.Instruction (
    declare, getvar, getLastLocal, addDefinition
  , instr, namedInstr, globalOp, localRef, toPtrType
  , allocate, store, load, namedLoad, getElemPtr, call, call2
  , add, namedAdd, mul, namedMul, sub, namedSub
  , eq, namedEq, neq, namedNeq, ifThenElse
) where

-- | standard library
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

-- | llvm-general
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Type as T
import qualified LLVM.General.AST.Instruction as I
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.AddrSpace as AS
import qualified LLVM.General.AST.IntegerPredicate as IP

import Packed.FirstOrder.Passes.LLVM.Monad
import Packed.FirstOrder.Passes.LLVM.Terminator


-- | Add a definition to the module's global definitions
--
addDefinition :: String -> AST.Definition -> CodeGen ()
addDefinition nm d =
  modify $ \s -> s { structs = Map.insert nm d (structs s)}


-- | Add a global declaration to the symbol table
--
declare :: G.Global -> CodeGen ()
declare g =
  let name = case G.name g of
               AST.Name n   -> n
               AST.UnName n -> show n
  in
    modify $ \s -> s { globalTable = Map.insert name g (globalTable s)}


-- | Generate a fresh (un)name.
--
freshName :: CodeGen AST.Name
freshName = state $ \s@CodeGenState{..} -> ( AST.UnName next,
                                             s { next = next + 1 } )


-- | Return local var reference
--
getvar :: String -> CodeGen AST.Operand
getvar nm = do
  vars <- gets localVars
  case Map.lookup nm vars of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show nm


getLastLocal :: CodeGen AST.Name
getLastLocal = do
  a <- gets next
  return $ AST.UnName (a - 1)


-- Causes GHC panic!
-- -- | Return the most recent unname
-- currentName :: CodeGen AST.Name
-- currentName = return $ AST.UnName . fromIntegral $ do gets next


-- | Add an instruction to the state of the currently active block so that it is
-- computed, and return the operand (LocalReference) that can be used to later
-- refer to it.
--

instr :: T.Type -> AST.Instruction -> CodeGen AST.Operand
instr ty ins = do
  name <- freshName
  instr_ $ name AST.:= ins
  return $ AST.LocalReference ty name


namedInstr :: T.Type -> String -> I.Instruction -> CodeGen AST.Operand
namedInstr ty nm ins = do
  instr_ $ (AST.Name nm) AST.:= ins
  let ref = AST.LocalReference ty (AST.Name nm)
  modify $ \s -> s { localVars = Map.insert nm ref (localVars s)}
  return ref


-- | Add raw assembly instructions to the execution stream
--
instr_ :: AST.Named AST.Instruction -> CodeGen ()
instr_ ins =
  modify $ \s ->
    case Seq.viewr (blockChain s) of
      Seq.EmptyR  -> error $ "instr_ empty block chain "  ++ show s
      bs Seq.:> b -> s { blockChain = bs Seq.|> b { instructions = instructions b Seq.|> ins } }


-- | Return a global reference pointing to an operand
--
globalOp :: T.Type -> AST.Name -> AST.Operand
globalOp ty nm = AST.ConstantOperand $ C.GlobalReference ty nm


-- | Return a local reference
--
localRef :: T.Type -> AST.Name -> AST.Operand
localRef = AST.LocalReference


-- | Add a function call to the execution stream
--
call :: G.Global -> [AST.Operand] -> CodeGen AST.Operand
call fn args = instr retTy $ I.Call Nothing CC.C [] (Right fn') args' [] []
  -- TODO(cskksc): declare fn -- ^ this doesn't work
  where fn'   = globalOp retTy nm
        args' = toArgs args
        nm    = G.name fn
        retTy = G.returnType fn

call2 :: T.Type -> AST.Name -> [AST.Operand] -> CodeGen AST.Operand
call2 ty nm args = instr ty $ I.Call Nothing CC.C [] (Right fn) args' [] []
  where fn = globalOp ty nm
        args' = toArgs args

-- | Convert operands to the expected args format
--
toArgs :: [AST.Operand] -> [(AST.Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))


-- | Allocate memory for the type
--
allocate :: T.Type -> CodeGen AST.Operand
allocate ty = instr ty $ I.Alloca ty Nothing 0 []


-- | Store operand as a new local unname
--
store :: AST.Operand -> AST.Operand -> CodeGen AST.Operand
store addr val = instr T.VoidType $ I.Store False addr val Nothing 0 []
-- TODO(cskksc): dont know if T.VoidType is correct


-- | Read from memory
--

load' :: Maybe String -> T.Type -> AST.Operand -> CodeGen AST.Operand
load' Nothing ty addr = instr ty $ I.Load False addr Nothing 8 []
load' (Just nm) ty addr = namedInstr ty nm $ I.Load False addr Nothing 8 []

load :: T.Type -> AST.Operand -> CodeGen AST.Operand
load = load' Nothing

namedLoad :: String -> T.Type -> AST.Operand -> CodeGen AST.Operand
namedLoad nm = load' (Just nm)


-- | Get the address of a subelement of an aggregate data structure
--
getElemPtr :: Bool -> AST.Operand -> [AST.Operand] -> CodeGen AST.Operand
getElemPtr inbounds addr idxs = instr T.VoidType $ I.GetElementPtr inbounds addr idxs []
-- TODO(cskksc): dont know if T.VoidType is correct


-- | Arithmetic operations
--

binop :: Maybe String
      -> (AST.Operand -> AST.Operand -> I.InstructionMetadata -> I.Instruction)
      -> AST.Operand -> AST.Operand -> CodeGen AST.Operand
binop Nothing ins x y = instr T.i64 $ ins x y []
binop (Just nm) ins x y= namedInstr T.i64 nm $ ins x y []

add :: AST.Operand -> AST.Operand -> CodeGen AST.Operand
add = binop Nothing (I.Add False False)

namedAdd :: String -> AST.Operand -> AST.Operand -> CodeGen AST.Operand
namedAdd nm = binop (Just nm) (I.Add False False)

mul :: AST.Operand -> AST.Operand -> CodeGen AST.Operand
mul = binop Nothing (I.Mul False False)

namedMul :: String -> AST.Operand -> AST.Operand -> CodeGen AST.Operand
namedMul nm = binop (Just nm) (I.Mul False False)

sub :: AST.Operand -> AST.Operand -> CodeGen AST.Operand
sub = binop Nothing (I.Sub False False)

namedSub :: String -> AST.Operand -> AST.Operand -> CodeGen AST.Operand
namedSub nm = binop (Just nm) (I.Sub False False)

-- | Compare two operands
--
icmp :: IP.IntegerPredicate -> Maybe String -> AST.Operand -> AST.Operand -> CodeGen AST.Operand
icmp p Nothing x y = instr T.i64 $ I.ICmp p x y []
icmp p (Just nm) x y = namedInstr T.i64 nm $ I.ICmp p x y []


-- | Equality operators
--

eq :: AST.Operand -> AST.Operand -> CodeGen AST.Operand
eq = icmp IP.EQ Nothing

namedEq :: String -> AST.Operand -> AST.Operand -> CodeGen AST.Operand
namedEq nm = icmp IP.EQ (Just nm)

neq :: AST.Operand -> AST.Operand -> CodeGen AST.Operand
neq = icmp IP.NE Nothing

namedNeq :: String -> AST.Operand -> AST.Operand -> CodeGen AST.Operand
namedNeq nm = icmp IP.NE (Just nm)


-- | Convert the type to a pointer type
--
toPtrType :: T.Type -> T.Type
toPtrType ty = T.PointerType ty (AS.AddrSpace 0)


-- | Add a phi node to the top of the current block
--
phi :: T.Type -> [(AST.Operand, AST.Name)] -> CodeGen AST.Operand
phi ty incoming = instr ty $ I.Phi ty incoming []


-- | Standard if-then-else expression
--
ifThenElse :: CodeGen AST.Operand -> CodeGen BlockState -> CodeGen BlockState -> CodeGen AST.Operand
ifThenElse test yes no = do
  ifThen  <- newBlock "if.then"
  ifElse  <- newBlock "if.else"
  ifExit  <- newBlock "if.exit"
  ifEntry <- newBlock "if.entry"

  _  <- br ifEntry
  setBlock ifEntry
  p  <- test
  _  <- cbr p ifThen ifElse

  setBlock ifThen
  _  <- yes
  tb <- br ifExit
  -- Since yes/no are BlockState's, we extract the last unname, assuming it's the
  -- last instruction of the then block, and use that as an Operand
  -- TODO(cskksc): This won't work if the block ends with a named instruction
  --               Also, somehow infer the T.i64 here
  last'  <- getLastLocal
  let tv = AST.LocalReference T.i64 last'

  setBlock ifElse
  _     <- no
  fb    <- br ifExit
  last' <- getLastLocal
  let fv = AST.LocalReference T.i64 last'

  setBlock ifExit
  phi T.i64 [(tv, blockLabel tb), (fv, blockLabel fb)]
