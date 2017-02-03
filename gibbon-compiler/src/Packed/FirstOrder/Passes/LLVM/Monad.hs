{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--


module Packed.FirstOrder.Passes.LLVM.Monad (

  -- monad and state
  CodeGen, CodeGenState, BlockState,

  -- basic blocks
  genModule, genBlocks, createBlocks, setBlock, newBlock, beginBlock,

  -- instructions
  declare, retval_, return_, call,
  getvar, allocate, load, namedLoad, store, getElemPtr,

  add, namedAdd, ifThenElse,
  instr, namedInstr, toPtrType, localRef, globalOp,

  next

) where

-- | standard library
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Maybe (fromJust)
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F

-- | llvm-general
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Type as T
import qualified LLVM.General.AST.Instruction as I
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.AddrSpace as AS

-- | The code generation state for our AST.
--
-- We use two records: one to hold all the code generation state as it walks the
-- AST, and one for each of the basic blocks that are generated during the walk.
--
data CodeGenState = CodeGenState
  { blockChain  :: Seq BlockState         -- ^ blocks for this function
  , globalTable :: Map String G.Global    -- ^ external functions symbol table
  , localVars   :: Map String AST.Operand -- ^ local vars
  , next        :: Word                   -- ^ names supply
  } deriving Show


data BlockState = BlockState
  { blockLabel   :: AST.Name                         -- ^ block name
  , instructions :: Seq (AST.Named AST.Instruction)  -- ^ stack of instructions
  , terminator   :: Maybe AST.Terminator             -- ^ block terminator
  } deriving Show


newtype CodeGen a = CodeGen { runCodegen :: State CodeGenState a }
  deriving (Functor, Applicative, Monad, MonadState CodeGenState )


-- | Run the CodeGen to produce an LLVM module
genModule :: CodeGen a -> AST.Module
genModule x =
  let initialState = CodeGenState
                     { blockChain  = initBlockChain
                     , globalTable = Map.empty
                     , localVars   = Map.empty
                     , next        = 0
                     }
      (_ , st) = runState (runCodegen x) initialState
      definitions = map AST.GlobalDefinition (Map.elems $ globalTable st)
      name  = "first-module"
  in AST.Module
    {
      AST.moduleName = name
    , AST.moduleSourceFileName = []
    , AST.moduleDefinitions = definitions
    , AST.moduleDataLayout = Nothing
    , AST.moduleTargetTriple = Just "x86_64-unknown-linux-gnu"
    }


genBlocks :: CodeGen [AST.BasicBlock] -> [AST.BasicBlock]
genBlocks m =
  let initialState = CodeGenState
                     { blockChain  = Seq.empty
                     , globalTable = Map.empty
                     , localVars   = Map.empty
                     , next        = 0
                     }
  in
    evalState (runCodegen m) initialState

-- Basic Blocks
-- ============

-- | initial block chain
--
initBlockChain :: Seq BlockState
initBlockChain = Seq.singleton $ BlockState (AST.Name"entry") Seq.empty Nothing


-- | Create a new basic block, but don't yet add it to the block chain. You need
-- to call 'setBlock' to append it to the chain, so that subsequent instructions
-- are added to this block.
--
-- Note: [Basic blocks]
--
-- The names of basic blocks are generated based on the base name provided to
-- the 'newBlock' function, as well as the current state (length) of the block
-- stream. By not immediately adding new blocks to the stream, we have the
-- advantage that:
--
--   1. Instructions are generated "in order", and are always appended to the
--      stream. There is no need to search the stream for a block of the right
--      name.
--
--   2. Blocks are named in groups, which helps readability. For example, the
--      blocks for the then and else branches of a conditional, created at the
--      same time, will be named similarly: 'if4.then' and 'if4.else', etc.
--
-- However, this leads to a slight awkwardness when walking the AST. Since a new
-- naming group scheme is only applied *after* a call to 'setBlock',
-- encountering (say) nested conditionals in the walk will generate logically
-- distinct blocks that happen to have the same name. This means that
-- instructions might be added to the wrong blocks, or the first set of blocks
-- will be emitted empty and/or without a terminator.
--

newBlock :: String -> CodeGen BlockState
newBlock nm =
  state $ \s ->
    let idx     = Seq.length (blockChain s)
        label   = let (h,t) = break (== '.') nm in (h ++ shows idx t)
        next    = BlockState (AST.Name label) Seq.empty Nothing
        -- err     = error "Block has no terminator"
    in
    ( next, s )


-- | Add this block to the block stream. Any instructions pushed onto the stream
-- by 'instr' and friends will now apply to this block.
--
setBlock :: BlockState -> CodeGen ()
setBlock next =
  modify $ \s -> s { blockChain = blockChain s Seq.|> next }


-- | Generate a new block and branch unconditionally to it.
--
beginBlock :: String -> CodeGen BlockState
beginBlock nm = do
  next <- newBlock nm
  _    <- br next
  setBlock next
  return next


-- | Extract the block state and construct the basic blocks that form a function
-- body. The block stream is re-initialised, but module-level state such as the
-- global symbol table is left intact.
--
createBlocks :: CodeGen [AST.BasicBlock]
createBlocks
  = state
  $ \s -> let s'     = s { blockChain = initBlockChain, next = 0 }
              blocks = makeBlock `fmap` blockChain s
              -- m      = Seq.length (blockChain s)
              -- n      = F.foldl' (\i b -> i + Seq.length (instructions b)) 0 (blockChain s)
          in
          ( F.toList blocks , s' )
  where
    makeBlock BlockState{..} =
      AST.BasicBlock blockLabel (F.toList instructions) (AST.Do $ fromJust terminator)


-- Instructions
-- ============

-- | Add a termination condition to the current instruction stream. Also return
-- the block that was just terminated.
--
terminate :: AST.Terminator -> CodeGen BlockState
terminate term =
  state $ \s ->
    case Seq.viewr (blockChain s) of
      Seq.EmptyR  -> error "terminate empty block chain"
      bs Seq.:> b -> ( b, s { blockChain = bs Seq.|> b { terminator = Just term } } )


-- | Return a value from a basic block
--
retval_ :: AST.Operand -> CodeGen BlockState
retval_ x = terminate $ AST.Ret (Just x) []

-- | Return void from a basic block
--
return_ :: CodeGen BlockState
return_ = terminate $ AST.Ret Nothing []


-- | Add a global declaration to the symbol table
--
declare :: G.Global -> CodeGen()
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
  ref <- return $ AST.LocalReference ty (AST.Name nm)
  modify $ \s -> s { localVars = Map.insert nm ref (localVars s)}
  return $ ref


-- | Add raw assembly instructions to the execution stream
--
instr_ :: AST.Named AST.Instruction -> CodeGen ()
instr_ ins =
  modify $ \s ->
    case Seq.viewr (blockChain s) of
      Seq.EmptyR  -> error "instr_ empty block chain"
      bs Seq.:> b -> s { blockChain = bs Seq.|> b { instructions = instructions b Seq.|> ins } }

-- | Return local var reference
--
getvar :: String -> CodeGen AST.Operand
getvar nm = do
  vars <- gets localVars
  case Map.lookup nm vars of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show nm


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
call fn args = do
  instr retTy $ I.Call Nothing CC.C [] (Right fn') args' [] []
  where fn'   = globalOp retTy nm
        args' = toArgs args
        nm    = G.name fn
        retTy = G.returnType fn


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


-- | Read from memory into a local unname
--

load :: T.Type -> AST.Operand -> CodeGen AST.Operand
load ty addr = instr ty $ loadInstr addr


-- | Read from memory into a local var named nm
--
namedLoad :: String -> T.Type -> AST.Operand -> CodeGen AST.Operand
namedLoad nm ty addr = namedInstr ty nm $ loadInstr addr


-- | LLVM load instruction
loadInstr :: AST.Operand -> I.Instruction
loadInstr addr = I.Load False addr Nothing 8 []


-- | Get the address of a subelement of an aggregate data structure
--
getElemPtr :: Bool -> AST.Operand -> [AST.Operand] -> CodeGen AST.Operand
getElemPtr inbounds addr idxs = instr T.VoidType $ I.GetElementPtr inbounds addr idxs []
-- TODO(cskksc): dont know if T.VoidType is correct


-- | Add two operands and store result in local unname
--
add :: [AST.Operand] -> CodeGen AST.Operand
add [x,y] = instr T.i32 $ addInstr x y


-- | Add two operands and store result in local nm
--
namedAdd :: String -> T.Type -> [AST.Operand] -> CodeGen AST.Operand
namedAdd nm ty [x,y] = namedInstr ty nm $ addInstr x y


-- | LLVM add instruction
--
addInstr :: AST.Operand -> AST.Operand -> I.Instruction
addInstr x y = I.Add False False x y []


-- | Convert the type to a pointer type
--
toPtrType :: T.Type -> T.Type
toPtrType ty = T.PointerType ty (AS.AddrSpace 0)


-- | Add a phi node to the top of the current block
--
phi :: T.Type -> [(AST.Operand, AST.Name)] -> CodeGen AST.Operand
phi ty incoming = instr ty $ I.Phi ty incoming []


-- | Unconditional branch. Return the name of the block that was branched from.
--
br :: BlockState -> CodeGen BlockState
br target = terminate $ AST.Br (blockLabel target) []


-- | Conditional branch. Return the name of the block that was branched from.
--
cbr :: AST.Operand -> BlockState -> BlockState -> CodeGen BlockState
cbr cond t f = terminate $ I.CondBr cond (blockLabel t) (blockLabel f) []


-- | Standard if-then-else expression
--
ifThenElse :: CodeGen AST.Operand -> CodeGen BlockState -> CodeGen BlockState -> CodeGen AST.Operand
ifThenElse test yes no = do
  ifThen <- newBlock "if.then"
  ifElse <- newBlock "if.else"
  ifExit <- newBlock "if.exit"

  _  <- beginBlock "if.entry"
  p  <- test
  _  <- cbr p ifThen ifElse

  setBlock ifThen
  _ <- yes
  tb <- br ifExit
  -- Since yes/no are BlockState's, we extract the last unname, assuming it's the
  -- last instruction of the then block, and use that as an Operand
  -- TODO(cskksc): This won't work if the block ends with a named instruction
  --               Also, somehow infer the T.i64 here
  tv <- do
    a <- gets next
    return $ AST.LocalReference T.i64 (AST.UnName (a - 1))

  setBlock ifElse
  _ <- no
  fb <- br ifExit
  fv <- do
    a <- gets next
    return $ AST.LocalReference T.i64 (AST.UnName (a - 1))

  setBlock ifExit
  phi T.i64 [(tv, (blockLabel tb)), (fv, (blockLabel fb))]
