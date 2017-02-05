{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--

module Packed.FirstOrder.Passes.LLVM.Monad where

-- | standard library
import Data.Map (Map)
import Data.Sequence (Seq)
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F

-- | llvm-general
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Global as G


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

initialCodeGenState :: CodeGenState
initialCodeGenState = CodeGenState
                     { blockChain  = Seq.empty
                     , globalTable = Map.empty
                     , localVars   = Map.empty
                     , next        = 0
                     }


-- | Run the CodeGen to produce an LLVM module
genModule :: CodeGen a -> AST.Module
genModule x =
  let (_ , st) = runState (runCodegen x) initialCodeGenState
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
genBlocks m = evalState (runCodegen m) initialCodeGenState


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
    makeBlock blk@BlockState{..} =
      case terminator of
        Just x -> AST.BasicBlock blockLabel (F.toList instructions) (AST.Do x)
        Nothing -> error $ "No terminator for block " ++ show blk
