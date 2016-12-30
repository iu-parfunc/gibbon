{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE DeriveAnyClass #-} -- Actually breaks Applicative SymM deriving!
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Utilities and common types.

module Packed.FirstOrder.Common
       (
         -- * Global constants
--         cPackedTagSize, cPointerTagSize -- FINISHME

         -- * Type and Data Constructors
         Constr
         -- * Variables and gensyms
       , Var, varAppend, SyM, gensym, genLetter, runSyM
       , cleanFunName

       , LocVar, Env2(..)

         -- * Runtime configuration
       , RunConfig(..), getRunConfig

         -- * Top-level function defs
       , FunDef(..), FunDefs
       , insertFD, fromListFD

         -- * Data definitions
       , DDef(..), DDefs, fromListDD, emptyDD, insertDD
       , lookupDDef, lookupDataCon, getConOrdering, getTyOfDataCon, getTagOfDataCon

         -- * Misc helpers
       , (#), fragileZip, sdoc, ndoc         

         -- * Debugging/logging:
       , dbgLvl, dbgPrint, dbgPrintLn, dbgTrace, dbgTraceIt, minChatLvl
       , Interp(..)

         -- * Establish conventions for the output of #lang gibbon:
       , truePrinted, falsePrinted
       ) where

import Data.Char
import Data.Word
import Control.Monad.State.Strict
import Control.DeepSeq (NFData)
import Data.List as L
import Data.Map as M
import GHC.Generics
import Text.PrettyPrint.GenericPretty
import GHC.Stack (errorWithStackTrace)
-- import Text.Printf
import System.IO
import System.Environment
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace

-- type CursorVar = Var
type Var    = String
type Constr = String

-- | Abstract location variables.
type LocVar = Var

varAppend :: Var -> Var -> Var
varAppend = (++)

--------------------------------------------------------------------------------

-- | Pure Gibbon programs, at any stage of compilation, should always
-- be evaluatable to a unique value.  The only side effects are timing
class Interp a where
  -- | Interpret while ignoring timing constructs, and dropping the
  -- corresponding output to stdout.
  interpNoLogs     :: RunConfig -> a -> String

  -- | Interpret and produce a "log" of output lines, as well as a
  -- final, printed result.
  interpWithStdout :: RunConfig -> a -> IO (String,[String])
                  
--------------------------------------------------------------------------------

-- | A common currency for a two part environment consisting of
-- function bindings and regular value bindings.
data Env2 a = Env2 { vEnv :: M.Map Var a
                   , fEnv :: M.Map Var (a, a) }

-- Primitive for now:
type DDefs a = Map Var (DDef a)

-- | In the extreme case we can strip packed datatypes of all type
-- parameters, or we can allow them to retain type params but require
-- that they always be fully instantiated to monomorphic types in the
-- context of our monomorphic programs.
data DDef a = DDef { tyName:: Var
                   -- , tyArgs:: [Var] -- ^ No polymorphism for now!
                   , dataCons :: [(Constr,[a])] }
  deriving (Read,Show,Eq,Ord, Functor, Generic)

instance NFData a => NFData (DDef a) where
  -- rnf DDef

instance Out a => Out (DDef a)
instance (Out k,Out v) => Out (Map k v) where
  doc         = doc . M.toList
  docPrec n v = docPrec n (M.toList v)

-- DDef utilities:

-- | Lookup a ddef in its entirety
lookupDDef :: Out a => DDefs a -> Var -> DDef a
lookupDDef mp v =
    case M.lookup v mp of
      Just x -> x
      Nothing -> error $ "lookupDDef failed on symbol: "++v++"\nDDefs: "++sdoc mp
                 

-- | Get the canonical ordering for data constructors, currently based
-- on ordering in the original source code.  Takes a TyCon as argument.
getConOrdering :: Out a => DDefs a -> Var -> [Constr]
getConOrdering dd tycon = L.map fst dataCons
  where DDef{dataCons} = lookupDDef dd tycon

-- | Lookup the name of the TyCon that goes with a given DataCon.
--   Must be unique!
getTyOfDataCon :: Out a => DDefs a -> Var -> Var
getTyOfDataCon dds con = fst $ lkp dds con

-- | Look up the numeric tag for a dataCon 
getTagOfDataCon :: Out a => DDefs a -> Var -> Word8
getTagOfDataCon dds dcon =
    -- dbgTrace 5 ("getTagOfDataCon -- "++sdoc(dds,dcon)) $
    fromIntegral ix
  where Just ix = L.elemIndex dcon $ getConOrdering dds tycon
        (tycon,_) = lkp dds dcon

-- | Lookup the arguments to a data contstructor.
lookupDataCon :: Out a => DDefs a -> Constr -> [a]
lookupDataCon dds con =
    -- dbgTrace 5 ("lookupDataCon -- "++sdoc(dds,con)) $
    snd $ snd $ lkp dds con

-- | Lookup a Datacon.  Return (TyCon, (DataCon, [flds]))
lkp :: Out a => DDefs a -> Constr -> (Var, (Constr, [a]))
lkp dds con =
   -- Here we try to lookup in ALL datatypes, assuming unique datacons:
  case [ (tycon,variant)
       | (tycon, DDef{dataCons}) <- M.toList dds
       , variant <- L.filter ((==con). fst) dataCons ] of
    [] -> error$ "lookupDataCon: could not find constructor "++show con
          ++", in datatypes:\n  "++show(doc dds)
    [hit] -> hit
    _ -> error$ "lookupDataCon: found multiple occurences of constructor "++show con
          ++", in datatypes:\n  "++show(doc dds)



insertDD :: DDef a -> DDefs a -> DDefs a
insertDD d = M.insertWith err (tyName d) d
  where
   err = error $ "insertDD: data definition with duplicate name: "++show (tyName d)

emptyDD :: DDefs a
emptyDD  = M.empty

fromListDD :: [DDef a] -> DDefs a
fromListDD = L.foldr insertDD M.empty


-- Fundefs
----------------------------------------

-- | A set of top-level recursive function definitions
type FunDefs ty ex = Map Var (FunDef ty ex)

data FunDef ty ex = FunDef { funName  :: Var
                               -- ^ Return type
                           , funArg   :: (Var,ty)
                           , funRetTy :: ty
                           , funBody  :: ex }
  deriving (Read,Show,Eq,Ord, Generic, Functor)

-- deriving
instance (NFData t, NFData e) => NFData (FunDef t e) where

instance (Out a, Out b) => Out (FunDef a b)

insertFD :: FunDef t e -> FunDefs t e -> FunDefs t e
insertFD d = M.insertWith err (funName d) d
  where
   err = error $ "insertFD: function definition with duplicate name: "++show (funName d)

fromListFD :: [FunDef t e] -> FunDefs t e
fromListFD = L.foldr insertFD M.empty


-- Gensym monad:
----------------------------------------

newtype SyM a = SyM (State Int a)
 deriving (Functor, Applicative, Monad, MonadState Int)
          
-- | Generate a unique symbol by attaching a numeric suffix.
gensym :: Var -> SyM Var
gensym v = state (\n -> (cleanFunName v `varAppend` show n, n + 1))

-- | Generate alphabetic variables 'a','b',...
genLetter :: SyM Var
genLetter = do
    n <- get
    modify (+1)
    return [chr (n + ord 'a')]

runSyM :: Int -> SyM a -> (a,Int)
runSyM n (SyM a) = runState a n

-- | Filter out non-C compatible characters.  This naively assumes it
-- will get no conflicts.  Which may be correct if function names were
-- gensym'd also....
cleanFunName :: Var -> Var
cleanFunName f =
    [ if isNumber c || isAlpha c
      then c
      else '_'
    | c <- f ]
                   
----------------------------------------


(#) :: (Ord a, Out a, Out b, Show a)
    => Map a b -> a -> b
m # k = case M.lookup k m of
          Just x  -> x
          Nothing -> errorWithStackTrace $ "Map lookup failed on key: "++show k
                     ++ " in map:\n "++ show (doc m)

fragileZip :: (Show a, Show b) => [a] -> [b] -> [(a, b)]
fragileZip [] [] = []
fragileZip (a:as) (b:bs) = (a,b) : fragileZip as bs
fragileZip as [] = error$ "fragileZip: right ran out, while left still has: "++show as
fragileZip [] bs = error$ "fragileZip: left ran out, while right still has: "++show bs

-- | Handy combination of show and doc                   
sdoc :: Out a => a -> String
sdoc = show . doc

-- | Like sdoc but inserts newline if it is longish.
ndoc :: Out a => a -> String
ndoc x = let s = sdoc x in
         if L.length s > 40
         then "\n  " ++ s
         else s


----------------------------------------------------------------------------------------------------
-- Global parameters              
----------------------------------------------------------------------------------------------------

-- | Runtime configuration for executing interpreters.
data RunConfig =
    RunConfig { rcSize  :: Int
              , rcIters :: Word64
              , rcDbg   :: Int
              , rcCursors :: Bool -- ^ Do we support cursors in L1.Exp at this point in the compiler.
              }
  
-- | We currently use the hacky approach of using env vars OR command
-- line args to set the two universal benchmark params: SIZE and ITERS.
--
-- This takes extra, optional command line args [Size, Iters] provided
-- after the file to process on the command line.  If these are not
-- present it
getRunConfig :: [String] -> IO RunConfig
getRunConfig ls =
 case ls of   
   [] -> case L.lookup "SIZE" theEnv of
           Nothing -> getRunConfig ["1"]
           Just n  -> getRunConfig [n]
   [sz] -> case L.lookup "ITERS" theEnv  of
             Nothing -> getRunConfig [sz,"1"]
             Just i  -> getRunConfig [sz,i]
   [sz,iters] ->
     return $ RunConfig { rcSize=read sz, rcIters=read iters, rcDbg= dbgLvl, rcCursors= False }
   _ -> error $ "getRunConfig: too many command line args, expected <size> <iters> at most: "++show ls

            
----------------------------------------------------------------------------------------------------
-- DEBUGGING
----------------------------------------------------------------------------------------------------

theEnv :: [(String, String)]
theEnv = unsafePerformIO getEnvironment

-- | Debugging flag shared by all modules.
--   This is activated by setting the environment variable DEBUG=1..5
dbgLvl :: Int
dbgLvl = case L.lookup "DEBUG" theEnv of
       Nothing  -> defaultDbg
       Just ""  -> defaultDbg
       Just "0" -> defaultDbg
       Just s   ->
         case reads s of
           ((n,_):_) ->
               if n >= minChatLvl
               then trace (" ! Responding to env Var: DEBUG="++s) n
               else n
           [] -> error$"Attempt to parse DEBUG env var as Int failed: "++show s

-- | We should not create chatter below this level.  DEBUG=1 is used
-- for assertions only, not chatter.
minChatLvl :: Int
minChatLvl = 2

defaultDbg :: Int
defaultDbg = 0

-- | Print if the debug level is at or above a threshold.
dbgPrint :: Int -> String -> IO ()
dbgPrint lvl str = if dbgLvl < lvl then return () else do
    hPutStrLn stderr str
    -- hPrintf stderr str
    -- hFlush stderr
    -- printf str
    -- hFlush stdout

-- | Conditional version of Debug.Trace.trace
dbgTrace :: Int -> String -> a -> a
dbgTrace lvl msg val =
    if   dbgLvl < lvl
    then val
    else trace msg val

dbgTraceIt :: Show a => Int -> String -> a -> a
dbgTraceIt lvl msg x = dbgTrace lvl (msg++": "++show x) x

dbgPrintLn :: Int -> String -> IO ()
dbgPrintLn lvl str = dbgPrint lvl (str++"\n")

--------------------------------------------------------------------------------

-- | For now this is designed to match the Racket output of "#lang
-- gibbon" which itself should change once we implement a custom
-- printer.
truePrinted :: String
truePrinted = "#t"
-- truePrinted = "true"

falsePrinted :: String
falsePrinted = "#f"
