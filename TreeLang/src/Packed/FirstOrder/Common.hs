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
       ( -- * Type and Data Constructors
         Constr
         -- * Variables and gensyms
       , Var, varAppend, SyM, gensym, genLetter, runSyM
         -- * Values (for interpreters)
       , Value(..), ValEnv
         -- * Top-level function defs
       , FunDef(..), FunDefs
       , insertFD, fromListFD
         -- * Data definitions
       , DDef(..), DDefs, fromListDD, emptyDD, insertDD
       , lookupDDef, lookupDataCon
         -- * Misc
       , (#), fragileZip, sdoc
         -- * Debugging/logging:
       , dbgLvl, dbgPrint, dbgPrintLn, dbgTrace, dbgTraceIt
       ) where 

import Data.Maybe (catMaybes)
import Data.Char
import Control.Monad.State
import Control.DeepSeq (NFData)
import Data.List as L
import Data.Map as M
import GHC.Generics
import Text.PrettyPrint.GenericPretty
import GHC.Stack (errorWithStackTrace)
import Text.Printf
import System.IO
import System.Environment
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace

-- type CursorVar = Var       
type Var    = String
type Constr = String

varAppend :: Var -> Var -> Var
varAppend = (++)

--------------------------------------------------------------------------------

data Value a = VInt Int
--             | VLam (ValEnv a) Var a
             | VProd [Value a]
--             | VLeft (Value a)
--             | VRight (Value a)
             | VPacked Constr [Value a]
  deriving (Read,Show,Eq,Ord,Generic)

type ValEnv a = Map Var (Value a)
    
------------------------------------------------------------

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
lookupDDef = (#)

-- -- | Lookup the arguments to a data contstructor.
-- lookupTyCon :: DDefs a -> Var -> [Var]
-- lookupTyCon dds  = tyArgs . lookupDDef dds

-- | Lookup the arguments to a data contstructor.
lookupDataCon :: Out a => DDefs a -> Var -> [a]
lookupDataCon dds con =
   -- Here we try to lookup in ALL datatypes, assume unique:
  let res = catMaybes 
            [ L.lookup con dataCons
            | DDef {dataCons} <- M.elems dds ] in
  -- trace ("Looked up "++show con++" got "++show (doc res)) $ 
  case res of   
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
 deriving (Functor, Applicative, Monad)

-- | Generate a unique symbol by attaching a numeric suffix.
gensym :: Var -> SyM Var
gensym v = SyM $ do modify (+1)
                    n <- get
                    return (v `varAppend` show n)

-- | Generate alphabetic variables 'a','b',...
genLetter :: SyM Var
genLetter = SyM $ 
    do n <- get
       modify (+1)       
       return [chr (n + ord 'a')]


runSyM :: Int -> SyM a -> (a,Int)
runSyM n (SyM a) = runState a n

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

sdoc :: Out a => a -> String
sdoc = show . doc                   

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
         trace (" ! Responding to env Var: DEBUG="++s)$
         case reads s of
           ((n,_):_) -> n
           [] -> error$"Attempt to parse DEBUG env var as Int failed: "++show s

defaultDbg :: Int
defaultDbg = 0

-- | Print if the debug level is at or above a threshold.
dbgPrint :: Int -> String -> IO ()
dbgPrint lvl str = if dbgLvl < lvl then return () else do
    hPutStrLn stderr str
    -- hPrintf stderr str 
    -- hFlush stderr
    -- printf str
    hFlush stdout

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
