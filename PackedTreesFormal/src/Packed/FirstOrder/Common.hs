{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
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
         -- * Data definitions
       , DDef(..), DDefs, fromListDD, emptyDD, insertDD
       , lookupDDef, lookupDataCon
       ) where 

import Data.Char
import Control.Monad.State
import Data.List as L
import Data.Map as M
import GHC.Generics
import Text.PrettyPrint.GenericPretty

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

instance Out a => Out (DDef a)
instance (Out k,Out v) => Out (Map k v) where
  doc         = doc . M.toList
  docPrec n v = docPrec n (M.toList v)

-- DDef utilities:
                
-- | Lookup a ddef in its entirety
lookupDDef :: DDefs a -> Var -> DDef a 
lookupDDef = (M.!)

-- -- | Lookup the arguments to a data contstructor.
-- lookupTyCon :: DDefs a -> Var -> [Var]
-- lookupTyCon dds  = tyArgs . lookupDDef dds

-- | Lookup the arguments to a data contstructor.
lookupDataCon :: DDefs a -> Var -> [a]
lookupDataCon dds v = 
   let DDef _ dc = lookupDDef dds v
   in snd $ L.head $ L.filter ((== v) . fst) dc


insertDD :: DDef a -> DDefs a -> DDefs a
insertDD d = M.insert (tyName d) d 

emptyDD :: DDefs a
emptyDD  = M.empty

fromListDD :: [DDef a] -> DDefs a
fromListDD = L.foldr (insertDD) emptyDD 

-- Fundefs
----------------------------------------

-- | A set of top-level recursive function definitions
type FunDefs ty ex = Map Var (FunDef ty ex)

data FunDef ty ex = FunDef { funName  :: Var
                               -- ^ Return type
                           , funArg   :: (Var,ty)
                           , funRetTy :: ty
                           , funBody  :: ex }
  deriving (Read,Show,Eq,Ord, Generic)

instance (Out a, Out b) => Out (FunDef a b)

    
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
