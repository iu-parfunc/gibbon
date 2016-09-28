-- {-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Utilities and common types.

module Packed.Common 
       ( Constr, Value(..), ValEnv
       , DDef(..), DDefs, fromListDD, emptyDD, insertDD
       , lookupDDef, lookupTyCon, lookupDataCon
       , Var, varAppend, SyM, gensym, runSyM) where 

import Data.Map as M
import Data.List as L
import Control.Monad.State
    
type Var    = String
type Constr = String

varAppend :: Var -> Var -> Var
varAppend = (++)

--------------------------------------------------------------------------------

data Value a = VInt Int | VLam (ValEnv a) Var a
             | VProd (Value a) (Value a)
             | VLeft (Value a)
             | VRight (Value a)
             | VPacked Constr [Value a]
  deriving (Read,Show,Eq,Ord)
               
type ValEnv a = Map Var (Value a)

------------------------------------------------------------

-- Primitive for now:
type DDefs a = Map Var (DDef a)

data DDef a = DDef { tyName:: Var
                   , tyArgs:: [Var] 
                   , dataCons :: [(Constr,[a])] }
  deriving (Read,Show,Eq,Ord)

-- | Lookup a ddef in its entirety
lookupDDef :: DDefs a -> Var -> DDef a 
lookupDDef = (M.!)

-- | Lookup the arguments to a data contstructor.
lookupTyCon :: DDefs a -> Var -> [Var]
lookupTyCon dds  = tyArgs . lookupDDef dds

-- | Lookup the arguments to a data contstructor.
lookupDataCon :: DDefs a -> Var -> [a]
lookupDataCon dds v = 
   let DDef _ _ dc = lookupDDef dds v
   in snd $ L.head $ L.filter ((== v) . fst) dc


insertDD :: DDef a -> DDefs a -> DDefs a
insertDD d = M.insert (tyName d) d 

emptyDD :: DDefs a
emptyDD  = M.empty

fromListDD :: [DDef a] -> DDefs a
fromListDD = L.foldr (insertDD) emptyDD 

-- Gensym monad:
----------------------------------------

newtype SyM a = SyM (State Int a)
 deriving (Functor, Applicative, Monad)

gensym :: Var -> SyM Var
gensym v = SyM $ do n <- modify (+1)
                    return (v `varAppend` show n)

runSyM :: Int -> SyM a -> (a,Int)
runSyM n (SyM a) = runState a n
