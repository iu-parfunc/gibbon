-- {-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Utilities and common types.

module Packed.Common 
       ( Constr, Value(..), ValEnv, DDef(..)
       , Var, varAppend, SyM, gensym, runSyM) where 

import Data.Map
import Control.Monad.State
    
type Var    = String
type Constr = String

data Value a = VInt Int | VLam (ValEnv a) Var a
             | VProd (Value a) (Value a)
             | VLeft (Value a)
             | VRight (Value a)
             | VPacked Constr [Value a]
  deriving (Read,Show,Eq,Ord)
               
type ValEnv a = Map Var (Value a)

data DDef a = DDef Var [(Constr,[a])]
  deriving (Read,Show,Eq,Ord)
           
varAppend :: Var -> Var -> Var
varAppend = (++)
           
-- Gensym monad:
----------------------------------------

newtype SyM a = SyM (State Int a)
 deriving (Functor, Applicative, Monad)

gensym :: Var -> SyM Var
gensym v = SyM $ do n <- modify (+1)
                    return (v `varAppend` show n)

runSyM :: Int -> SyM a -> (a,Int)
runSyM n (SyM a) = runState a n
