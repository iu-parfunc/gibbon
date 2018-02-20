{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Tests for InferLocations2
--
module InferLocations2 where

import Data.Set as S
import Data.Map as M

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Packed.FirstOrder.Common hiding (FunDef)
import Packed.FirstOrder.L2.Syntax as L2
import Packed.FirstOrder.L2.Examples
import Packed.FirstOrder.Passes.InferLocations2
import qualified Packed.FirstOrder.L1.Syntax as L1


import qualified Control.Monad.Trans.State.Strict as St
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans (lift)


-- some basic tests of unification, some should succeed and others should fail
utest1 = runSyM 0 $ St.runStateT (runExceptT m) M.empty
    where m = do
            u1 <- fixLoc "a"
            u2 <- fixLoc "b"
            l1 <- fresh 
            l2 <- fresh
            assocLoc l1 u1
            assocLoc l2 u2 
            unify l1 l2 (return True) (return False)

utest2 = runSyM 0 $ St.runStateT (runExceptT m) M.empty
    where m = do
            u1 <- fixLoc "a"
            u2 <- fixLoc "b"
            l1 <- fresh 
            l2 <- fresh
            assocLoc l1 u1
            assocLoc l2 u1 
            unify l1 l2 (return True) (return False)

utest3 = runSyM 0 $ St.runStateT (runExceptT m) M.empty
    where m = do
            u1 <- fixLoc "a"
            u2 <- fixLoc "b"
            u3 <- freshUnifyLoc
            l1 <- fresh 
            l2 <- fresh
            l3 <- fresh
            assocLoc l1 u1
            assocLoc l2 u2
            assocLoc l3 u3
            unify l1 l3 (unify l2 l3 (return True) (return False)) (return False)

utest4 = runSyM 0 $ St.runStateT (runExceptT m) M.empty
    where m = do
            u1 <- fixLoc "a"
            u2 <- fixLoc "b"
            u3 <- freshUnifyLoc
            l1 <- fresh 
            l2 <- fresh
            l3 <- fresh
            l4 <- fresh
            assocLoc l1 u1
            assocLoc l2 u2
            assocLoc l3 u3
            unify l1 l3 (unify l4 l2 (return True) (return False)) (return False)

case_unify1 :: Assertion
case_unify1 = (Right False) @=? (fst $ fst utest1)

case_unify2 :: Assertion
case_unify2 = (Right True) @=? (fst $ fst utest2)

case_unify3 :: Assertion
case_unify3 = (Right False) @=? (fst $ fst utest3)

case_unify4 :: Assertion
case_unify4 = (Right True) @=? (fst $ fst utest4)

inferLocations2Tests :: TestTree
inferLocations2Tests = $(testGroupGenerator)
