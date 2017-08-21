{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Tests for RouteEnds2
--
module InferEffects2 where

import Data.Set as S
import Data.Map as M

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Packed.FirstOrder.Common hiding (FunDef)
import Packed.FirstOrder.L2.Syntax as L2
import Packed.FirstOrder.Passes.InferEffects2
import qualified Packed.FirstOrder.L1.Syntax as L1

import Common

assertInferEffects :: FunDef -> Set Effect -> Assertion
assertInferEffects fn@FunDef{funname} expected = eff @=? expected
  where funs  = (M.fromList [(funname , fn)])
        prg   = Prog ddtree funs Nothing

        -- run inferEffects and get the effect from it's type
        Prog{fundefs} = fst $ runSyM 0 $ inferEffects prg
        eff = arrEffs $ funty (fundefs ! funname)


-- | Add1 function has a traversal effect [Traverse "lin"]
--
case_add1 :: Assertion
case_add1 = assertInferEffects add1Fun (S.singleton $ Traverse "lin")

-- | Identity fn doesn't have any traversal effect
case_id1 :: Assertion
case_id1 = assertInferEffects id1 S.empty

inferEffects2Tests :: TestTree
inferEffects2Tests = $(testGroupGenerator)
