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
assertInferEffects fn@FunDef{funname} expected = expected @=? eff
  where funs  = (M.fromList [(funname , fn)])
        prg   = Prog ddtree funs Nothing

        -- run inferEffects and get the effect from it's type
        Prog{fundefs} = fst $ runSyM 0 $ inferEffects prg
        eff = arrEffs $ funty (fundefs ! funname)


-- | Add1 function has a traversal effect [Traverse "lin2"]
--
case_add1 :: Assertion
case_add1 = assertInferEffects add1Fun (S.singleton $ Traverse "lin2")

-- | Identity fn doesn't have any traversal effect
case_id1 :: Assertion
case_id1 = assertInferEffects id1 S.empty

-- | Copying has a traversal effect
case_copyTree :: Assertion
case_copyTree =  assertInferEffects copyTree (S.singleton $ Traverse "lin23")

-- | Call copy on the body of id1
case_call_copy_on_id1 :: Assertion
case_call_copy_on_id1 = (S.singleton $ Traverse "lin19") @=? eff
  where funs  = (M.fromList [("copyTree" , copyTree),
                             ("id1WithCopy", id1WithCopy)])
        id1WithCopy = id1 { funbod = l$ AppE "copyTree" ["lin19","lout21"]
                                     (l$ VarE "tr18")
                          , funname = "id1WithCopy"
                          }
        prg   = Prog ddtree funs Nothing

        -- run inferEffects and get the effect from it's type
        Prog{fundefs} = fst $ runSyM 0 $ inferEffects prg
        eff = arrEffs $ funty (fundefs ! "id1WithCopy")


inferEffects2Tests :: TestTree
inferEffects2Tests = $(testGroupGenerator)
