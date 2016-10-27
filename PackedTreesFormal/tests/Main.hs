{-# LANGUAGE TemplateHaskell #-}

-- |

module Main where

import Test.Tasty.HUnit
import Test.Tasty.TH
import qualified Packed.FirstOrder.Common as C
import Packed.FirstOrder.Common hiding (FunDef)
import qualified Packed.FirstOrder.L1_Source as L1
import Packed.FirstOrder.L1_Source (Exp(..))
import Packed.FirstOrder.LTraverse
import Data.Set as S
import Data.Map as M

main :: IO ()
main = $(defaultMainGenerator)
    
-- Unit test the LTraverse.hs functions:
--------------------------------------------------------------------------------

t0 :: Set Effect -> Set Effect
t0 eff = fst $ runSyM 0 $
     inferEffects (M.empty,
                   M.singleton "foo" (ArrowTy (PackedTy "K" "p")
                                              eff
                                              (PackedTy "K" "p")))
                  (C.FunDef "foo" ("x", L1.Packed "K") (L1.Packed "K")
                        (L1.AppE "foo" (L1.VarE "x")))

case_t0 :: Assertion
case_t0 = assertEqual "infinite loop traverses anything"
                     (S.fromList [Traverse "p"]) (t0 (S.singleton (Traverse "p")))

case_t0b :: Assertion
case_t0b = assertEqual "infinite loop cannot bootstrap with bad initial effect set"
                     S.empty (t0 S.empty)

                                                 
-- The function foo below should traverse "a" but does not have any
-- output locations.
t1 :: (Set Effect)
t1 = fst $ runSyM 0 $
     inferEffects (M.empty,
                   M.fromList 
                   [("copy",(ArrowTy (PackedTy "K" "p")
                                                   (S.fromList [Traverse "p", Traverse "o"])
                                              (PackedTy "K" "o")))
                   ,("foo", ArrowTy (PackedTy "K" "a") S.empty IntTy)])
                  (C.FunDef "foo" ("x", L1.Packed "K") L1.IntTy $
                     L1.LetE ("ignr",L1.Packed "K", (L1.AppE "copy" (L1.VarE "x"))) $
                       L1.LitE 33
                  )

case_t1 :: Assertion
case_t1 = assertEqual "traverse input via another call"
          (S.fromList [Traverse "a"]) t1 

t2 :: (Set Effect)
t2 = fst $ runSyM 0 $
     inferEffects ( fromListDD [DDef "Bool" [("True",[]), ("False",[])]]
                  , M.fromList [("foo", ArrowTy (PackedTy "Bool" "p") S.empty IntTy)])
                  (C.FunDef "foo" ("x", L1.Packed "Bool") L1.IntTy $
                    L1.CaseE (VarE "x") $ M.fromList 
                      [ ("True", ([],LitE 3))
                      , ("False", ([],LitE 3)) ])
     
case_t2 :: Assertion
case_t2 = assertEqual "Traverse a Bool with case"
            (S.fromList [Traverse "p"]) t2
           
t2b :: (Set Effect)
t2b = fst $ runSyM 0 $
     inferEffects ( fromListDD [DDef "Bool" [("True",[]), ("False",[])]]
                  , M.fromList [("foo", ArrowTy (PackedTy "Bool" "p") S.empty IntTy)])
                  (C.FunDef "foo" ("x", L1.Packed "Bool") L1.IntTy $
                    LitE 33)

case_t2b :: Assertion
case_t2b = assertEqual "No traverse from a lit" S.empty t2b
                  
t2c :: (Set Effect)
t2c = fst $ runSyM 0 $
     inferEffects ( fromListDD [DDef "Bool" [("True",[]), ("False",[])]]
                  , M.fromList [("foo", ArrowTy (PackedTy "Bool" "p") S.empty IntTy)])
                  (C.FunDef "foo" ("x", L1.Packed "Bool") L1.IntTy $
                    VarE "x")

case_t2c :: Assertion
case_t2c = assertEqual "No traverse from identity function" S.empty t2b
