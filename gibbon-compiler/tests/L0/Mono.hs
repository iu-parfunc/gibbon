{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Tests for the L0 specializer
module L0.Mono where

import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty

import Control.Monad.Except
import Data.Loc
import Data.Map as M
import Data.Set as S

import Packed.FirstOrder.Common 
import Packed.FirstOrder.L0.Syntax as L0
import Packed.FirstOrder.L1.Syntax as L1
import Packed.FirstOrder.L0.Mono

type Exp = L Exp0

-- |
assertValue :: L0Fun -> L0Call -> (L0Fun, L0Call) -> Assertion
assertValue f call expected = let actual = tester f call
                              in expected @=? actual


tester :: L0Fun -> L0Call -> (L0Fun, L0Call)
tester f c = let (newf, newc) = specialize f c in (newf, newc $ c)

specializeTests :: TestTree
specializeTests = $(testGroupGenerator)

--------------------------------------------------------------------------------

-- function f \ x -> x of type Int -> Int
t1Fun :: L0Fun
t1Fun = VarDef (toVar "f") (ArrowTy L0.IntTy L0.IntTy) (l $ Ext $ LambdaE (toVar "x") $ l $ VarE $ toVar "x")

-- call (f 3)
t1Call :: L0Call
t1Call = l $ Ext $ PolyAppE (l $ VarE (toVar "f")) (l $ LitE 3)

t1Ex :: (L0Fun, L0Call)
t1Ex = (VarDef (toVar "f1") L0.IntTy (l $ LitE 3), l $ VarE $ toVar "f1")

case_t1 :: Assertion
case_t1 = assertValue t1Fun t1Call t1Ex

-- function f1 \ (f) (x) (y) -> (+ (f x) y)
t2Fun :: L0Fun
t2Fun = VarDef (toVar "f1") (ArrowTy (ArrowTy L0.IntTy L0.IntTy) (ArrowTy L0.IntTy (ArrowTy L0.IntTy L0.IntTy)))
        (l $ Ext $ LambdaE (toVar "f")
          (l $ Ext $ LambdaE (toVar "x")
            (l $ Ext $ LambdaE (toVar "y")
               (l $ PrimAppE AddP [(l $ Ext $ PolyAppE (l $ VarE $ toVar "f") (l $ VarE $ toVar "x")),
                                   (l $ VarE $ toVar "y")]))))

-- call (f1 (\ x -> x) 4 6)
t2Call :: L0Call
t2Call = (l $ Ext $ PolyAppE (l $ Ext $ PolyAppE (l $ Ext $ PolyAppE (l $ VarE $ toVar "f1") (l $ Ext $ LambdaE (toVar "x") (l $ VarE $ toVar "x")))
                                                 (l $ LitE 4))
                             (l $ LitE 6))
           

t2Ex :: (L0Fun, L0Call)
t2Ex = (VarDef (toVar "f11") L0.IntTy (l $ PrimAppE AddP [(l $ LitE 4),(l $ LitE 6)]),
        l $ VarE $ toVar "f11")
  
case_t2 :: Assertion
case_t2 = assertValue t2Fun t2Call t2Ex

-- function f1 \ (f) (x) (y) -> (+ (f x) y)
t3Fun :: L0Fun
t3Fun = VarDef (toVar "f1") (ArrowTy (ArrowTy L0.IntTy L0.IntTy) (ArrowTy L0.IntTy (ArrowTy L0.IntTy L0.IntTy)))
        (l $ Ext $ LambdaE (toVar "f")
          (l $ Ext $ LambdaE (toVar "x")
            (l $ Ext $ LambdaE (toVar "y")
               (l $ PrimAppE AddP [(l $ Ext $ PolyAppE (l $ VarE $ toVar "f") (l $ VarE $ toVar "x")),
                                   (l $ VarE $ toVar "y")]))))

-- call (f1 (\ x -> (+ x x)) 4 6)
t3Call :: L0Call
t3Call = (l $ Ext $ PolyAppE (l $ Ext $ PolyAppE (l $ Ext $ PolyAppE (l $ VarE $ toVar "f1")
                                                                     (l $ Ext $ LambdaE (toVar "x")
                                                                                (l $ PrimAppE AddP [(l $ VarE $ toVar "x"),
                                                                                                    (l $ VarE $ toVar "x")])))
                                                 (l $ LitE 4))
                             (l $ LitE 6))
           

t3Ex :: (L0Fun, L0Call)
t3Ex = (VarDef (toVar "f11") L0.IntTy (l $ PrimAppE AddP [(l $ PrimAppE AddP [(l $ LitE 4), (l $ LitE 4)]),(l $ LitE 6)]),
        l $ VarE $ toVar "f11")
  
case_t3 :: Assertion
case_t3 = assertValue t3Fun t3Call t3Ex

-- function f1 \ (f) (x) (y) -> (+ (f x) y)
t4Fun :: L0Fun
t4Fun = VarDef (toVar "f1") (ArrowTy (ArrowTy L0.IntTy L0.IntTy) (ArrowTy L0.IntTy (ArrowTy L0.IntTy L0.IntTy)))
        (l $ Ext $ LambdaE (toVar "f")
          (l $ Ext $ LambdaE (toVar "x")
            (l $ Ext $ LambdaE (toVar "y")
               (l $ PrimAppE AddP [(l $ Ext $ PolyAppE (l $ VarE $ toVar "f") (l $ VarE $ toVar "x")),
                                   (l $ VarE $ toVar "y")]))))

-- call (f1 (\ x -> (+ x 2)) 4 6)
t4Call :: L0Call
t4Call = (l $ Ext $ PolyAppE (l $ Ext $ PolyAppE (l $ Ext $ PolyAppE (l $ VarE $ toVar "f1")
                                                                     (l $ Ext $ LambdaE (toVar "x")
                                                                                (l $ PrimAppE AddP [(l $ VarE $ toVar "x"),
                                                                                                    (l $ LitE 2)])))
                                                 (l $ LitE 4))
                             (l $ LitE 6))
           

t4Ex :: (L0Fun, L0Call)
t4Ex = (VarDef (toVar "f11") L0.IntTy (l $ PrimAppE AddP [(l $ PrimAppE AddP [(l $ LitE 4), (l $ LitE 2)]),(l $ LitE 6)]),
        l $ VarE $ toVar "f11")
  
case_t4 :: Assertion
case_t4 = assertValue t4Fun t4Call t4Ex

-- map example
listTy :: Ty0
listTy = L0.PackedTy "List" []

mapF :: L0Fun
mapF = VarDef (toVar "map") (ArrowTy (ArrowTy L0.IntTy L0.IntTy) (ArrowTy listTy listTy)) mapBod

mapBod :: L Exp0
mapBod = l $ Ext $ LambdaE (toVar "f")
         (l $ Ext $ LambdaE (toVar "l")
          (l $ CaseE (l $ VarE $ toVar "l")
            [ ("Empty", [], l$ DataConE () "Empty" []),
              ("Cons", [(toVar "a", ()), (toVar "d",())],
               l$ DataConE () "Cons"
                [ l$ Ext $ PolyAppE (l $ VarE $ toVar "f") (l$ VarE $ toVar "a")
                , l$ Ext $ PolyAppE (l$ Ext $ PolyAppE (l $ VarE $ toVar "map") (l$ VarE (toVar "f")))
                                    (l$ VarE $ toVar "d")])
            ]))

mapCall :: L Exp0
mapCall = l $ Ext $ PolyAppE (l $ Ext $ PolyAppE (l $ VarE $ toVar "map")
                              (l $ Ext $ LambdaE (toVar "x") (l $ PrimAppE AddP [(l $ VarE $ toVar "x"), (l $ LitE 2)])))
                             (l $ VarE $ toVar "ls")

mapEx :: (L0Fun, L0Call)
mapEx = (VarDef (toVar "map1") (ArrowTy listTy listTy) mapBodEx,
         l $ Ext $ PolyAppE (l $ VarE $ toVar "map1") (l $ VarE $ toVar "ls"))

mapBodEx :: L Exp0
mapBodEx = l $ Ext $ LambdaE (toVar "l")
            (l $ CaseE (l $ VarE $ toVar "l")
             [ ("Empty", [], l$ DataConE () "Empty" []),
               ("Cons", [(toVar "a", ()), (toVar "d",())],
                l$ DataConE () "Cons"
                [ l$ PrimAppE AddP [(l$ VarE $ toVar "a"), (l$ LitE 2)]
                , l$ Ext $ PolyAppE (l $ VarE $ toVar "map1") (l$ VarE (toVar "d"))])
             ])

case_map :: Assertion
case_map = assertValue mapF mapCall mapEx

