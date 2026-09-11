{-# LANGUAGE TemplateHaskell #-}

-- | Tests for Unariser2
--
module Unariser
  (unariser2Tests) where

import Data.Set as S
import Data.Map as M

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Gibbon.Common hiding (FunDef, FunDefs)
import Gibbon.L1.Syntax hiding (FunDef, Prog, PreProg(..), FunDefs)
import Gibbon.L3.Syntax
import Gibbon.Passes.Unariser


run :: Exp3 -> Exp3
run x = fst $ defaultRunPassM $ unariserExp False undefined [] (Env2 M.empty M.empty) x

case_t1 :: Assertion
case_t1 = expected @=? actual
  where
    actual = run test1

    test1 :: Exp3
    test1 = LetE ("v1",[],ProdTy [IntTy W64, IntTy W64],
                     MkProdE [mkLitE64 1, mkLitE64 2]) $
            LetE ("v2",[],ProdTy [IntTy W64, ProdTy [IntTy W64, IntTy W64]],
                     MkProdE [mkLitE64 3, VarE "v1"]) $
            LetE ("v6",[],ProdTy [IntTy W64, ProdTy [IntTy W64, IntTy W64]] ,
                     MkProdE [mkLitE64 1, MkProdE [mkLitE64 1, mkLitE64 2]]) $
            LetE ("v4",[],ProdTy [IntTy W64, IntTy W64],
                     ProjE 1 (VarE "v6")) $
            LetE ("v3",[], ProdTy [IntTy W64, ProdTy [IntTy W64, ProdTy [IntTy W64, IntTy W64]]],
                      MkProdE [mkLitE64 4, VarE "v2"]) $
            LetE ("v5",[], ProdTy [IntTy W64, ProdTy [IntTy W64, IntTy W64]],
                     ProjE 1 (VarE "v3")) $
            VarE "v5"

    expected :: Exp3
    expected =  LetE ("v1",[],ProdTy [IntTy W64, IntTy W64],
                         MkProdE [mkLitE64 1, mkLitE64 2]) $
                LetE ("v2",[],ProdTy [IntTy W64,IntTy W64,IntTy W64],
                         MkProdE [mkLitE64 3, ProjE 0 (VarE "v1"), ProjE 1 (VarE "v1")]) $
                LetE ("v6",[],ProdTy [IntTy W64,IntTy W64,IntTy W64] ,
                         MkProdE [mkLitE64 1, mkLitE64 1, mkLitE64 2]) $
                LetE ("v4",[],ProdTy [IntTy W64, IntTy W64],
                         MkProdE [ProjE 1 (VarE "v6"), ProjE 2 (VarE "v6")]) $
                LetE ("v3",[], ProdTy [IntTy W64,IntTy W64,IntTy W64,IntTy W64],
                         MkProdE [mkLitE64 4, ProjE 0 (VarE "v2"), ProjE 1 (VarE "v2"),
                                     ProjE 2 (VarE "v2")]) $
                LetE ("v5",[], ProdTy [IntTy W64,IntTy W64,IntTy W64],
                         MkProdE [ProjE 1 (VarE "v3"), ProjE 2 (VarE "v3"),
                                     ProjE 3 (VarE "v3")]) $
                VarE "v5"

case_t2 :: Assertion
case_t2 = expected @=? actual
  where
    actual = run test2

    test2 :: Exp3
    test2 = LetE ("v1",[],ProdTy [IntTy W64, IntTy W64], MkProdE [mkLitE64 1, mkLitE64 2]) $
            LetE ("v2",[],ProdTy [ProdTy [CursorTy, CursorTy], IntTy W64],
                     MkProdE [VarE "v1", mkLitE64 3]) $
            LetE ("v3",[], IntTy W64, ProjE 1 (VarE "v2")) $
            VarE "v3"

    expected :: Exp3
    expected = LetE ("v1",[],ProdTy [IntTy W64, IntTy W64],
                        MkProdE [mkLitE64 1, mkLitE64 2]) $
               LetE ("v2",[],ProdTy [CursorTy, CursorTy, IntTy W64],
                        MkProdE [ProjE 0 (VarE "v1"), ProjE 1 (VarE "v1"), mkLitE64 3]) $
               LetE ("v3",[], IntTy W64, ProjE 2 (VarE "v2")) $
               VarE "v3"

case_t3 :: Assertion
case_t3 = expected @=? actual
  where
    actual = run test3

    test3 :: Exp3
    test3 = LetE ("v1",[],ProdTy [IntTy W64, IntTy W64], MkProdE [mkLitE64 1, mkLitE64 2]) $
            LetE ("v2",[],ProdTy [IntTy W64, ProdTy [IntTy W64, IntTy W64]],
                     MkProdE [mkLitE64 3, VarE "v1"]) $
            LetE ("v3",[], IntTy W64,
                     ProjE 1 (ProjE 1 (VarE "v2"))) $
            VarE "v3"


    expected :: Exp3
    expected = LetE ("v1",[],ProdTy [IntTy W64, IntTy W64], MkProdE [mkLitE64 1, mkLitE64 2]) $
               LetE ("v2",[],ProdTy [IntTy W64, IntTy W64, IntTy W64],
                        MkProdE [mkLitE64 3, ProjE 0 (VarE "v1"), ProjE 1 (VarE "v1")]) $
               LetE ("v3",[], IntTy W64,
                        ProjE 2 (VarE "v2")) $
               VarE "v3"


case_t4 :: Assertion
case_t4 = expected @=? actual
  where
    actual = run test4

    test4 :: Exp3
    test4 = LetE ("v1",[],ProdTy [IntTy W64, IntTy W64], MkProdE [mkLitE64 1, mkLitE64 2]) $
            LetE ("v2",[],ProdTy [IntTy W64, ProdTy [IntTy W64, IntTy W64]],
                     MkProdE [mkLitE64 3, VarE "v1"]) $
            LetE ("v3",[],ProdTy [IntTy W64, ProdTy [IntTy W64, ProdTy [IntTy W64, IntTy W64]]],
                     MkProdE [mkLitE64 4, VarE "v2"]) $
            LetE ("v4",[], ProdTy [IntTy W64, ProdTy [IntTy W64, IntTy W64]],
                     ProjE 1 (VarE "v3")) $
            VarE "v4"


    expected :: Exp3
    expected = LetE ("v1",[],ProdTy [IntTy W64, IntTy W64], MkProdE [mkLitE64 1, mkLitE64 2]) $
               LetE ("v2",[],ProdTy [IntTy W64, IntTy W64, IntTy W64],
                        MkProdE [mkLitE64 3, ProjE 0 (VarE "v1"), ProjE 1 (VarE "v1")]) $
               LetE ("v3",[],ProdTy [IntTy W64, IntTy W64, IntTy W64, IntTy W64],
                        MkProdE [mkLitE64 4, ProjE 0 (VarE "v2"), ProjE 1 (VarE "v2"), ProjE 2 (VarE "v2")]) $
               LetE ("v4",[], ProdTy [IntTy W64, IntTy W64, IntTy W64],
                        MkProdE [ProjE 1 (VarE "v3"), ProjE 2 (VarE "v3"), ProjE 3 (VarE "v3")]) $
               VarE "v4"


case_t5 :: Assertion
case_t5 = expected @=? actual
  where
    actual = run test5

    test5 :: Exp3
    test5 = LetE ("v1",[],ProdTy [IntTy W64, IntTy W64], MkProdE [mkLitE64 1, mkLitE64 2]) $
            LetE ("v2",[],ProdTy [IntTy W64, ProdTy [IntTy W64, IntTy W64]],
                     MkProdE [mkLitE64 1, MkProdE [ProjE 0 (VarE "v1"), ProjE 1 (VarE "v1")]]) $
            LetE ("v3",[], ProdTy [IntTy W64, ProdTy [IntTy W64, IntTy W64]],
                     MkProdE [mkLitE64 1, MkProdE [mkLitE64 2, ProjE 0 $ ProjE 1 (VarE "v2")]]) $
            VarE "v3"

    expected :: Exp3
    expected = LetE ("v1",[],ProdTy [IntTy W64, IntTy W64], MkProdE [mkLitE64 1, mkLitE64 2]) $
               LetE ("v2",[],ProdTy [IntTy W64, IntTy W64, IntTy W64],
                        MkProdE [mkLitE64 1, ProjE 0 (VarE "v1"), ProjE 1 (VarE "v1")]) $
               LetE ("v3",[], ProdTy [IntTy W64, IntTy W64, IntTy W64],
                        MkProdE [mkLitE64 1, mkLitE64 2, ProjE 1 (VarE "v2")]) $
               VarE "v3"


unariser2Tests :: TestTree
unariser2Tests = $(testGroupGenerator)
