{-# LANGUAGE TemplateHaskell #-}

-- | Tests for HoistBoundsCheck
module HoistBoundsCheck where

import Data.Map as M
import Data.Set as S
import qualified Data.List as L
import Gibbon.Common hiding (FunDef)
import Gibbon.NewL2.Syntax as L2
import Gibbon.L2.Syntax as OldL2
import Gibbon.Passes.HoistBoundsCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

case_t1 :: Assertion
case_t1 = expected @=? actual
  where
    actual = fst $ defaultPackedRunPassM $ hoistBoundsCheck test1 S.empty

    test1 :: L2.Exp2
    test1 =
      LetE ("b", [], MkTy2 BoolTy, mkLitE64 1) $
        IfE
          (VarE "b")
          ( Ext
              $ LetRegionE (VarR "r1") Undefined RegionImmutable Nothing
              $ LetE
                ( "_",
                  [],
                  MkTy2 (IntTy W64),
                  Ext $
                    BoundsCheck
                      10
                      (Loc (LREM (singleLocVar "l1") (SingleR "r1") (SingleR "end_r1") Output))
                      (EndOfReg (SingleR "r1") Output (SingleR "end_r1"))
                )
              $ Ext
              $ LetLocE (Loc (LREM (singleLocVar "l1") (SingleR "r1") (SingleR "end_r1") Output)) (StartOfRegionLE (VarR "r1"))
              $ (MkProdE [])
          )
          (MkProdE [])

    expected :: L2.Exp2
    expected =
      Ext
        $ LetRegionE (VarR "r1") Undefined RegionImmutable Nothing
        $ Ext
        $ LetLocE (Loc (LREM (singleLocVar "l1") (SingleR "r1") (SingleR "end_r1") Output)) (StartOfRegionLE (VarR "r1"))
        $ LetE
          ( "_",
            [],
            MkTy2 (IntTy W64),
            Ext $
              BoundsCheck
                10
                (Loc (LREM (singleLocVar "l1") (SingleR "r1") (SingleR "end_r1") Output))
                (EndOfReg (SingleR "r1") Output (SingleR "end_r1"))
          )
        $ LetE ("b", [], MkTy2 BoolTy, mkLitE64 1)
        $ IfE
          (VarE "b")
          (MkProdE [])
          (MkProdE [])


-- | NOTE: the two region bindings here are independent, so either nesting
-- order is correct.  'hoistBoundsCheck' folds over a set keyed by 'Var', and
-- 'Var' wraps an interned 'Symbol' whose 'Ord' follows global interning order
-- -- so the order this pass emits them in depends on what else the process has
-- interned.  Normalise the leading region chain on both sides so this test
-- asserts what it means (the bounds check is hoisted above both regions)
-- rather than an incidental ordering.  The underlying interning-order
-- dependence in the pass is a separate, pre-existing issue.
case_t2 :: Assertion
case_t2 = sortRegionPrefix expected @=? sortRegionPrefix actual
  where
    actual = fst $ defaultPackedRunPassM $ hoistBoundsCheck test2 S.empty

    test2 :: L2.Exp2
    test2 =
      LetE ("b", [], MkTy2 BoolTy, mkLitE64 1) $
        IfE
          (VarE "b")
          ( Ext
              $ LetRegionE (VarR "r1") Undefined RegionImmutable Nothing
              $ LetE
                ( "_",
                  [],
                  MkTy2 (IntTy W64),
                  Ext $
                    BoundsCheckVector
                      [
                        (
                          10,
                          (Loc (LREM (singleLocVar "l1") (SingleR "r1") (SingleR "end_r1") Output)),
                          (EndOfReg (SingleR "r1") Output (SingleR "end_r1"))
                        ) , 
                        (
                          10,
                          (Loc (LREM (singleLocVar "l2") (SingleR "r2") (SingleR "end_r2") Output)),
                          (EndOfReg (SingleR "r2") Output (SingleR "end_r2"))
                        )
                      ]
                )
              $ Ext
              $ LetLocE (Loc (LREM (singleLocVar "l1") (SingleR "r1") (SingleR "end_r1") Output)) (StartOfRegionLE (VarR "r1"))
              $ Ext 
              $ LetRegionE (VarR "r2") Undefined RegionImmutable Nothing
              $ Ext
              $ LetLocE (Loc (LREM (singleLocVar "l2") (SingleR "r2") (SingleR "end_r2") Output)) (StartOfRegionLE (VarR "r2"))
              $ (MkProdE [])
          )
          (MkProdE [])

    expected :: L2.Exp2
    expected =
      Ext
        $ LetRegionE (VarR "r2") Undefined RegionImmutable Nothing
        $ Ext
        $ LetRegionE (VarR "r1") Undefined RegionImmutable Nothing
        $ Ext
        $ LetLocE (Loc (LREM (singleLocVar "l1") (SingleR "r1") (SingleR "end_r1") Output)) (StartOfRegionLE (VarR "r1"))
        $ Ext 
        $ LetLocE (Loc (LREM (singleLocVar "l2") (SingleR "r2") (SingleR "end_r2") Output)) (StartOfRegionLE (VarR "r2"))
        $ LetE
          ( "_",
            [],
            MkTy2 (IntTy W64),
            Ext $
              BoundsCheckVector 
              [
                (
                  10,
                  (Loc (LREM (singleLocVar "l1") (SingleR "r1") (SingleR "end_r1") Output)),
                  (EndOfReg (SingleR "r1") Output (SingleR "end_r1"))
                )
                ,
                (
                  10,
                  (Loc (LREM (singleLocVar "l2") (SingleR "r2") (SingleR "end_r2") Output)),
                  (EndOfReg (SingleR "r2") Output (SingleR "end_r2"))
                )
              ]
          )
        $ LetE ("b", [], MkTy2 BoolTy, mkLitE64 1)
        $ IfE
          (VarE "b")
          (MkProdE [])
          (MkProdE [])


-- changes branch compared to test1
case_t3 :: Assertion
case_t3 = expected @=? actual
  where
    actual = fst $ defaultPackedRunPassM $ hoistBoundsCheck test3 S.empty

    test3 :: L2.Exp2
    test3 =
      LetE ("b", [], MkTy2 BoolTy, mkLitE64 1) $
        IfE
          (VarE "b")
          (MkProdE [])
          ( Ext
              $ LetRegionE (VarR "r1") Undefined RegionImmutable Nothing
              $ LetE
                ( "_",
                  [],
                  MkTy2 (IntTy W64),
                  Ext $
                    BoundsCheck
                      10
                      (Loc (LREM (singleLocVar "l1") (SingleR "r1") (SingleR "end_r1") Output))
                      (EndOfReg (SingleR "r1") Output (SingleR "end_r1"))
                )
              $ Ext
              $ LetLocE (Loc (LREM (singleLocVar "l1") (SingleR "r1") (SingleR "end_r1") Output)) (StartOfRegionLE (VarR "r1"))
              $ (MkProdE [])
          )
    
    expected :: L2.Exp2
    expected =
      Ext
        $ LetRegionE (VarR "r1") Undefined RegionImmutable Nothing
        $ Ext
        $ LetLocE (Loc (LREM (singleLocVar "l1") (SingleR "r1") (SingleR "end_r1") Output)) (StartOfRegionLE (VarR "r1"))
        $ LetE
          ( "_",
            [],
            MkTy2 (IntTy W64),
            Ext $
              BoundsCheck
                10
                (Loc (LREM (singleLocVar "l1") (SingleR "r1") (SingleR "end_r1") Output))
                (EndOfReg (SingleR "r1") Output (SingleR "end_r1"))
          )
        $ LetE ("b", [], MkTy2 BoolTy, mkLitE64 1)
        $ IfE
          (VarE "b")
          (MkProdE [])
          (MkProdE [])


hoistBoundsCheckTests :: TestTree
hoistBoundsCheckTests = $(testGroupGenerator)

-- | Sort a leading chain of independent 'LetRegionE' bindings by region name.
sortRegionPrefix :: L2.Exp2 -> L2.Exp2
sortRegionPrefix e0 =
  let (regs, body) = peel e0
  in rebuild (L.sortOn (\(r,_,_,_) -> show r) regs) body
  where
    peel (Ext (LetRegionE r sz m ty bod)) =
      let (rs, b) = peel bod in ((r,sz,m,ty):rs, b)
    peel e = ([], e)
    rebuild [] b = b
    rebuild ((r,sz,m,ty):rs) b = Ext (LetRegionE r sz m ty (rebuild rs b))
