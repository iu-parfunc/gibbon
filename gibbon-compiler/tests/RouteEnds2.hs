{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for RouteEnds2
--
module RouteEnds2 where

import Data.Set as S
import Data.Map as M

import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty

import Packed.FirstOrder.Common
import Packed.FirstOrder.L2.Syntax as L2
import Packed.FirstOrder.Passes.RouteEnds2
import qualified Packed.FirstOrder.L1.Syntax as L1

--
import Common


test1 :: Exp2
test1 = Ext $ LetRegionE (VarR "r") $ Ext $ LetLocE "ltest" (StartOfLE (VarR "r")) $
        Ext $ LetLocE "ltest1" (AfterConstantLE 1 "ltest") $
        LetE ("x", [], PackedTy "Tree" "ltest1", DataConE "ltest1" "Leaf" [LitE 1]) $
        Ext $ LetLocE "ltest2" (AfterVariableLE "x" "ltest1") $
        LetE ("y", [], PackedTy "Tree" "ltest2", DataConE "ltest2" "Leaf" [LitE 2]) $
        LetE ("z", [], PackedTy "Tree" "ltest", DataConE "ltest" "Node" [VarE "x", VarE "y"]) $
        Ext $ LetRegionE (VarR "o") $ Ext $ LetLocE "lo" (StartOfLE (VarR "o")) $
        AppE "add1" ["l","lo"] (VarE "z")


expectedTest1 :: L2.Prog
expectedTest1 = Prog {ddefs = M.fromList [(Var "Tree",DDef {tyName = Var "Tree", dataCons = [("Leaf",[(False,IntTy)]),("Node",[(False,PackedTy "Tree" (Var "l")),(False,PackedTy "Tree" (Var "l"))])]})], fundefs = M.fromList [(Var "add1",L2.FunDef {funname = Var "add1", funty = ArrowTy {locVars = [LRM (Var "lin") (VarR (Var "r1")) Input,LRM (Var "lout") (VarR (Var "r1")) Output], arrIn = PackedTy "Tree" (Var "lin"), arrEffs = S.fromList [Traverse (Var "lin")], arrOut = PackedTy "Tree" (Var "lout"), locRets = [EndOf (LRM (Var "lin") (VarR (Var "r1")) Input)]}, funarg = Var "tr", funbod = CaseE (VarE (Var "tr")) [("Leaf",[(Var "n",Var "l0")],Ext (LetLocE (Var "jump1") (AfterConstantLE 1 (Var "l0")) (LetE (Var "v",[],IntTy,PrimAppE L1.AddP [VarE (Var "n"),LitE 1]) (LetE (Var "lf",[],PackedTy "Tree" (Var "lout"),DataConE (Var "lout") "Leaf" [VarE (Var "v")]) (Ext (RetE [Var "jump1"] (Var "lf"))))))),("Node",[(Var "x",Var "l1"),(Var "y",Var "l2")],Ext (LetLocE (Var "lout1") (AfterConstantLE 1 (Var "lout")) (LetE (Var "x1",[Var "endof2"],PackedTy "Tree" (Var "lout1"),AppE (Var "add1") [Var "l1",Var "lout1"] (VarE (Var "x"))) (Ext (LetLocE (Var "l2") (FromEndLE (Var "endof2")) (Ext (LetLocE (Var "lout2") (AfterVariableLE (Var "x1") (Var "lout1")) (LetE (Var "y1",[Var "endof3"],PackedTy "Tree" (Var "lout2"),AppE (Var "add1") [Var "l2",Var "lout2"] (VarE (Var "y"))) (LetE (Var "z",[],PackedTy "Tree" (Var "lout"),DataConE (Var "lout") "Node" [VarE (Var "x1"),VarE (Var "y1")]) (Ext (RetE [Var "endof3"] (Var "z"))))))))))))]})], mainExp = Just (Ext (LetRegionE (VarR (Var "r")) (Ext (LetLocE (Var "ltest") (StartOfLE (VarR (Var "r"))) (Ext (LetLocE (Var "ltest1") (AfterConstantLE 1 (Var "ltest")) (LetE (Var "x",[],PackedTy "Tree" (Var "ltest1"),DataConE (Var "ltest1") "Leaf" [LitE 1]) (Ext (LetLocE (Var "ltest2") (AfterVariableLE (Var "x") (Var "ltest1")) (LetE (Var "y",[],PackedTy "Tree" (Var "ltest2"),DataConE (Var "ltest2") "Leaf" [LitE 2]) (LetE (Var "z",[],PackedTy "Tree" (Var "ltest"),DataConE (Var "ltest") "Node" [VarE (Var "x"),VarE (Var "y")]) (Ext (LetRegionE (VarR (Var "o")) (Ext (LetLocE (Var "lo") (StartOfLE (VarR (Var "o"))) (LetE (Var "tailapp4",[Var "endof5"],PackedTy "Tree" (Var "lout"),AppE (Var "add1") [Var "l",Var "lo"] (VarE (Var "z"))) (Ext (RetE [] (Var "tailapp4")))))))))))))))))),IntTy)}


case_add1_test1 :: Assertion
case_add1_test1 = actualTest1 @=? expectedTest1
  where
    ddfs  = ddtree
    funs  = (M.fromList [(toVar "add1",add1Fun)])
    prg   = Prog ddfs funs (Just (test1,IntTy))
    actualTest1   = fst $ runSyM 1 $ routeEnds prg

routeEnds2Tests :: TestTree
routeEnds2Tests = $(testGroupGenerator)


-- FunDef {funname = Var "add1",
-- funty = ArrowTy {locVars = [LRM (Var "lin") (VarR (Var "r1")) Input,LRM (Var "lout") (VarR (Var "r1")) Output],
--                  arrIn = PackedTy "tree" (Var "lin"),
--                  arrEffs = fromList [Traverse (Var "lin")],
--                  arrOut = PackedTy "tree" (Var "lout"),
--                  locRets = [EndOf (LRM (Var "lin") (VarR (Var "r1")) Input)]},
-- funarg = Var "tr",
-- funbod = CaseE (VarE (Var "tr"))
--          [("Leaf",[(Var "n",Var "l0")],
--           Ext (LetLocE (Var "jump1") (AfterConstantLE 1 (Var "l0") (Var "jump1"))
--           (LetE (Var "v",[],IntTy,PrimAppE AddP [VarE (Var "n"),LitE 1]) (Ext (RetE [Var "jump1"] (Var "v")))))),
--           ("Node",[(Var "x",Var "l1"),(Var "y",Var "l2")],
--           Ext (LetLocE (Var "lout1") (AfterConstantLE 1 (Var "lout") (Var "lout1"))
--           (LetE (Var "x1",[Var "endof2"],PackedTy "Tree" (Var "lout1"),AppE (Var "add1") [Var "l1",Var "lout1"] (VarE (Var "x")))
--           (Ext (LetLocE (Var "l2") (FromEndLE (Var "endof2")) (Ext (LetLocE (Var "lout2") (AfterVariableLE (Var "x1") (Var "lout1") (Var "lout2"))
--           (LetE (Var "y1",[Var "endof3"],PackedTy "Tree" (Var "lout2"),AppE (Var "add1") [Var "l2",Var "lout2"] (VarE (Var "y")))
--           (LetE (Var "z",[],PackedTy "Tree" (Var "lout"),DataConE (Var "lout") "Node" [VarE (Var "x1"),VarE (Var "y1")])
--           (ext (RetE [Var "endof3"] (Var "z"))))))))))))]}
