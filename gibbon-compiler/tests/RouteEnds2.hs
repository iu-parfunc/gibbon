{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for RouteEnds2
--
module RouteEnds2 where

import Data.Loc
import Data.Set as S
import Data.Map as M

import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty

import Packed.FirstOrder.Common
import Packed.FirstOrder.L2.Syntax as L2
import Packed.FirstOrder.Passes.RouteEnds2
import qualified Packed.FirstOrder.L1.Syntax as L1

import Common


test1 :: L Exp2
test1 = L NoLoc $ Ext $ LetRegionE (VarR "r") $ L NoLoc $ Ext $ LetLocE "ltest" (StartOfLE (VarR "r")) $
        L NoLoc $ Ext $ LetLocE "ltest1" (AfterConstantLE 1 "ltest") $
        L NoLoc $ LetE ("x", [], PackedTy "Tree" "ltest1", L NoLoc $ DataConE "ltest1" "Leaf" [L NoLoc $ LitE 1]) $
        L NoLoc $ Ext $ LetLocE "ltest2" (AfterVariableLE "x" "ltest1") $
        L NoLoc $ LetE ("y", [], PackedTy "Tree" "ltest2", L NoLoc $ DataConE "ltest2" "Leaf" [L NoLoc $ LitE 2]) $
        L NoLoc $ LetE ("z", [], PackedTy "Tree" "ltest", L NoLoc $ DataConE "ltest" "Node" [L NoLoc $ VarE "x", L NoLoc $ VarE "y"]) $
        L NoLoc $ Ext $ LetRegionE (VarR "o") $ L NoLoc $ Ext $ LetLocE "lo" (StartOfLE (VarR "o")) $
        L NoLoc $ AppE "add1" ["l","lo"] (L NoLoc $ VarE "z")

expectedTest1 :: L2.Prog
expectedTest1 = Prog {ddefs = M.fromList [(Var "Tree",DDef {tyName = Var "Tree", dataCons = [("Leaf",[(False,IntTy)]),("Node",[(False,PackedTy "Tree" (Var "l")),(False,PackedTy "Tree" (Var "l"))])]})], fundefs = M.fromList [(Var "add1",L2.FunDef {funName = Var "add1", funTy = ArrowTy {locVars = [LRM (Var "lin") (VarR (Var "r1")) Input,LRM (Var "lout") (VarR (Var "r1")) Output], arrIn = PackedTy "Tree" (Var "lin"), arrEffs = S.fromList [Traverse (Var "lin")], arrOut = PackedTy "Tree" (Var "lout"), locRets = [EndOf (LRM (Var "lin") (VarR (Var "r1")) Input)]}, funArg = Var "tr", funBody = L NoLoc $ CaseE (L NoLoc $ VarE (Var "tr")) [("Leaf",[(Var "n",Var "l0")],L NoLoc $ Ext (LetLocE (Var "jump1") (AfterConstantLE 1 (Var "l0")) (L NoLoc $ LetE (Var "v",[],IntTy,L NoLoc $ PrimAppE L1.AddP [L NoLoc $ VarE (Var "n"),L NoLoc $ LitE 1]) (L NoLoc $ LetE (Var "lf",[],PackedTy "Tree" (Var "lout"),L NoLoc $ DataConE (Var "lout") "Leaf" [L NoLoc $ VarE (Var "v")]) (L NoLoc $ Ext (RetE [Var "jump1"] (Var "lf"))))))),("Node",[(Var "x",Var "l1"),(Var "y",Var "l2")],L NoLoc $ Ext (LetLocE (Var "lout1") (AfterConstantLE 1 (Var "lout")) (L NoLoc $ LetE (Var "x1",[Var "endof2"],PackedTy "Tree" (Var "lout1"),L NoLoc $ AppE (Var "add1") [Var "l1",Var "lout1"] (L NoLoc $ VarE (Var "x"))) (L NoLoc $ Ext (LetLocE (Var "l2") (FromEndLE (Var "endof2")) (L NoLoc $ Ext (LetLocE (Var "lout2") (AfterVariableLE (Var "x1") (Var "lout1")) (L NoLoc $ LetE (Var "y1",[Var "endof3"],PackedTy "Tree" (Var "lout2"),L NoLoc $ AppE (Var "add1") [Var "l2",Var "lout2"] (L NoLoc $ VarE (Var "y"))) (L NoLoc $ LetE (Var "z",[],PackedTy "Tree" (Var "lout"),L NoLoc $ DataConE (Var "lout") "Node" [L NoLoc $ VarE (Var "x1"),L NoLoc $ VarE (Var "y1")]) (L NoLoc $ Ext (RetE [Var "endof3"] (Var "z"))))))))))))]})], mainExp = Just (L NoLoc $ Ext (LetRegionE (VarR (Var "r")) (L NoLoc $ Ext (LetLocE (Var "ltest") (StartOfLE (VarR (Var "r"))) (L NoLoc $ Ext (LetLocE (Var "ltest1") (AfterConstantLE 1 (Var "ltest")) (L NoLoc $ LetE (Var "x",[],PackedTy "Tree" (Var "ltest1"),L NoLoc $ DataConE (Var "ltest1") "Leaf" [L NoLoc $ LitE 1]) (L NoLoc $ Ext (LetLocE (Var "ltest2") (AfterVariableLE (Var "x") (Var "ltest1")) (L NoLoc $ LetE (Var "y",[],PackedTy "Tree" (Var "ltest2"),L NoLoc $ DataConE (Var "ltest2") "Leaf" [L NoLoc $ LitE 2]) (L NoLoc $ LetE (Var "z",[],PackedTy "Tree" (Var "ltest"),L NoLoc $ DataConE (Var "ltest") "Node" [L NoLoc $ VarE (Var "x"),L NoLoc $ VarE (Var "y")]) (L NoLoc $ Ext (LetRegionE (VarR (Var "o")) (L NoLoc $ Ext (LetLocE (Var "lo") (StartOfLE (VarR (Var "o"))) (L NoLoc $ LetE (Var "tailapp4",[Var "endof5"],PackedTy "Tree" (Var "lout"),L NoLoc $ AppE (Var "add1") [Var "l",Var "lo"] (L NoLoc $ VarE (Var "z"))) (L NoLoc $ Ext (RetE [] (Var "tailapp4")))))))))))))))))),IntTy)}

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
