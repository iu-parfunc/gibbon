{-# LANGUAGE OverloadedStrings #-}

-- | Mock definitions and other utilities
module Common where

import Data.Loc
import Data.Set as S

import Packed.FirstOrder.Common
import Packed.FirstOrder.L2.Syntax as L2
-- import Packed.FirstOrder.L2.Typecheck
import qualified Packed.FirstOrder.L1.Syntax as L1


ddtree :: DDefs Ty2
ddtree = fromListDD [DDef (toVar "Tree")
                      [ ("Leaf",[(False,IntTy)])
                      , ("Node",[ (False,PackedTy "Tree" "l")
                                , (False,PackedTy "Tree" "l")])]]


add1Fun :: L2.FunDef Ty2 (L Exp2)
add1Fun = L2.FunDef "add1" "tr" add1FunTy add1FunBod
  where
    add1FunTy :: ArrowTy Ty2
    add1FunTy = (ArrowTy
                  (PackedTy "Tree" "lin")
                  (PackedTy "Tree" "lout")
                  [LRM "lin" (VarR "r1") Input, LRM "lout" (VarR "r1") Output]
                  (S.fromList [Traverse "lin"])
                  [EndOf $ LRM "lin" (VarR "r1") Input])

    add1FunBod :: L Exp2
    add1FunBod =
      L NoLoc $ CaseE (L NoLoc $ VarE "tr") $
      [ ("Leaf", [("n","l0")],
          L NoLoc $ LetE ("v",[],IntTy, L NoLoc $ PrimAppE L1.AddP
                                                 [L NoLoc $ VarE "n",
                                                  L NoLoc $ LitE 1]) $
          L NoLoc $ LetE ("lf",[],PackedTy "Tree" "lout",
                          L NoLoc $ DataConE "lout" "Leaf" [L NoLoc $ VarE "v"]) $
          L NoLoc $ VarE "lf")
      , ("Node", [("x","l1"),("y","l2")],
         L NoLoc $ Ext $ LetLocE "lout1" (AfterConstantLE 1 "lout") $
         L NoLoc $ LetE ("x1",[],PackedTy "Tree" "lout1",
                         L NoLoc $ AppE "add1" ["l1","lout1"]
                                 (L NoLoc $ VarE "x")) $
         L NoLoc $ Ext $ LetLocE "lout2" (AfterVariableLE "x1" "lout1") $
         L NoLoc $ LetE ("y1",[],PackedTy "Tree" "lout2", L NoLoc $ AppE "add1" ["l2","lout2"] (L NoLoc $ VarE "y")) $
         L NoLoc $ LetE ("z",[],PackedTy "Tree" "lout",
                  L NoLoc $ DataConE "lout" "Node" [ L NoLoc $ VarE "x1" , L NoLoc $ VarE "y1"]) $
         L NoLoc $ VarE "z")
      ]
