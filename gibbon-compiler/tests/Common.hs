{-# LANGUAGE OverloadedStrings #-}

-- | Mock definitions and other utilities
module Common where

import Data.Set as S
import Data.Map as M

import Packed.FirstOrder.Common
import Packed.FirstOrder.L2.Syntax as L2
import Packed.FirstOrder.L2.Typecheck
import qualified Packed.FirstOrder.L1.Syntax as L1


ddtree :: DDefs Ty2
ddtree = fromListDD [DDef (toVar "Tree")
                      [ ("Leaf",[(False,IntTy)])
                      , ("Node",[ (False,PackedTy "Tree" "l")
                                , (False,PackedTy "Tree" "l")])]]


add1Fun :: L2.FunDef
add1Fun = L2.FunDef "add1" add1FunTy "tr" add1FunBod
  where
    add1FunTy :: ArrowTy Ty2
    add1FunTy = (ArrowTy
                [LRM "lin" (VarR "r1") Input, LRM "lout" (VarR "r1") Output]
                (PackedTy "Tree" "lin")
                (S.fromList [Traverse "lin"])
                (PackedTy "Tree" "lout")
                [EndOf $ LRM "lin" (VarR "r1") Input])

    add1FunBod :: Exp2
    add1FunBod =
      CaseE (VarE "tr") $
      [ ("Leaf", [("n","l0")],
          LetE ("v",[],IntTy,PrimAppE L1.AddP [VarE "n", LitE 1]) $
          LetE ("lf",[],PackedTy "Tree" "lout", DataConE "lout" "Leaf" [VarE "v"]) $
          VarE "lf")
      , ("Node", [("x","l1"),("y","l2")],
         Ext $ LetLocE "lout1" (AfterConstantLE 1 "lout" "lout1") $
         LetE ("x1",[],PackedTy "Tree" "lout1", AppE "add1" ["l1","lout1"] (VarE "x")) $
         Ext $ LetLocE "lout2" (AfterVariableLE "x1" "lout1" "lout2") $
         LetE ("y1",[],PackedTy "Tree" "lout2", AppE "add1" ["l2","lout2"] (VarE "y")) $
         LetE ("z",[],PackedTy "Tree" "lout",
                  DataConE "lout" "Node" [ VarE "x1" , VarE "y1"]) $
         VarE "z")
      ]
