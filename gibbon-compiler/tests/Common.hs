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

    add1FunBod :: L Exp2
    add1FunBod =
      l$ CaseE (l$ VarE "tr") $
      [ ("Leaf", [("n","l0")],
          l$ LetE ("v",[],IntTy, l$ PrimAppE L1.AddP
                                                 [l$ VarE "n",
                                                  l$ LitE 1]) $
          l$ LetE ("lf",[],PackedTy "Tree" "lout",
                          l$ DataConE "lout" "Leaf" [l$ VarE "v"]) $
          l$ VarE "lf")
      , ("Node", [("x","l1"),("y","l2")],
         l$ Ext $ LetLocE "lout1" (AfterConstantLE 1 "lout") $
         l$ LetE ("x1",[],PackedTy "Tree" "lout1",
                         l$ AppE "add1" ["l1","lout1"]
                                 (l$ VarE "x")) $
         l$ Ext $ LetLocE "lout2" (AfterVariableLE "x1" "lout1") $
         l$ LetE ("y1",[],PackedTy "Tree" "lout2", l$ AppE "add1" ["l2","lout2"] (l$ VarE "y")) $
         l$ LetE ("z",[],PackedTy "Tree" "lout",
                  l$ DataConE "lout" "Node" [ l$ VarE "x1" , l$ VarE "y1"]) $
         l$ VarE "z")
      ]
