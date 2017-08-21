{-# LANGUAGE OverloadedStrings #-}

-- | Mock definitions and other utilities
module Common where

import Data.Loc
import Data.Set as S
-- import Text.PrettyPrint.GenericPretty

import Packed.FirstOrder.Common
import Packed.FirstOrder.L2.Syntax as L2
import qualified Packed.FirstOrder.L1.Syntax as L1

ddtree :: DDefs Ty2
ddtree = fromListDD [DDef (toVar "Tree")
                      [ ("Leaf",[(False,IntTy)])
                      , ("Node",[ (False,PackedTy "Tree" "l")
                                , (False,PackedTy "Tree" "l")])]]


add1TraversedFun :: L2.FunDef
add1TraversedFun = L2.FunDef "add1" add1TraversedFunTy "tr" add1FunBod
  where add1TraversedFunTy = add1FunTy { arrEffs = S.fromList [Traverse "lin"] }


add1Fun :: L2.FunDef
add1Fun = L2.FunDef "add1" add1FunTy "tr" add1FunBod

{-
FunDef {funname = "id1",
        funty = ArrowTy {locVars = [LRM "lin" (VarR "r1") Input,
                                    LRM "lout" (VarR "r1") Output],
                         arrIn = PackedTy "Tree" "lin",
                         arrEffs = [],
                         arrOut = PackedTy "Tree" "lout",
                         locRets = [EndOf (LRM "lin" (VarR "r1") Input)]},
        funarg = "tr",
        funbod = VarE "tr"}
-}
id1 :: L2.FunDef
id1 = L2.FunDef "id1" add1FunTy "tr" (l$ VarE "tr")


add1FunTy :: ArrowTy Ty2
add1FunTy = (ArrowTy
             [LRM "lin" (VarR "r1") Input, LRM "lout" (VarR "r1") Output]
             (PackedTy "Tree" "lin")
             (S.empty)
             (PackedTy "Tree" "lout")
             [EndOf $ LRM "lin" (VarR "r1") Input])


add1FunBod :: L Exp2
add1FunBod = l$ CaseE (l$ VarE "tr") $
  [ ("Leaf", [("n","l0")],
      l$ LetE ("v",[],IntTy, l$ PrimAppE L1.AddP
                                             [l$ VarE "n", l$ LitE 1]) $
      l$ LetE ("lf",[],PackedTy "Tree" "lout",
                      l$ DataConE "lout" "Leaf" [l$ VarE "v"]) $
      l$ VarE "lf")

  , ("Node", [("x","lx"),("y","ly")],
     l$ Ext $ LetLocE "lx1" (AfterConstantLE 1 "lout") $
     l$ LetE ("x1",[],PackedTy "Tree" "lx1",
                     l$ AppE "add1" ["lx","lx1"]
                             (l$ VarE "x")) $
     l$ Ext $ LetLocE "ly1" (AfterVariableLE "x1" "lx1") $
     l$ LetE ("y1",[],PackedTy "Tree" "ly1", l$ AppE "add1" ["ly","ly1"] (l$ VarE "y")) $
     l$ LetE ("z",[],PackedTy "Tree" "lout",
              l$ DataConE "lout" "Node" [ l$ VarE "x1" , l$ VarE "y1"]) $
     l$ VarE "z")
  ]
