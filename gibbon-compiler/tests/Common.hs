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

--------------------------------------------------------------------------------
-- Add1

add1TraversedFun :: L2.FunDef
add1TraversedFun = L2.FunDef "add1" add1TraversedFunTy "tr1" add1FunBod
  where add1TraversedFunTy = add1FunTy { arrEffs = S.fromList [Traverse "lin2"] }


add1Fun :: L2.FunDef
add1Fun = L2.FunDef "add1" add1FunTy "tr1" add1FunBod


add1FunTy :: ArrowTy Ty2
add1FunTy = (ArrowTy
             [LRM "lin2" (VarR "r3") Input, LRM "lout4" (VarR "r3") Output]
             (PackedTy "Tree" "lin2")
             (S.empty)
             (PackedTy "Tree" "lout4")
             [EndOf $ LRM "lin2" (VarR "r3") Input])


add1FunBod :: L Exp2
add1FunBod = l$ CaseE (l$ VarE "tr1") $
  [ ("Leaf", [("n5","l6")],
      l$ LetE ("v7",[],IntTy,
               l$ PrimAppE L1.AddP [l$ VarE "n5", l$ LitE 1]) $
      l$ LetE ("lf8",[],PackedTy "Tree" "lout4",
               l$ DataConE "lout4" "Leaf" [l$ VarE "v7"]) $
      l$ VarE "lf8")

  , ("Node", [("x9","l10"),("y11","l12")],
     l$ Ext $ LetLocE "l13" (AfterConstantLE 1 "lout4") $
     l$ LetE ("x14",[],PackedTy "Tree" "l13",
               l$ AppE "add1" ["l10","l13"] (l$ VarE "x9")) $
     l$ Ext $ LetLocE "l15" (AfterVariableLE "x14" "l13") $
     l$ LetE ("y16",[],PackedTy "Tree" "l15", l$ AppE "add1" ["l12","l15"] (l$ VarE "y11")) $
     l$ LetE ("z17",[],PackedTy "Tree" "lout4",
              l$ DataConE "lout4" "Node" [ l$ VarE "x14" , l$ VarE "y16"]) $
     l$ VarE "z17")
  ]

--------------------------------------------------------------------------------
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
id1 = L2.FunDef "id1" idFunTy "tr18" (l$ VarE "tr18")
  where
    idFunTy :: ArrowTy Ty2
    idFunTy = (ArrowTy
               [LRM "lin19" (VarR "r20") Input, LRM "lout21" (VarR "r20") Output]
               (PackedTy "Tree" "lin19")
               (S.empty)
               (PackedTy "Tree" "lout21")
               [EndOf $ LRM "lin19" (VarR "r20") Input])


--------------------------------------------------------------------------------

copyTree :: L2.FunDef
copyTree = L2.FunDef "copyTree" copyFunTy "tr22" copyBod
  where
    copyFunTy = (ArrowTy
                 [LRM "lin23" (VarR "r24") Input, LRM "lout25" (VarR "r24") Output]
                 (PackedTy "Tree" "lin23")
                 (S.singleton (Traverse "lin23"))
                 (PackedTy "Tree" "lout25")
                 [EndOf $ LRM "lin23" (VarR "r24") Input])

    copyBod = l$ CaseE (l$ VarE "tr22") $
                 [ ("Leaf", [("n27","lin26")],
                     l$ LetE ("n28",[],PackedTy "Tree" "lout25",
                               l$ DataConE "lout25" "Leaf" [l$ VarE "n27"]) $
                     l$ VarE "n28")

                 , ("Node", [("x29","lx30"),("y31","ly32")],
                    l$ Ext  $ LetLocE "lx33" (AfterConstantLE 1 "lout25") $
                    l$ LetE ("x34", [], PackedTy "Tree" "lx33",
                             l$ AppE "copyTree" ["lx30","lx33"] (l$ VarE "x29")) $
                    l$ Ext  $ LetLocE "ly35" (AfterVariableLE "x34" "lx33") $
                    l$ LetE ("y36", [], PackedTy "Tree" "ly35",
                            l$ AppE "copyTree" ["ly32","ly35"] (l$ VarE "y31")) $
                    l$ DataConE "lout25" "Node" [l$ VarE "x34", l$ VarE "y36"])
                 ]
