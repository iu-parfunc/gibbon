{-# LANGUAGE OverloadedStrings #-}

module Packed.FirstOrder.L2.Examples
  ( -- * Data definitions
    ddtree
    -- * Functions
  , add1Fun, add1TraversedFun, id1Fun, copyTreeFun, id2Fun

    -- * Programs
  , add1Prog, id1Prog, copyTreeProg, id2Prog, copyOnId1Prog
  ) where

import Data.Loc
import Data.Set as S
import Data.Map as M
import Text.PrettyPrint.GenericPretty

import Packed.FirstOrder.Common hiding (FunDef)
import Packed.FirstOrder.L2.Syntax
import Packed.FirstOrder.L1.Syntax hiding (Prog, FunDef, ddefs, fundefs, mainExp, add1Prog)


ddtree :: DDefs Ty2
ddtree = fromListDD [DDef (toVar "Tree")
                      [ ("Leaf",[(False,IntTy)])
                      , ("Node",[ (False,PackedTy "Tree" "l")
                                , (False,PackedTy "Tree" "l")])]]

--------------------------------------------------------------------------------
-- Add1

add1TraversedFun :: FunDef
add1TraversedFun = FunDef "add1" add1TraversedFunTy "tr1" add1FunBod
  where add1TraversedFunTy = add1FunTy { arrEffs = S.fromList [Traverse "lin2"] }


add1Fun :: FunDef
add1Fun = FunDef "add1" add1FunTy "tr1" add1FunBod


add1FunTy :: ArrowTy Ty2
add1FunTy = (ArrowTy
             [LRM "lin2" (VarR "r3") Input, LRM "lout4" (VarR "r3") Output]
             (PackedTy "Tree" "lin2")
             (S.empty)
             (PackedTy "Tree" "lout4")
             [])


add1FunBod :: L Exp2
add1FunBod = l$ CaseE (l$ VarE "tr1") $
  [ ("Leaf", [("n5","l6")],
      l$ LetE ("v7",[],IntTy,
               l$ PrimAppE AddP [l$ VarE "n5", l$ LitE 1]) $
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

add1Prog :: Prog
add1Prog = Prog ddtree (M.fromList [("add1", add1Fun)]) Nothing

--------------------------------------------------------------------------------

id1Fun :: FunDef
id1Fun = FunDef "id1" idFunTy "tr18" idFunBod
  where
    idFunBod = (l$ VarE "tr18")

    idFunTy :: ArrowTy Ty2
    idFunTy = (ArrowTy
               [LRM "lin19" (VarR "r20") Input, LRM "lout21" (VarR "r20") Output]
               (PackedTy "Tree" "lin19")
               (S.empty)
               (PackedTy "Tree" "lout21")
               [])


id1Prog :: Prog
id1Prog = Prog ddtree (M.fromList [("id1", id1Fun)]) Nothing

--------------------------------------------------------------------------------

copyTreeFun :: FunDef
copyTreeFun = FunDef "copyTree" copyFunTy "tr22" copyBod
  where
    copyFunTy = (ArrowTy
                 [LRM "lin23" (VarR "r24") Input, LRM "lout25" (VarR "r24") Output]
                 (PackedTy "Tree" "lin23")
                 S.empty
                 (PackedTy "Tree" "lout25")
                 [])

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

copyTreeProg :: Prog
copyTreeProg = Prog ddtree (M.fromList [("copyTree", copyTreeFun)]) Nothing

--------------------------------------------------------------------------------

id2Fun :: FunDef
id2Fun = FunDef "id2" id2Ty "tr41" id2Bod
  where
    id2Ty :: ArrowTy Ty2
    id2Ty = (ArrowTy
             [LRM "lin37" (VarR "r38") Input, LRM "lout39" (VarR "r38") Output]
             (PackedTy "Tree" "lin37")
             (S.empty)
             (PackedTy "Tree" "lout39")
             [])

    id2Bod = l$ IfE (l$ PrimAppE EqIntP [l$ LitE 20, l$ LitE 20])
             (l$ (VarE "tr41"))
             (l$ (VarE "tr41"))

id2Prog :: Prog
id2Prog = Prog ddtree (M.fromList [("id2", id2Fun)]) Nothing

--------------------------------------------------------------------------------

copyOnId1Prog :: Prog
copyOnId1Prog = Prog ddtree funs Nothing
  where
    funs  = (M.fromList [("copyTree" , copyTreeFun),
                         ("id1WithCopy", id1WithCopyFun)])

id1WithCopyFun :: FunDef
id1WithCopyFun = id1Fun { funbod = l$ AppE "copyTree" ["lin19","lout21"]
                                   (l$ VarE "tr18")
                        , funname = "id1WithCopy"
                        }
