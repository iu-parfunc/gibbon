{-# LANGUAGE OverloadedStrings #-}

module Packed.FirstOrder.L1.Examples where

import Data.Loc
import Data.Map as M

import Packed.FirstOrder.L1.Syntax
import Packed.FirstOrder.Common

treeTy :: Ty1
treeTy = PackedTy "Tree" ()

treeDD :: DDefs (UrTy ())
treeDD = (fromListDD [DDef "Tree"
                      [ ("Leaf",[(False,IntTy)])
                      , ("Node",[(False,PackedTy "Tree" ())
                                ,(False,PackedTy "Tree" ())])]])

mkAdd1Prog :: L Exp1 -> Maybe (L Exp1) -> Prog
mkAdd1Prog bod mainExp = Prog treeDD
                              (M.fromList [("add1",mkAdd1Fun bod)])
                              mainExp

mkAdd1Fun :: ex -> FunDef Ty1 ex
mkAdd1Fun bod = FunDef "add1" ("tr",treeTy) treeTy bod


--------------------------------------------------------------------------------


-- | The basic form of the add1 program where recursions happen
-- immediately as arguments to the data-constructor.
add1Prog :: Prog
add1Prog = mkAdd1Prog exadd1Bod Nothing

exadd1Bod :: L Exp1
exadd1Bod = l$
    CaseE (l$ VarE "tr") $
      [ ("Leaf", [("n",())],
         l$ PrimAppE AddP [l$ VarE "n", l$ LitE 1])
      , ("Node", [("x",()),("y",())],
         l$ DataConE () "Node"
          [ l$ AppE "add1" [] (l$ VarE "x")
          , l$ AppE "add1" [] (l$ VarE "y")])
      ]

exadd1BodLetLeft :: L Exp1
exadd1BodLetLeft = l$
    CaseE (l$ VarE "tr") $
      [ ("Leaf", [("n",())], l$ PrimAppE AddP [l$ VarE "n", l$ LitE 1])
      , ("Node", [("x",()),("y",())],
         l$ LetE ("x2",[], PackedTy "Tree" (), l$ AppE "add1" [] (l$ VarE "x")) $
         l$ LetE ("y2",[], PackedTy "Tree" (), l$ AppE "add1" [] (l$ VarE "y")) $
         l$ DataConE () "Node"
          [ l$ VarE "x2", l$ VarE "y2"])
      ]

-- | A more challenging case where recursions are performed right-to-left
exadd1BodLetRight :: L Exp1
exadd1BodLetRight = l$
    CaseE (l$ VarE "tr") $
      [ ("Leaf", [("n",())], l$ PrimAppE AddP [l$ VarE "n", l$ LitE 1])
      , ("Node", [("x",()),("y",())],
         l$ LetE ("y2",[], PackedTy "Tree" (), l$ AppE "add1" [] (l$ VarE "y")) $
         l$ LetE ("x2",[], PackedTy "Tree" (), l$ AppE "add1" [] (l$ VarE "x")) $
         l$ DataConE () "Node"
          [ l$ VarE "x2", l$ VarE "y2"])
      ]

-- | Add1 program with let bindings, recurring in left-to-right order.
add1ProgLetLeft :: Prog
add1ProgLetLeft = mkAdd1Prog exadd1BodLetLeft Nothing

-- | Add1 program with let bindings, recurring in right-to-left order.
add1ProgLetRight :: Prog
add1ProgLetRight = mkAdd1Prog exadd1BodLetRight Nothing


-- | An even more challenging case where there is an (apparent) data
-- dependency where x2 depends on y2.
add1ProgChallenge :: Prog
add1ProgChallenge =
    Prog treeDD
         (M.fromList [ ("add1",mkAdd1Fun bod)
                     , ("pred", FunDef "pred" ("tr", treeTy) BoolTy
                        (l$ CaseE (l$ VarE "tr") $
                         [ ("Leaf", [("n",())], l$ PrimAppE MkTrue [])
                         , ("Node", [("x",()),("y",())], l$ PrimAppE MkFalse [])]))])
         Nothing
  where
   bod = l$
    CaseE (l$ VarE "tr") $
      [ ("Leaf", [("n",())], l$ PrimAppE AddP [l$ VarE "n", l$ LitE 1])
      , ("Node", [("x",()),("y",())],
         l$ LetE ("y2",[], PackedTy "Tree" (), l$ AppE "add1" [] (l$ VarE "y")) $
         l$ LetE ("x2",[], PackedTy "Tree" (),
              (l$ IfE (l$ AppE "pred" [] (l$ VarE "y2"))
                   (l$ AppE "add1" [] (l$ VarE "x"))
                   (l$ AppE "add1" [] (l$ VarE "x")))) $
         l$ DataConE () "Node" [ l$ VarE "x2", l$ VarE "y2"])
      ]

-- | This program is a challenge because a packed value flows to two destinations.
add1ProgSharing :: Prog
add1ProgSharing = Prog treeDD (M.fromList [("add1",mkAdd1Fun bod)]) Nothing
  where
   bod = l$
    CaseE (l$ VarE "tr") $
      [ ("Leaf", [("n",())], l$ PrimAppE AddP [l$ VarE "n", l$ LitE 1])
      , ("Node", [("x",()),("y",())],
         l$ LetE ("x2",[], PackedTy "Tree" (), l$ AppE "add1" [] (l$ VarE "x")) $
         l$ DataConE () "Node" [ l$ VarE "x2", l$ VarE "x2"])
      ]

--------------------------------------------------------------------------------
