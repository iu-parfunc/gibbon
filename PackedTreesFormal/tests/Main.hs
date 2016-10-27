{-# LANGUAGE TemplateHaskell #-}

-- |

module Main where

import Test.Tasty.HUnit
import Test.Tasty.TH
import qualified Packed.FirstOrder.Common as C
import Packed.FirstOrder.Common hiding (FunDef)
import qualified Packed.FirstOrder.L1_Source as L1
import Packed.FirstOrder.L1_Source (Exp(..))
import Packed.FirstOrder.LTraverse
import Data.Set as S
import Data.Map as M
    
main :: IO ()
main = $(defaultMainGenerator)
    
-- Unit test the LTraverse.hs functions:
--------------------------------------------------------------------------------

t0 :: Set Effect -> Set Effect
t0 eff = fst $ runSyM 0 $
     inferEffects (M.empty,
                   M.singleton "foo" (ArrowTy (PackedTy "K" "p")
                                              eff
                                              (PackedTy "K" "p")))
                  (C.FunDef "foo" ("x", L1.Packed "K") (L1.Packed "K")
                        (L1.AppE "foo" (L1.VarE "x")))

case_t0 :: Assertion
case_t0 = assertEqual "infinite loop traverses anything"
                     (S.fromList [Traverse "p"]) (t0 (S.singleton (Traverse "p")))

case_t0b :: Assertion
case_t0b = assertEqual "infinite loop cannot bootstrap with bad initial effect set"
                     S.empty (t0 S.empty)

                                                 
-- The function foo below should traverse "a" but does not have any
-- output locations.
t1 :: (Set Effect)
t1 = fst $ runSyM 0 $
     inferEffects (M.empty,
                   M.fromList 
                   [("copy",(ArrowTy (PackedTy "K" "p")
                                                   (S.fromList [Traverse "p", Traverse "o"])
                                              (PackedTy "K" "o")))
                   ,("foo", ArrowTy (PackedTy "K" "a") S.empty IntTy)])
                  (C.FunDef "foo" ("x", L1.Packed "K") L1.IntTy $
                     L1.LetE ("ignr",L1.Packed "K", (L1.AppE "copy" (L1.VarE "x"))) $
                       L1.LitE 33
                  )

case_t1 :: Assertion
case_t1 = assertEqual "traverse input via another call"
          (S.fromList [Traverse "a"]) t1 

t2env :: (DDefs a, FunEnv)
t2env = ( fromListDD [DDef "Bool" [("True",[]), ("False",[])]]
                  , M.fromList [("foo", ArrowTy (PackedTy "Bool" "p") S.empty IntTy)])
fooBoolInt :: a -> L1.FunDef L1.Ty a
fooBoolInt = C.FunDef "foo" ("x", L1.Packed "Bool") L1.IntTy
        
t2 :: (Set Effect)
t2 = fst $ runSyM 0 $
     inferEffects t2env
                  (fooBoolInt $
                    L1.CaseE (VarE "x") $ M.fromList 
                      [ ("True", ([],LitE 3))
                      , ("False", ([],LitE 3)) ])
     
case_t2 :: Assertion
case_t2 = assertEqual "Traverse a Bool with case"
            (S.fromList [Traverse "p"]) t2
           
t2b :: (Set Effect)
t2b = fst $ runSyM 0 $
     inferEffects t2env (fooBoolInt $ LitE 33)

case_t2b :: Assertion
case_t2b = assertEqual "No traverse from a lit" S.empty t2b
                  
t2c :: (Set Effect)
t2c = fst $ runSyM 0 $
     inferEffects t2env (fooBoolInt $ VarE "x")

case_t2c :: Assertion
case_t2c = assertEqual "No traverse from identity function" S.empty t2b


t3 :: Exp -> Set Effect
t3 bod = fst $ runSyM 0 $
     inferEffects ( fromListDD [DDef "SillyTree"
                                  [ ("Leaf",[])
                                  , ("Node",[L1.Packed "SillyTree", L1.IntTy])]]
                  , M.fromList [("foo", ArrowTy (PackedTy "SillyTree" "p") S.empty IntTy)])
                  (C.FunDef "foo" ("x", L1.Packed "SillyTree") L1.IntTy 
                    bod)

case_t3a :: Assertion
case_t3a = assertEqual "sillytree1" S.empty (t3 (LitE 33))

case_t3b :: Assertion
case_t3b = assertEqual "sillytree2" S.empty $ t3 $ VarE "x"


case_t3c :: Assertion
case_t3c = assertEqual "sillytree3: reference rightmost"
           (S.singleton (Traverse "p")) $ t3 $
           L1.CaseE (VarE "x") $ M.fromList 
            [ ("Leaf", ([],     LitE 3))
            , ("Node", (["l","r"], VarE "r"))
            ]

case_t3d :: Assertion
case_t3d = assertEqual "sillytree3: reference leftmost"
           S.empty $ t3 $
           L1.CaseE (VarE "x") $ M.fromList 
            [ ("Leaf", ([],     LitE 3))
            , ("Node", (["l","r"], VarE "l"))]

t4 :: Exp -> Set Effect
t4 bod = fst $ runSyM 0 $
     inferEffects t4env
                  (C.FunDef "foo" ("x", L1.Packed "Tree") L1.IntTy 
                    bod)

t4env :: (DDefs L1.Ty, FunEnv)
t4env = ( fromListDD [DDef "Tree"
                      [ ("Leaf",[L1.IntTy])
                      , ("Node",[L1.Packed "Tree", L1.Packed "Tree"])]]
        , M.fromList [("foo", ArrowTy (PackedTy "Tree" "p")
                       (S.singleton (Traverse "p"))
                       IntTy)])
                  
case_t4a :: Assertion
case_t4a = assertEqual "bintree1" S.empty (t4 (LitE 33))

case_t4b :: Assertion
case_t4b = assertEqual "bintree2: matching is not enough for traversal"
           S.empty $ t4 $
           L1.CaseE (VarE "x") $ M.fromList 
            [ ("Leaf", (["n"],     LitE 3))
            , ("Node", (["l","r"], LitE 4))]

case_t4c :: Assertion
case_t4c = assertEqual "bintree2: referencing is not enough for traversal"
           S.empty $ t4 $
           L1.CaseE (VarE "x") $ M.fromList 
            [ ("Leaf", (["n"],     LitE 3))
            , ("Node", (["l","r"], VarE "r"))]

case_t4d :: Assertion
case_t4d = assertEqual "bintree2: recurring left is not enough"
           S.empty $ t4 $
           L1.CaseE (VarE "x") $ M.fromList 
            [ ("Leaf", (["n"],     LitE 3))
            , ("Node", (["l","r"], AppE "foo" (VarE "l")))]

case_t4e :: Assertion
case_t4e = assertEqual "bintree2: recurring on the right IS enough"
           (S.singleton (Traverse "p")) $ t4 $
           trav_right_bod

trav_right_bod :: Exp
trav_right_bod = L1.CaseE (VarE "x") $ M.fromList 
                 [ ("Leaf", (["n"],     LitE 3))
                 , ("Node", (["l","r"], AppE "foo" (VarE "r")))]
         -- ^ NOTE - this should return a location inside the input.  A
         -- sub-region of the region at p.

t4_prog :: L1.Prog
t4_prog = L1.Prog (fst t4env)
          (fromListFD [C.FunDef "foo" ("x", L1.Packed "Tree") L1.IntTy 
                       trav_right_bod])
          Nothing

t4p :: Prog
t4p = inferProg t4_prog

case_t4p :: Assertion
case_t4p =
    assertEqual "Infer the effects for an entire tree-traversal prog:"
      (S.singleton (Traverse "a"))
      (let FunDef _ (ArrowTy _ efs _) _ _ = fundefs t4p M.! "foo"
       in efs)

case_t4p2 :: Assertion
case_t4p2 =
    assertEqual "A program which needs more than one fix-point iteration."
      (S.empty)
      (let prg = inferProg
                 (L1.Prog (fst t4env)
                        (fromListFD [C.FunDef "foo" ("x", L1.Packed "Tree") L1.IntTy $
                          L1.CaseE (VarE "x") $ M.fromList 
                            [ ("Leaf", (["n"],     LitE 3))
                            , ("Node", (["l","r"], AppE "foo" (VarE "l")))] ])
                  Nothing)
           FunDef _ (ArrowTy _ efs _) _ _ = fundefs prg M.! "foo"
       in efs)
      
----------------------------------------


-- Now the full copy-tree example:
copy :: Prog
copy = inferProg
     (L1.Prog (fst t4env)
      (fromListFD [C.FunDef "copy" ("x", L1.Packed "Tree") (L1.Packed "Tree") $
                   L1.CaseE (VarE "x") $ M.fromList 
                      [ ("Leaf", (["n"],     VarE "n"))
                      , ("Node", (["l","r"],
                        LetE ("a", L1.Packed "Tree", AppE "copy" (VarE "l")) $
                        LetE ("b", L1.Packed "Tree", AppE "copy" (VarE "r")) $ 
                        MkPackedE "Node" [VarE "a", VarE "b"]
                        ))] ])
      Nothing)

case_copy :: Assertion
case_copy =      
     assertEqual "A program which needs more than one fix-point iteration."
      (S.singleton (Traverse "a"))
      (let prg = copy
           FunDef _ (ArrowTy _ efs _) _ _ = fundefs prg M.! "copy"
       in efs)
