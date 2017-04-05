{-# LANGUAGE TemplateHaskell #-}

-- |

module Main where

import Control.Exception (bracket, bracket_)
import Data.Map as M
import Data.Set as S
import Data.Word (Word8)
import System.Directory (removeFile)
import System.IO
import System.Info
import System.Process (readCreateProcess, shell)

import Test.Tasty.HUnit
import Test.Tasty.TH

import Packed.FirstOrder.Common hiding (FunDef)
import qualified Packed.FirstOrder.Common as C
-- import Packed.FirstOrder.L1_Source (Exp (..))
import qualified Packed.FirstOrder.L1_Source as L1
import           Packed.FirstOrder.L2_Traverse as L2
import           Packed.FirstOrder.Passes.InferEffects
import           Packed.FirstOrder.Passes.Cursorize
import           Packed.FirstOrder.L3_Target hiding (Prog (..), Ty (..))
import qualified Packed.FirstOrder.L3_Target as T
import qualified Packed.FirstOrder.TargetInterp as TI
import           Packed.FirstOrder.Passes.Codegen

main :: IO ()
main = $(defaultMainGenerator)

-- Unit test the L2_Traverse.hs functions:
--------------------------------------------------------------------------------

t0 :: Set Effect -> Set Effect
t0 eff = arrEffs $ fst $ runSyM 0 $
     inferFunDef (M.empty,
                   M.singleton (toVar "foo") (ArrowTy (PackedTy "K" (toVar "p"))
                                              eff
                                              (PackedTy "K" (toVar "p"))))
                  (C.FunDef (toVar "foo") (toVar "x", L1.Packed "K") (L1.Packed "K")
                   (L1.AppE (toVar "foo") (L1.VarE (toVar "x"))))

case_t0 :: Assertion
case_t0 = assertEqual "infinite loop traverses anything"
                     (S.fromList [Traverse (toVar "p")]) (t0 (S.singleton (Traverse (toVar "p"))))

case_t0b :: Assertion
case_t0b = assertEqual "infinite loop cannot bootstrap with bad initial effect set"
                     S.empty (t0 S.empty)


-- The function foo below should traverse "a" but does not have any
-- output locations.
t1 :: (Set Effect)
t1 = arrEffs $ fst $ runSyM 0 $
     inferFunDef (M.empty,
                   M.fromList
                   [(toVar "copy",(ArrowTy (PackedTy "K" (toVar "p"))
                                                   (S.fromList [Traverse (toVar "p"), Traverse (toVar "o")])
                                              (PackedTy "K" (toVar "o"))))
                   ,(toVar "foo", ArrowTy (PackedTy "K" (toVar "a")) S.empty IntTy)])
                  (C.FunDef (toVar "foo") ((toVar "x"), L1.Packed "K") L1.IntTy $
                     L1.LetE (toVar "ignr",L1.Packed "K", (L1.AppE (toVar "copy") (L1.VarE (toVar "x")))) $
                     L1.LitE 33
                  )

case_t1 :: Assertion
case_t1 = assertEqual "traverse input via another call"
          (S.fromList [Traverse (toVar "a")]) t1

type FunEnv = M.Map Var (L2.ArrowTy Ty)

t2env :: (DDefs a, FunEnv)
t2env = ( fromListDD [DDef (toVar "Bool") [("True",[]), ("False",[])]]
                  , M.fromList [((toVar "foo"), ArrowTy (PackedTy "Bool" (toVar "p")) S.empty IntTy)])
fooBoolInt :: a -> L1.FunDef L1.Ty a
fooBoolInt = C.FunDef (toVar "foo") ((toVar "x"), L1.Packed "Bool") L1.IntTy

t2 :: (Set Effect)
t2 = arrEffs $ fst $ runSyM 0 $
     inferFunDef t2env
                  (fooBoolInt $
                    L1.CaseE (VarE (toVar "x")) $
                      [ ("True",[],LitE 3)
                      , ("False",[],LitE 3) ])

case_t2 :: Assertion
case_t2 = assertEqual "Traverse a Bool with case"
            (S.fromList [Traverse (toVar "p")]) t2

t2b :: (Set Effect)
t2b = arrEffs $ fst $ runSyM 0 $
     inferFunDef t2env (fooBoolInt $ LitE 33)

case_t2b :: Assertion
case_t2b = assertEqual "No traverse from a lit" S.empty t2b

t2c :: (Set Effect)
t2c = arrEffs $ fst $ runSyM 0 $
     inferFunDef t2env (fooBoolInt $ VarE (toVar "x"))

case_t2c :: Assertion
case_t2c = assertEqual "No traverse from identity function" S.empty t2b


t3 :: Exp -> Set Effect
t3 bod = arrEffs $ fst $ runSyM 0 $
     inferFunDef ( fromListDD [DDef (toVar "SillyTree")
                                  [ ("Leaf",[])
                                  , ("Node",[L1.Packed "SillyTree", L1.IntTy])]]
                  , M.fromList [((toVar "foo"), ArrowTy (PackedTy "SillyTree" (toVar "p")) S.empty IntTy)])
                  (C.FunDef (toVar "foo") ((toVar "x"), L1.Packed "SillyTree") L1.IntTy
                    bod)

case_t3a :: Assertion
case_t3a = assertEqual "sillytree1" S.empty (t3 (LitE 33))

case_t3b :: Assertion
case_t3b = assertEqual "sillytree2" S.empty $ t3 $ VarE (toVar "x")


case_t3c :: Assertion
case_t3c = assertEqual "sillytree3: reference rightmost"
           (S.singleton (Traverse (toVar "p"))) $ t3 $
           L1.CaseE (VarE (toVar "x"))
            [ ("Leaf", [],     LitE 3)
            , ("Node", [(toVar "l"),(toVar "r")], VarE (toVar "r"))
            ]

case_t3d :: Assertion
case_t3d = assertEqual "sillytree3: reference leftmost"
           S.empty $ t3 $
           L1.CaseE (VarE (toVar "x"))
            [ ("Leaf", [],     LitE 3)
            , ("Node", [toVar "l",toVar "r"], VarE (toVar "l"))]

t4 :: Exp -> Set Effect
t4 bod = arrEffs $ fst $ runSyM 0 $
     inferFunDef t4env
                  (C.FunDef (toVar "foo") ((toVar "x"), L1.Packed "Tree") L1.IntTy
                    bod)

t4env :: (DDefs L1.Ty, FunEnv)
t4env = ( fromListDD [DDef (toVar "Tree")
                      [ ("Leaf",[L1.IntTy])
                      , ("Node",[L1.Packed "Tree", L1.Packed "Tree"])]]
        , M.fromList [((toVar "foo"), ArrowTy (PackedTy "Tree" (toVar "p"))
                       (S.singleton (Traverse (toVar "p")))
                       IntTy)])

case_t4a :: Assertion
case_t4a = assertEqual "bintree1" S.empty (t4 (LitE 33))

case_t4b :: Assertion
case_t4b = assertEqual "bintree2: matching is not enough for traversal"
           S.empty $ t4 $
           L1.CaseE (VarE (toVar "x"))
            [ ("Leaf", [toVar "n"],     LitE 3)
            , ("Node", [toVar "l",toVar "r"], LitE 4)]

case_t4c :: Assertion
case_t4c = assertEqual "bintree2: referencing is not enough for traversal"
           S.empty $ t4 $
           L1.CaseE (VarE (toVar "x"))
            [ ("Leaf", [toVar "n"],     LitE 3)
            , ("Node", [toVar "l",toVar "r"], VarE (toVar "r"))]

case_t4d :: Assertion
case_t4d = assertEqual "bintree2: recurring left is not enough"
           S.empty $ t4 $
           L1.CaseE (VarE (toVar "x"))
            [ ("Leaf", [toVar "n"],     LitE 3)
            , ("Node", [toVar "l",toVar "r"], AppE (toVar "foo") (VarE (toVar "l")))]

case_t4e :: Assertion
case_t4e = assertEqual "bintree2: recurring on the right IS enough"
           (S.singleton (Traverse (toVar "p"))) $ t4 $
           trav_right_bod

trav_right_bod :: Exp
trav_right_bod = L1.CaseE (VarE (toVar "x"))
                 [ ("Leaf", [toVar "n"],     LitE 3)
                 , ("Node", [toVar "l",toVar "r"], AppE (toVar "foo") (VarE (toVar "r")))]
         -- ^ NOTE - this should return a location inside the input.  A
         -- sub-region of the region at p.

t4_prog :: L1.Prog
t4_prog = L1.Prog (fst t4env)
          (fromListFD [C.FunDef (toVar "foo") ((toVar "x"), L1.Packed "Tree") L1.IntTy
                       trav_right_bod])
          Nothing

t4p :: Prog
t4p = fst $ runSyM 0 $ inferEffects t4_prog

case_t4p :: Assertion
case_t4p =
    assertEqual "Infer the effects for an entire tree-traversal prog:"
      (S.singleton (Traverse (toVar "a")))
      (let FunDef _ (ArrowTy _ efs _) _ _ = fundefs t4p M.! (toVar "foo")
       in efs)

case_t4p2 :: Assertion
case_t4p2 =
    assertEqual "A program which needs more than one fix-point iteration."
      (S.empty)
      (let prg = fst $ runSyM 0 $ inferEffects
                 (L1.Prog (fst t4env)
                        (fromListFD [C.FunDef (toVar "foo") ((toVar "x"), L1.Packed "Tree") L1.IntTy $
                          L1.CaseE (VarE (toVar "x"))
                            [ ("Leaf", [toVar "n"],     LitE 3)
                            , ("Node", [toVar "l",toVar "r"], AppE (toVar "foo") (VarE (toVar "l")))] ])
                  Nothing)
           FunDef _ (ArrowTy _ efs _) _ _ = fundefs prg M.! (toVar "foo")
       in efs)

----------------------------------------


-- Now the full copy-tree example:
copy :: Prog
copy = fst $ runSyM 0 $ inferEffects
     (L1.Prog (fst t4env)
      (fromListFD [C.FunDef (toVar "copy") ((toVar "x"), L1.Packed "Tree") (L1.Packed "Tree") $
                   L1.CaseE (VarE (toVar "x"))
                      [ ("Leaf", [toVar "n"],     VarE (toVar "n"))
                      , ("Node", [toVar "l",toVar "r"],
                        LetE ((toVar "a"), L1.Packed "Tree", AppE (toVar "copy") (VarE (toVar "l"))) $
                        LetE ((toVar "b"), L1.Packed "Tree", AppE (toVar "copy") (VarE (toVar "r"))) $
                        MkPackedE "Node" [VarE (toVar "a"), VarE (toVar "b")]
                        )] ])
      Nothing)

case_copy :: Assertion
case_copy =
     assertEqual "A program which needs more than one fix-point iteration."
      (S.singleton (Traverse (toVar "a")))
      (let prg = copy
           FunDef _ (ArrowTy _ efs _) _ _ = fundefs prg M.! (toVar "copy")
       in efs)

-- t5 :: Prog
-- t5 = fst $ runSyM 1000 $
--      cursorize copy

--------------------------------------------------------------------------------
-- add1 example encoded as AST by hand

add1_prog :: T.Prog
add1_prog = T.Prog [build_tree, add1]
            (Just $ PrintExp $
             LetPrimCallT [(toVar "buf", T.CursorTy)] T.NewBuf [] $
             LetPrimCallT [(toVar "buf2", T.CursorTy)] T.NewBuf [] $
             LetCallT [(toVar "tr", T.PtrTy)] (toVar "build_tree") [IntTriv 10, VarTriv (toVar "buf")] $
             LetCallT [(toVar "ignored1", T.CursorTy), (toVar "ignored2", T.CursorTy)] (toVar "add1")  [VarTriv (toVar "tr"), VarTriv (toVar "buf2")] $
             (RetValsT [])
            )
  where
    build_tree = FunDecl (toVar "build_tree") [(toVar "n",T.IntTy),(toVar "tout",T.CursorTy)] T.CursorTy buildTree_tail
    add1 = FunDecl (toVar "add1") [((toVar "t"),T.CursorTy),(toVar "tout",T.CursorTy)] (T.ProdTy [T.CursorTy,T.CursorTy]) add1_tail

    buildTree_tail =
        Switch (VarTriv (toVar "n")) (IntAlts [(0, base_case)]) (Just recursive_case)
      where
        base_case, recursive_case :: Tail

        base_case =
          LetPrimCallT [(toVar "tout1", T.CursorTy)] T.WriteInt [IntTriv 0, VarTriv (toVar "tout")] $
          RetValsT [VarTriv (toVar "tout1")]

        recursive_case =
          LetPrimCallT [(toVar "n1",T.IntTy)] SubP [VarTriv (toVar "n"), IntTriv 1] $
          LetPrimCallT [(toVar "tout1",T.CursorTy)] WriteTag [TagTriv 1, VarTriv (toVar "tout")] $
          LetCallT [(toVar "tout2",T.CursorTy)] (toVar "build_tree") [VarTriv (toVar "n1"), VarTriv (toVar "tout1")] $
          LetCallT [(toVar "tout3",T.CursorTy)] (toVar "build_tree") [VarTriv (toVar "n1"), VarTriv (toVar "tout2")] $
          RetValsT [VarTriv (toVar "tout3")]

    add1_tail =
        LetPrimCallT [(toVar "ttag",T.TagTyPacked),((toVar "t2"),T.CursorTy)] ReadTag [VarTriv (toVar "t")] $
        Switch (VarTriv (toVar "ttag"))
               (TagAlts [(leafTag,leafCase),
                         (nodeTag,nodeCase)])
               Nothing
      where
        leafCase =
          LetPrimCallT [(toVar "tout2",T.CursorTy)] WriteTag [TagTriv leafTag, VarTriv (toVar "tout")] $
          LetPrimCallT [(toVar "n",T.IntTy),((toVar "t3"),T.CursorTy)] T.ReadInt [VarTriv (toVar "t2")] $
          LetPrimCallT [(toVar "n1",T.IntTy)] AddP [VarTriv (toVar "n"), IntTriv 1] $
          LetPrimCallT [(toVar "tout3",T.CursorTy)] T.WriteInt [VarTriv (toVar "n1"), VarTriv (toVar "tout2")] $
          RetValsT [VarTriv (toVar "t3"), VarTriv (toVar "tout3")]

        nodeCase =
          LetPrimCallT [(toVar "tout2",T.CursorTy)] WriteTag [TagTriv nodeTag, VarTriv (toVar "tout")] $
          LetCallT [(toVar "t3",T.CursorTy),(toVar "tout3",T.CursorTy)] (toVar "add1") [VarTriv (toVar "t2"), VarTriv (toVar "tout2")] $
          TailCall (toVar "add1") [VarTriv (toVar "t3"), VarTriv (toVar "tout3")]

        leafTag, nodeTag :: Word8
        leafTag = 0
        nodeTag = 1

-- [2017.01.11] FIXME: I think there's something wrong with the above
-- program.  It doesn't pass in interpreter or compiler.  Disabling
-- these two tests until there is time to debug further.

_case_interp_add1 :: Assertion
_case_interp_add1 =
    do [_val] <- TI.execProg add1_prog
       -- FIXME: assert correct val.
       return ()

_case_add1 :: Assertion
_case_add1 =
    bracket (openFile file WriteMode)
            (\h -> hClose h
             -- >> removeFile file -- Leave around for debugging [2017.01.11].
            )
            runTest
  where
    file = "add1_out.c"

    runTest :: Handle -> Assertion
    runTest h = do
      str <- codegenProg True add1_prog
      hPutStr h str
      hFlush h
      gcc_out <- readCreateProcess (shell ("gcc -std=gnu11 -o add1.exe " ++ file)) ""
      assertEqual "unexpected gcc output" "" gcc_out

      let valgrind = case os of
                       "linux" -> "valgrind -q --error-exitcode=99 "
                       _       -> "" -- Don't assume valgrind on macos/etc

      -- just test for return value 0
      _proc_out <-
        bracket_ (return ())
                 (return ()) -- (removeFile "add1.exe") -- Leave around for debugging [2017.01.11].
                 (readCreateProcess (shell (valgrind++"./add1.exe 10 10")) "")

      return ()
