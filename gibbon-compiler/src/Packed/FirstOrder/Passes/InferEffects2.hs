{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

-- | An intermediate language with an effect system that captures traversals.
--
-- ASSUMES that the flatten pass has run, and thus we have trivial AppE operands.
--

module Packed.FirstOrder.Passes.InferEffects2
    -- ( inferEffects, inferFunDef
    --  -- * For other passes that perform similar location-trackinga
    -- , instantiateApp, freshLoc, freshenArrowSchema, zipLT, zipTL
    -- )
    where
import Control.Monad (when)
import Control.DeepSeq
import Data.Loc
import Data.List as L
import Data.Set as S
import Data.Map as M
import Text.PrettyPrint.GenericPretty
import Debug.Trace

import Packed.FirstOrder.L2.Syntax
import Packed.FirstOrder.Common
import Packed.FirstOrder.L1.Syntax hiding (Prog, FunDef)

--------------------------------------------------------------------------------

-- | Chatter level for this module:
lvl :: Int
lvl = 5

inferEffects :: Prog -> SyM Prog
inferEffects prg = return prg

inferExp :: DDefs Ty2 -> NewFuns -> L Exp2 -> SyM (Set Effect)
inferExp ddefs fenv (L p ex) =
  case ex of
    -- QUESTION: does a variable reference count as traversing to the end?
    -- If so, the identity function has the traverse effect.
    -- I'd prefer that the identity function get type (Tree_p -{}-> Tree_p).
    VarE _    -> return S.empty
    LitE _    -> return S.empty
    LitSymE _ -> return S.empty

    -- AppE

    -- If rands are already trivial, no traversal effects can occur here.
    PrimAppE p es -> assertTrivs es $ return S.empty

    -- LetE
    -- IfE
    -- MkProdE
    -- ProjE

    oth -> error $ "FINISHME: inferExp" ++ sdoc oth


{-

add1 fn should have an effect; [Traverse "lin"]

Prog {ddefs = [("Tree",
                DDef {tyName = "Tree",
                      dataCons = [("Leaf", [(False, IntTy)]),
                                  ("Node",
                                   [(False, PackedTy "Tree" "l"),(False, PackedTy "Tree" "l")])]})],
      fundefs = [("add1",
                  FunDef {funname = "add1",
                          funty = ArrowTy {locVars = [LRM "lin" (VarR "r1") Input,
                                                      LRM "lout" (VarR "r1") Output],
                                           arrIn = PackedTy "tree" "lin",
                                           arrEffs = [],
                                           arrOut = PackedTy "tree" "lout",
                                           locRets = [EndOf (LRM "lin" (VarR "r1") Input)]},
                          funarg = "tr",
                          funbod = CaseE (VarE "tr")
                                         [("Leaf",
                                           [("n", "l0")],
                                           LetE ("v", [], IntTy, PrimAppE AddP [VarE "n",LitE 1])
                                                (VarE "v")),
                                          ("Node",
                                           [("x", "lx"),("y", "ly")],
                                           Ext (LetLocE "lx1"
                                                        (AfterConstantLE 1 "lout")
                                                        (LetE ("x1",
                                                               [],
                                                               PackedTy "Tree" "lx1",
                                                               AppE "add1"
                                                                    ["lx","lx1"]
                                                                    (VarE "x"))
                                                              (Ext (LetLocE "ly1"
                                                                            (AfterVariableLE "x1"
                                                                                             "lx1")
                                                                            (LetE ("y1",
                                                                                   [],
                                                                                   PackedTy "Tree"
                                                                                            "ly1",
                                                                                   AppE "add1"
                                                                                        ["ly",
                                                                                         "ly1"]
                                                                                        (VarE "y"))
                                                                                  (LetE ("z",
                                                                                         [],
                                                                                         PackedTy "Tree"
                                                                                                  "lout",
                                                                                         DataConE "lout"
                                                                                                  "Node"
                                                                                                  [VarE "x1",
                                                                                                   VarE "y1"])
                                                                                        (VarE "z"))))))))]})],
      mainExp = Nothing}
-}
