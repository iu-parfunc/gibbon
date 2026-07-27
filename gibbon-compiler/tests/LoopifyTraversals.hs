{-# LANGUAGE TemplateHaskell #-}

module LoopifyTraversals
  ( loopifyTraversalsTests
  ) where

import qualified Data.Map as M

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Gibbon.Common
import Gibbon.DynFlags
import Gibbon.Language
import Gibbon.L3.Syntax
import Gibbon.L3.Typecheck (tcProg)
import qualified Gibbon.L4.Syntax as T
import Gibbon.Passes.Lower (lower)
import Gibbon.Passes.LoopifyTraversals
import Gibbon.Passes.LoopifiedTraversalFusion

runner :: Prog3 -> Prog3
runner prg = fst $ defaultPackedRunPassM $ loopifyTraversals prg

runnerWithCounts :: Prog3 -> Prog3
runnerWithCounts prg =
  fst $
    runPassM
      (defaultConfig {dynflags = loopifyDFlags})
      0
      (loopifyTraversals prg)

runnerWithCountsAuto :: Prog3 -> Prog3
runnerWithCountsAuto prg =
  fst $
    runPassM
      (defaultConfig {dynflags = gopt_set Opt_AutoLoopification loopifyDFlags})
      0
      (loopifyTraversals prg)

loopifyDFlags :: DynFlags
loopifyDFlags =
  gopt_set Opt_EnableLoopification $
    gopt_set Opt_StoreScalarFieldCounts (dynflags defaultConfig)

withoutFunOpts :: Var -> Prog3 -> Prog3
withoutFunOpts fn prg@Prog{fundefs} =
  prg { fundefs = M.adjust clearOpts fn fundefs }
  where
    clearOpts fd@FunDef{funMeta} = fd { funMeta = funMeta { funOpt = [] } }

withCountedProducer :: TyCon -> Prog3 -> Prog3
withCountedProducer tycon prg@Prog{fundefs} =
  prg { fundefs = M.insert "countedBuilder" (countedBuilderFun tycon) fundefs }

runnerWithCountsNoFusion :: Prog3 -> Prog3
runnerWithCountsNoFusion prg =
  fst $
    runPassM
      ( defaultConfig
          { dynflags =
              loopifyDFlags
          }
      )
      0
      (loopifyTraversals prg)

runnerWithCountsThenFusion :: Prog3 -> Prog3
runnerWithCountsThenFusion prg =
  fst $
    runPassM
      (defaultConfig {dynflags = gopt_set Opt_EnableLoopFusion loopifyDFlags})
      0
      (loopifyTraversals prg >>= fuseLoopifiedTraversals)

runLowering :: Prog3 -> T.Prog
runLowering prg = fst $ defaultPackedRunPassM $ do
  prg' <- tcProg True prg
  lower prg'

listProg :: MemoryLayout -> [FunOpt] -> Prog3
listProg layout opts =
  Prog
    (M.fromList [("List", listDDef layout)])
    (M.fromList [("add1List", listFun opts)])
    Nothing

mixedProg :: Prog3
mixedProg =
  Prog
    (M.fromList [("List", listDDef FullyFactored), ("Tree", treeDDef FullyFactored)])
    (M.fromList [("mix", mixedFun [CanVectorize])])
    Nothing

loopIrProg :: Prog3
loopIrProg =
  Prog
    M.empty
    (M.fromList [("loopProbe", loopProbeFun)])
    Nothing

cursorizedLoopifyProg :: Prog3
cursorizedLoopifyProg =
  Prog
    (M.fromList [("List", cursorizedListDDef)])
    (M.fromList [("fastAdd1List", cursorizedLoopifyFun)])
    Nothing

cursorizedMutableLoopifyProg :: Prog3
cursorizedMutableLoopifyProg =
  Prog
    (M.fromList [("List", cursorizedListDDef)])
    (M.fromList [("fastAdd1ListMut", cursorizedMutableLoopifyFun)])
    Nothing

cursorizedMutableLoopifyExtraCursorProg :: Prog3
cursorizedMutableLoopifyExtraCursorProg =
  Prog
    (M.fromList [("List", cursorizedListDDef)])
    (M.fromList [("fastAdd1ListMutExtra", cursorizedMutableLoopifyExtraCursorFun)])
    Nothing

cursorizedMutableParentChildDependentProg :: Prog3
cursorizedMutableParentChildDependentProg =
  Prog
    (M.fromList [("List", cursorizedListDDef)])
    (M.fromList [("badParentChildMut", cursorizedMutableParentChildDependentFun)])
    Nothing

cursorizedMutableTreeLoopifyProg :: Prog3
cursorizedMutableTreeLoopifyProg =
  Prog
    (M.fromList [("Tree", treeDDef FullyFactored)])
    (M.fromList [("fastAdd1TreeMut", cursorizedMutableTreeLoopifyFun)])
    Nothing

cursorizedRealisticMutableLoopifyProg :: Prog3
cursorizedRealisticMutableLoopifyProg =
  Prog
    (M.fromList [("List", cursorizedRuntimeListDDef)])
    (M.fromList [("fastAdd1ListRealisticMut", cursorizedRealisticMutableLoopifyFun)])
    Nothing

listDDef :: MemoryLayout -> DDef3
listDDef layout =
  DDef
    { tyName = "List"
    , tyArgs = []
    , dataCons =
        [ ("Nil", [])
        , ("Cons", [(False, IntTy), (True, PackedTy "List" ())])
        ]
    , memLayout = layout
    }

treeDDef :: MemoryLayout -> DDef3
treeDDef layout =
  DDef
    { tyName = "Tree"
    , tyArgs = []
    , dataCons =
        [ ("Leaf", [(False, IntTy)])
        , ("Node", [(True, PackedTy "Tree" ()), (True, PackedTy "Tree" ())])
        ]
    , memLayout = layout
    }

cursorizedListDDef :: DDef3
cursorizedListDDef =
  DDef
    { tyName = "List"
    , tyArgs = []
    , dataCons =
        [ ("Nil", [])
        , ("Cons", [(False, IntTy), (False, FloatTy), (True, PackedTy "List" ())])
        ]
    , memLayout = FullyFactored
    }

cursorizedRuntimeListDDef :: DDef3
cursorizedRuntimeListDDef =
  DDef
    { tyName = "List"
    , tyArgs = []
    , dataCons =
        [ ("Nil", [])
        , ("Cons", [(False, IntTy), (False, FloatTy), (True, PackedTy "List" ())])
        , ("INDIRECTION_0", [(False, CursorArrayTy 3)])
        , ("REDIRECTION_1", [(False, CursorTy)])
        ]
    , memLayout = FullyFactored
    }

listFun :: [FunOpt] -> FunDef3
listFun opts =
  FunDef
    "add1List"
    ["xs"]
    ([CursorArrayTy 2], CursorArrayTy 2)
    listBody
    (FunMeta Rec NoInline False opts)

mixedFun :: [FunOpt] -> FunDef3
mixedFun opts =
  FunDef
    "mix"
    ["xs"]
    ([CursorArrayTy 2], CursorArrayTy 2)
    mixedBody
    (FunMeta Rec NoInline False opts)

loopProbeFun :: FunDef3
loopProbeFun =
  FunDef
    "loopProbe"
    ["footer"]
    ([CursorTy], ProdTy [])
    loopProbeBody
    (FunMeta Rec NoInline False [])

countedBuilderFun :: TyCon -> FunDef3
countedBuilderFun tycon =
  FunDef
    "countedBuilder"
    ["footer"]
    ([CursorTy], ProdTy [])
    (Ext $ ScalarCountBump dcon ["footer"])
    (FunMeta Rec NoInline False [StoreScalarCounts])
  where
    dcon = case tycon of
             "List" -> "Cons"
             "Tree" -> "Leaf"
             _ -> error $ "countedBuilderFun: unknown tycon " ++ sdoc tycon

cursorizedLoopifyFun :: FunDef3
cursorizedLoopifyFun =
  FunDef
    "fastAdd1List"
    ["inEnds", "outEnds", "outCurs", "inCurs"]
    ([CursorArrayTy 3, CursorArrayTy 3, CursorArrayTy 3, CursorArrayTy 3], ProdTy [CursorArrayTy 3, CursorArrayTy 3, CursorArrayTy 3, ProdTy [CursorArrayTy 3, CursorArrayTy 3]])
    cursorizedLoopifyBody
    (FunMeta TailRec NoInline False [CanVectorize])

cursorizedMutableLoopifyFun :: FunDef3
cursorizedMutableLoopifyFun =
  FunDef
    "fastAdd1ListMut"
    ["inEnds", "outEnds", "outCurs", "inCurs"]
    ([CursorArrayTy 3, CursorArrayTy 3, CursorArrayTy 3, CursorArrayTy 3], ProdTy [])
    cursorizedMutableLoopifyBody
    (FunMeta TailRec NoInline False [CanVectorize])

cursorizedMutableLoopifyExtraCursorFun :: FunDef3
cursorizedMutableLoopifyExtraCursorFun =
  FunDef
    "fastAdd1ListMutExtra"
    ["inEnds", "outEnds", "outCurs", "inCurs", "spareCursors"]
    ([CursorArrayTy 3, CursorArrayTy 3, CursorArrayTy 3, CursorArrayTy 3, CursorArrayTy 1], ProdTy [])
    (cursorizedMutableLoopifyBodyFor "fastAdd1ListMutExtra")
    (FunMeta TailRec NoInline False [CanVectorize])

cursorizedMutableParentChildDependentFun :: FunDef3
cursorizedMutableParentChildDependentFun =
  FunDef
    "badParentChildMut"
    ["inEnds", "outEnds", "outCurs", "inCurs"]
    ([CursorArrayTy 3, CursorArrayTy 3, CursorArrayTy 3, CursorArrayTy 3], ProdTy [])
    cursorizedMutableParentChildDependentBody
    (FunMeta TailRec NoInline False [CanVectorize])

cursorizedMutableTreeLoopifyFun :: FunDef3
cursorizedMutableTreeLoopifyFun =
  FunDef
    "fastAdd1TreeMut"
    ["inEnds", "outEnds", "outCurs", "inCurs"]
    ([CursorArrayTy 2, CursorArrayTy 2, CursorArrayTy 2, CursorArrayTy 2], ProdTy [])
    cursorizedMutableTreeLoopifyBody
    (FunMeta TailRec NoInline False [CanVectorize])

cursorizedRealisticMutableLoopifyFun :: FunDef3
cursorizedRealisticMutableLoopifyFun =
  FunDef
    "fastAdd1ListRealisticMut"
    ["inEnds", "outEnds", "outCurs", "inCurs"]
    ([CursorArrayTy 3, CursorArrayTy 3, CursorArrayTy 3, CursorArrayTy 3], ProdTy [])
    cursorizedRealisticMutableLoopifyBody
    (FunMeta TailRec NoInline False [CanVectorize])

listBody :: Exp3
listBody =
  CaseE
    (VarE "xs")
    [ ("Nil", [], DataConE () "Nil" [])
    , ("Cons", [("i", ()), ("rst", ())], DataConE () "Cons" [VarE "i", VarE "rst"])
    ]

mixedBody :: Exp3
mixedBody =
  LetE
    ("t", [], CursorTy, Ext $ WriteTag "Leaf" "some_cur")
    (CaseE
      (VarE "xs")
      [ ("Nil", [], DataConE () "Nil" [])
      , ("Cons", [("i", ()), ("rst", ())], DataConE () "Cons" [VarE "i", VarE "rst"])
      ])

loopProbeBody :: Exp3
loopProbeBody =
  LetE
    ("first", [], CursorTy, Ext $ ReadScalarCountFirstFooter "footer")
    (LetE
      ("count", [], IntTy, Ext $ ReadScalarCount "first")
      (LetE
        ("next", [], CursorTy, Ext $ ReadScalarCountNextFooter "first")
        (LetE
          ("nextCount", [], IntTy, Ext $ ReadScalarCount "next")
          (LetE
            ("_loop", [], ProdTy [], Ext $ ForE "i" (VarE "count") (MkProdE []))
            (MkProdE [])))))

cursorizedLoopifyBody :: Exp3
cursorizedLoopifyBody =
  mkLets
    [ ("dout_end", [], CursorTy, Ext $ IndexCursorArray "outEnds" 0)
    , ("int_out_end", [], CursorTy, Ext $ IndexCursorArray "outEnds" 1)
    , ("float_out_end", [], CursorTy, Ext $ IndexCursorArray "outEnds" 2)
    , ("int_out_loc", [], CursorTy, Ext $ IndexCursorArray "outCurs" 1)
    , ("dout_loc", [], CursorTy, Ext $ IndexCursorArray "outCurs" 0)
    , ("float_out_loc", [], CursorTy, Ext $ IndexCursorArray "outCurs" 2)
    , ( "_bounds"
      , []
      , IntTy
      , Ext $ BoundsCheckVector [(13, "float_out_end", "float_out_loc", ("float_out_end", "float_out_loc"))
                                ,(17, "int_out_end", "int_out_loc", ("int_out_end", "int_out_loc"))
                                ,(34, "dout_end", "dout_loc", ("dout_end", "dout_loc"))]
      )
    , ("overwrite_reg", [], CursorArrayTy 3, Ext $ MakeCursorArray 3 ["dout_end", "int_out_end", "float_out_end"])
    , ("dcur", [], CursorTy, Ext $ IndexCursorArray "inCurs" 0)
    ]
    (CaseE
      (VarE "dcur")
      [ ("Nil", [], nilBranch)
      , ("Cons", [], recBranch)
      ])
  where
    nilBranch =
      mkLets
        [ ("writetag_nil", [], CursorTy, Ext $ WriteTag "Nil" "dout_loc")
        , ("after_tag_nil", [], CursorTy, Ext $ AddCursor "dout_loc" (LitE 1))
        , ("aft_nil", [], CursorArrayTy 3, Ext $ MakeCursorArray 3 ["after_tag_nil", "int_out_loc", "float_out_loc"])
        , ("packed_nil", [], ProdTy [CursorArrayTy 3, CursorArrayTy 3], MkProdE [VarE "outCurs", VarE "aft_nil"])
        ]
        (MkProdE [VarE "inEnds", VarE "overwrite_reg", VarE "inCurs", VarE "packed_nil"])

    recBranch =
      mkLets
        [ ("in_int_cur", [], CursorTy, Ext $ IndexCursorArray "inCurs" 1)
        , ("read_int_pair", [], ProdTy [IntTy, CursorTy], Ext $ ReadScalar IntS "in_int_cur")
        , ("i", [], IntTy, ProjE 0 (VarE "read_int_pair"))
        , ("plus1", [], IntTy, PrimAppE AddP [VarE "i", LitE 1])
        , ("out_int_cur", [], CursorTy, Ext $ IndexCursorArray "outCurs" 1)
        , ("write_int", [], CursorTy, Ext $ WriteScalar IntS "out_int_cur" (VarE "plus1"))
        , ("in_float_cur", [], CursorTy, Ext $ IndexCursorArray "inCurs" 2)
        , ("read_float_pair", [], ProdTy [FloatTy, CursorTy], Ext $ ReadScalar FloatS "in_float_cur")
        , ("f", [], FloatTy, ProjE 0 (VarE "read_float_pair"))
        , ("out_float_cur", [], CursorTy, Ext $ IndexCursorArray "outCurs" 2)
        , ("write_float", [], CursorTy, Ext $ WriteScalar FloatS "out_float_cur" (VarE "f"))
        , ("dcon_next", [], CursorTy, Ext $ AddCursor "dcur" (LitE 1))
        , ("next_in", [], CursorArrayTy 3, Ext $ MakeCursorArray 3 ["dcon_next", "in_int_cur", "in_float_cur"])
        , ("next_out_dcon", [], CursorTy, Ext $ AddCursor "dout_loc" (LitE 1))
        , ("next_out_int", [], CursorTy, Ext $ AddCursor "int_out_loc" (LitE 8))
        , ("next_out_float", [], CursorTy, Ext $ AddCursor "float_out_loc" (LitE 4))
        , ("next_out", [], CursorArrayTy 3, Ext $ MakeCursorArray 3 ["next_out_dcon", "next_out_int", "next_out_float"])
        , ("writetag_cons", [], CursorTy, Ext $ WriteTag "Cons" "dout_loc")
        , ("recur", [], ProdTy [CursorArrayTy 3, CursorArrayTy 3, CursorArrayTy 3, ProdTy [CursorArrayTy 3, CursorArrayTy 3]], AppE "fastAdd1List" TailModuloCons [] [VarE "inEnds", VarE "overwrite_reg", VarE "next_out", VarE "next_in"])
        ]
        (VarE "recur")

-- | A cursorized nullary-constructor branch.  Real cursorize output always
-- writes the branch's own constructor tag into the output tag stream and
-- advances both dcon cursors, so the fixtures model that.
mutableNilBranch :: DataCon -> Exp3
mutableNilBranch dcon =
  mkLets
    [ ("nil_out_dcon_cur", [], CursorTy, Ext $ DerefMutCursor "out_dcon_loc")
    , ("nil_write_tag", [], CursorTy, Ext $ WriteTag dcon "nil_out_dcon_cur")
    , ("nil_bump_dcon_in", [], ProdTy [], Ext $ BumpCursorMutable "in_dcon_loc" (LitE 1))
    , ("nil_bump_dcon_out", [], ProdTy [], Ext $ BumpCursorMutable "out_dcon_loc" (LitE 1))
    ]
    (MkProdE [])

cursorizedMutableLoopifyBody :: Exp3
cursorizedMutableLoopifyBody = cursorizedMutableLoopifyBodyFor "fastAdd1ListMut"

-- | Same body, parameterized by the name of the enclosing function so that the
-- recursive call really is a self call (as it is in real cursorize output).
cursorizedMutableLoopifyBodyFor :: Var -> Exp3
cursorizedMutableLoopifyBodyFor selfName =
  mkLets
    [ ("in_dcon_loc", [], MutCursorTy, Ext $ AddrOfCursor (Ext $ IndexCursorArray "inCurs" 0))
    , ("out_dcon_loc", [], MutCursorTy, Ext $ AddrOfCursor (Ext $ IndexCursorArray "outCurs" 0))
    , ("in_int_loc", [], MutCursorTy, Ext $ AddrOfCursor (Ext $ IndexCursorArray "inCurs" 1))
    , ("out_int_loc", [], MutCursorTy, Ext $ AddrOfCursor (Ext $ IndexCursorArray "outCurs" 1))
    , ("in_float_loc", [], MutCursorTy, Ext $ AddrOfCursor (Ext $ IndexCursorArray "inCurs" 2))
    , ("out_float_loc", [], MutCursorTy, Ext $ AddrOfCursor (Ext $ IndexCursorArray "outCurs" 2))
    , ("dcur", [], CursorTy, Ext $ DerefMutCursor "in_dcon_loc")
    ]
    (CaseE
      (VarE "dcur")
      [ ("Nil", [], mutableNilBranch "Nil")
      , ("Cons", [], recBranch)
      ])
  where
    recBranch =
      mkLets
        [ ("in_int_cur", [], CursorTy, Ext $ DerefMutCursor "in_int_loc")
        , ("read_int_pair", [], ProdTy [IntTy, CursorTy], Ext $ ReadScalar IntS "in_int_cur")
        , ("i", [], IntTy, ProjE 0 (VarE "read_int_pair"))
        , ("plus1", [], IntTy, PrimAppE AddP [VarE "i", LitE 1])
        , ("out_int_cur", [], CursorTy, Ext $ DerefMutCursor "out_int_loc")
        , ("write_int", [], CursorTy, Ext $ WriteScalar IntS "out_int_cur" (VarE "plus1"))
        , ("bump_int_in", [], ProdTy [], Ext $ BumpCursorMutable "in_int_loc" (LitE 8))
        , ("bump_int_out", [], ProdTy [], Ext $ BumpCursorMutable "out_int_loc" (LitE 8))
        , ("in_float_cur", [], CursorTy, Ext $ DerefMutCursor "in_float_loc")
        , ("read_float_pair", [], ProdTy [FloatTy, CursorTy], Ext $ ReadScalar FloatS "in_float_cur")
        , ("f", [], FloatTy, ProjE 0 (VarE "read_float_pair"))
        , ("out_float_cur", [], CursorTy, Ext $ DerefMutCursor "out_float_loc")
        , ("write_float", [], CursorTy, Ext $ WriteScalar FloatS "out_float_cur" (VarE "f"))
        , ("bump_float_in", [], ProdTy [], Ext $ BumpCursorMutable "in_float_loc" (LitE 4))
        , ("bump_float_out", [], ProdTy [], Ext $ BumpCursorMutable "out_float_loc" (LitE 4))
        , ("out_dcon_cur", [], CursorTy, Ext $ DerefMutCursor "out_dcon_loc")
        , ("write_tag", [], CursorTy, Ext $ WriteTag "Cons" "out_dcon_cur")
        , ("bump_dcon_in", [], ProdTy [], Ext $ BumpCursorMutable "in_dcon_loc" (LitE 1))
        , ("bump_dcon_out", [], ProdTy [], Ext $ BumpCursorMutable "out_dcon_loc" (LitE 1))
        , ("recur", [], ProdTy [], AppE selfName TailModuloCons [] [VarE "inEnds", VarE "outEnds", VarE "outCurs", VarE "inCurs"])
        ]
        (MkProdE [])

cursorizedMutableParentChildDependentBody :: Exp3
cursorizedMutableParentChildDependentBody =
  mkLets
    [ ("in_dcon_loc", [], MutCursorTy, Ext $ AddrOfCursor (Ext $ IndexCursorArray "inCurs" 0))
    , ("out_dcon_loc", [], MutCursorTy, Ext $ AddrOfCursor (Ext $ IndexCursorArray "outCurs" 0))
    , ("in_int_loc", [], MutCursorTy, Ext $ AddrOfCursor (Ext $ IndexCursorArray "inCurs" 1))
    , ("out_int_loc", [], MutCursorTy, Ext $ AddrOfCursor (Ext $ IndexCursorArray "outCurs" 1))
    , ("in_float_loc", [], MutCursorTy, Ext $ AddrOfCursor (Ext $ IndexCursorArray "inCurs" 2))
    , ("out_float_loc", [], MutCursorTy, Ext $ AddrOfCursor (Ext $ IndexCursorArray "outCurs" 2))
    , ("dcur", [], CursorTy, Ext $ DerefMutCursor "in_dcon_loc")
    ]
    (CaseE
      (VarE "dcur")
      [ ("Nil", [], mutableNilBranch "Nil")
      , ("Cons", [], recBranch)
      ])
  where
    recBranch =
      mkLets
        [ ("in_int_cur", [], CursorTy, Ext $ DerefMutCursor "in_int_loc")
        , ("read_int_pair", [], ProdTy [IntTy, CursorTy], Ext $ ReadScalar IntS "in_int_cur")
        , ("i", [], IntTy, ProjE 0 (VarE "read_int_pair"))
        , ("out_int_cur", [], CursorTy, Ext $ DerefMutCursor "out_int_loc")
        , ("write_int", [], CursorTy, Ext $ WriteScalar IntS "out_int_cur" (VarE "i"))
        , ("bump_int_in", [], ProdTy [], Ext $ BumpCursorMutable "in_int_loc" (LitE 8))
        , ("bump_int_out", [], ProdTy [], Ext $ BumpCursorMutable "out_int_loc" (LitE 8))
        , ("in_float_cur", [], CursorTy, Ext $ DerefMutCursor "in_float_loc")
        , ("read_float_pair", [], ProdTy [FloatTy, CursorTy], Ext $ ReadScalar FloatS "in_float_cur")
        , ("f", [], FloatTy, ProjE 0 (VarE "read_float_pair"))
        , ("out_float_cur", [], CursorTy, Ext $ DerefMutCursor "out_float_loc")
        , ("write_float", [], CursorTy, Ext $ WriteScalar FloatS "out_float_cur" (VarE "f"))
        , ("bump_float_in", [], ProdTy [], Ext $ BumpCursorMutable "in_float_loc" (LitE 4))
        , ("bump_float_out", [], ProdTy [], Ext $ BumpCursorMutable "out_float_loc" (LitE 4))
        , ("out_dcon_cur", [], CursorTy, Ext $ DerefMutCursor "out_dcon_loc")
        , ("write_tag", [], CursorTy, Ext $ WriteTag "Cons" "out_dcon_cur")
        , ("bump_dcon_in", [], ProdTy [], Ext $ BumpCursorMutable "in_dcon_loc" (LitE 1))
        , ("bump_dcon_out", [], ProdTy [], Ext $ BumpCursorMutable "out_dcon_loc" (LitE 1))
        , ("child_cond", [], BoolTy, AppE "badParentChildMut" TailModuloCons [] [VarE "inEnds", VarE "outEnds", VarE "outCurs", VarE "inCurs"])
        ]
        (IfE (VarE "child_cond") (MkProdE []) (MkProdE []))

cursorizedMutableTreeLoopifyBody :: Exp3
cursorizedMutableTreeLoopifyBody =
  mkLets
    [ ("in_dcon_loc", [], MutCursorTy, Ext $ AddrOfCursor (Ext $ IndexCursorArray "inCurs" 0))
    , ("out_dcon_loc", [], MutCursorTy, Ext $ AddrOfCursor (Ext $ IndexCursorArray "outCurs" 0))
    , ("in_int_loc", [], MutCursorTy, Ext $ AddrOfCursor (Ext $ IndexCursorArray "inCurs" 1))
    , ("out_int_loc", [], MutCursorTy, Ext $ AddrOfCursor (Ext $ IndexCursorArray "outCurs" 1))
    , ("dcur", [], CursorTy, Ext $ DerefMutCursor "in_dcon_loc")
    ]
    (CaseE
      (VarE "dcur")
      [ ("Leaf", [], leafBranch)
      , ("Node", [], nodeBranch)
      ])
  where
    leafBranch =
      mkLets
        [ ("in_int_cur", [], CursorTy, Ext $ DerefMutCursor "in_int_loc")
        , ("read_int_pair", [], ProdTy [IntTy, CursorTy], Ext $ ReadScalar IntS "in_int_cur")
        , ("i", [], IntTy, ProjE 0 (VarE "read_int_pair"))
        , ("plus1", [], IntTy, PrimAppE AddP [VarE "i", LitE 1])
        , ("out_int_cur", [], CursorTy, Ext $ DerefMutCursor "out_int_loc")
        , ("write_int", [], CursorTy, Ext $ WriteScalar IntS "out_int_cur" (VarE "plus1"))
        , ("out_dcon_cur", [], CursorTy, Ext $ DerefMutCursor "out_dcon_loc")
        , ("write_tag_leaf", [], CursorTy, Ext $ WriteTag "Leaf" "out_dcon_cur")
        , ("bump_int_in", [], ProdTy [], Ext $ BumpCursorMutable "in_int_loc" (LitE 8))
        , ("bump_int_out", [], ProdTy [], Ext $ BumpCursorMutable "out_int_loc" (LitE 8))
        , ("bump_dcon_in", [], ProdTy [], Ext $ BumpCursorMutable "in_dcon_loc" (LitE 1))
        , ("bump_dcon_out", [], ProdTy [], Ext $ BumpCursorMutable "out_dcon_loc" (LitE 1))
        ]
        (MkProdE [])

    nodeBranch =
      mkLets
        [ ("out_dcon_cur", [], CursorTy, Ext $ DerefMutCursor "out_dcon_loc")
        , ("write_tag_node", [], CursorTy, Ext $ WriteTag "Node" "out_dcon_cur")
        , ("bump_dcon_in", [], ProdTy [], Ext $ BumpCursorMutable "in_dcon_loc" (LitE 1))
        , ("bump_dcon_out", [], ProdTy [], Ext $ BumpCursorMutable "out_dcon_loc" (LitE 1))
        ]
        (MkProdE [])

cursorizedRealisticMutableLoopifyBody :: Exp3
cursorizedRealisticMutableLoopifyBody =
  mkLets
    [ ("in_dcon_loc", [], MutCursorTy, Ext $ AddrOfCursor (Ext $ IndexCursorArray "inCurs" 0))
    , ("out_dcon_loc", [], MutCursorTy, Ext $ AddrOfCursor (Ext $ IndexCursorArray "outCurs" 0))
    , ("in_int_loc", [], MutCursorTy, Ext $ AddrOfCursor (Ext $ IndexCursorArray "inCurs" 1))
    , ("out_int_loc", [], MutCursorTy, Ext $ AddrOfCursor (Ext $ IndexCursorArray "outCurs" 1))
    , ("in_float_loc", [], MutCursorTy, Ext $ AddrOfCursor (Ext $ IndexCursorArray "inCurs" 2))
    , ("out_float_loc", [], MutCursorTy, Ext $ AddrOfCursor (Ext $ IndexCursorArray "outCurs" 2))
    , ("dcur", [], CursorTy, Ext $ DerefMutCursor "in_dcon_loc")
    ]
    (CaseE
      (VarE "dcur")
      [ ("Nil", [], mutableNilBranch "Nil")
      , ("Cons", [], recBranch)
      ])
  where
    recBranch =
      mkLets
        [ ("in_int_cur", [], CursorTy, Ext $ DerefMutCursor "in_int_loc")
        , ("read_int_pair", [], ProdTy [IntTy, CursorTy], Ext $ ReadScalar IntS "in_int_cur")
        , ("i", [], IntTy, ProjE 0 (VarE "read_int_pair"))
        , ("plus1", [], IntTy, PrimAppE AddP [VarE "i", LitE 1])
        , ("in_float_cur", [], CursorTy, Ext $ DerefMutCursor "in_float_loc")
        , ("read_float_pair", [], ProdTy [FloatTy, CursorTy], Ext $ ReadScalar FloatS "in_float_cur")
        , ("f", [], FloatTy, ProjE 0 (VarE "read_float_pair"))
        , ( "tup_packed"
          , []
          , ProdTy []
          , mkLets
              [ ("out_dcon_cur", [], CursorTy, Ext $ DerefMutCursor "out_dcon_loc")
              , ("write_tag", [], CursorTy, Ext $ WriteTag "Cons" "out_dcon_cur")
              , ("out_int_cur", [], CursorTy, Ext $ DerefMutCursor "out_int_loc")
              , ("write_int", [], CursorTy, Ext $ WriteScalar IntS "out_int_cur" (VarE "plus1"))
              , ("out_float_cur", [], CursorTy, Ext $ DerefMutCursor "out_float_loc")
              , ("write_float", [], CursorTy, Ext $ WriteScalar FloatS "out_float_cur" (VarE "f"))
              , ("bump_dcon_in", [], ProdTy [], Ext $ BumpCursorMutable "in_dcon_loc" (LitE 1))
              , ("bump_dcon_out", [], ProdTy [], Ext $ BumpCursorMutable "out_dcon_loc" (LitE 1))
              , ("bump_int_in", [], ProdTy [], Ext $ BumpCursorMutable "in_int_loc" (LitE 8))
              , ("bump_int_out", [], ProdTy [], Ext $ BumpCursorMutable "out_int_loc" (LitE 8))
              , ("bump_float_in", [], ProdTy [], Ext $ BumpCursorMutable "in_float_loc" (LitE 4))
              , ("bump_float_out", [], ProdTy [], Ext $ BumpCursorMutable "out_float_loc" (LitE 4))
              ]
              (MkProdE [])
          )
        , ("recur", [], ProdTy [], AppE "fastAdd1ListRealisticMut" TailModuloCons [] [VarE "inEnds", VarE "outEnds", VarE "outCurs", VarE "inCurs"])
        ]
        (MkProdE [])

collectTailPrims :: T.Tail -> [T.Prim]
collectTailPrims tl =
  case tl of
    T.EndOfMain -> []
    T.RetValsT _ -> []
    T.AssnValsT _ mb -> maybe [] collectTailPrims mb
    T.LetCallT {T.bod} -> collectTailPrims bod
    T.LetPrimCallT {T.prim, T.bod} -> prim : collectTailPrims bod
    T.LetTrivT {T.bod} -> collectTailPrims bod
    T.LetIfT {T.ife = (_, t1, t2), T.bod} ->
      collectTailPrims t1 ++ collectTailPrims t2 ++ collectTailPrims bod
    T.LetUnpackT {T.bod} -> collectTailPrims bod
    T.LetAllocT {T.bod} -> collectTailPrims bod
    T.LetAvailT {T.bod} -> collectTailPrims bod
    T.ForLoopT {T.loopBody, T.bod} ->
      collectTailPrims loopBody ++ collectTailPrims bod
    T.WhileCursorT {T.loopBody, T.bod} ->
      collectTailPrims loopBody ++ collectTailPrims bod
    T.IfT {T.con, T.els} ->
      collectTailPrims con ++ collectTailPrims els
    T.ErrT _ -> []
    T.LetTimedT {T.timed, T.bod} ->
      collectTailPrims timed ++ collectTailPrims bod
    T.Switch _ _ alts mb ->
      collectAltsPrims alts ++ maybe [] collectTailPrims mb
    T.TailCall _ _ -> []
    T.Goto _ -> []
    T.LetArenaT {T.bod} -> collectTailPrims bod

collectAltsPrims :: T.Alts -> [T.Prim]
collectAltsPrims alts =
  case alts of
    T.TagAlts ls -> concatMap (collectTailPrims . snd) ls
    T.IntAlts ls -> concatMap (collectTailPrims . snd) ls

containsForLoop :: T.Tail -> Bool
containsForLoop tl =
  case tl of
    T.ForLoopT {} -> True
    T.EndOfMain -> False
    T.RetValsT _ -> False
    T.AssnValsT _ mb -> maybe False containsForLoop mb
    T.LetCallT {T.bod} -> containsForLoop bod
    T.LetPrimCallT {T.bod} -> containsForLoop bod
    T.LetTrivT {T.bod} -> containsForLoop bod
    T.LetIfT {T.ife = (_, t1, t2), T.bod} ->
      containsForLoop t1 || containsForLoop t2 || containsForLoop bod
    T.LetUnpackT {T.bod} -> containsForLoop bod
    T.LetAllocT {T.bod} -> containsForLoop bod
    T.LetAvailT {T.bod} -> containsForLoop bod
    T.IfT {T.con, T.els} ->
      containsForLoop con || containsForLoop els
    T.ErrT _ -> False
    T.LetTimedT {T.timed, T.bod} ->
      containsForLoop timed || containsForLoop bod
    T.Switch _ _ alts mb ->
      containsLoopAlts alts || maybe False containsForLoop mb
    T.TailCall _ _ -> False
    T.Goto _ -> False
    T.LetArenaT {T.bod} -> containsForLoop bod

containsLoopAlts :: T.Alts -> Bool
containsLoopAlts alts =
  case alts of
    T.TagAlts ls -> any (containsForLoop . snd) ls
    T.IntAlts ls -> any (containsForLoop . snd) ls

containsL3For :: Exp3 -> Bool
containsL3For ex =
  case ex of
    VarE{} -> False
    LitE{} -> False
    CharE{} -> False
    FloatE{} -> False
    LitSymE{} -> False
    AppE _ _ _ args -> any containsL3For args
    PrimAppE _ args -> any containsL3For args
    LetE (_, _, _, rhs) bod -> containsL3For rhs || containsL3For bod
    ProjE _ e -> containsL3For e
    IfE a b c -> any containsL3For [a, b, c]
    MkProdE ls -> any containsL3For ls
    CaseE scrt brs -> containsL3For scrt || any (containsL3For . (\(_, _, rhs) -> rhs)) brs
    DataConE _ _ args -> any containsL3For args
    TimeIt e _ _ -> containsL3For e
    WithArenaE _ e -> containsL3For e
    SpawnE _ _ args -> any containsL3For args
    SyncE -> False
    MapE (_, _, e1) e2 -> containsL3For e1 || containsL3For e2
    FoldE (_, _, e1) (_, _, e2) e3 -> any containsL3For [e1, e2, e3]
    Ext (ForE _ _ _) -> True
    Ext (WhileCursor _ bod) -> containsL3For bod
    Ext (WriteScalar _ _ rhs) -> containsL3For rhs
    Ext (WriteTaggedCursor _ rhs) -> containsL3For rhs
    Ext (WriteCursorMutable _ rhs) -> containsL3For rhs
    Ext (WriteList _ rhs _) -> containsL3For rhs
    Ext (WriteVector _ rhs _) -> containsL3For rhs
    Ext (AddCursor _ rhs) -> containsL3For rhs
    Ext (BumpCursorMutable _ rhs) -> containsL3For rhs
    Ext (AddrOfCursor rhs) -> containsL3For rhs
    Ext (LetAvail _ bod) -> containsL3For bod
    Ext (Assert rhs) -> containsL3For rhs
    Ext _ -> False

countL3For :: Exp3 -> Int
countL3For ex =
  case ex of
    VarE{} -> 0
    LitE{} -> 0
    CharE{} -> 0
    FloatE{} -> 0
    LitSymE{} -> 0
    AppE _ _ _ args -> sum (map countL3For args)
    PrimAppE _ args -> sum (map countL3For args)
    LetE (_, _, _, rhs) bod -> countL3For rhs + countL3For bod
    ProjE _ e -> countL3For e
    IfE a b c -> sum (map countL3For [a, b, c])
    MkProdE ls -> sum (map countL3For ls)
    CaseE scrt brs -> countL3For scrt + sum (map (countL3For . (\(_, _, rhs) -> rhs)) brs)
    DataConE _ _ args -> sum (map countL3For args)
    TimeIt e _ _ -> countL3For e
    WithArenaE _ e -> countL3For e
    SpawnE _ _ args -> sum (map countL3For args)
    SyncE -> 0
    MapE (_, _, e1) e2 -> countL3For e1 + countL3For e2
    FoldE (_, _, e1) (_, _, e2) e3 -> sum (map countL3For [e1, e2, e3])
    Ext (ForE _ bound bod) -> 1 + countL3For bound + countL3For bod
    Ext (WhileCursor _ bod) -> countL3For bod
    Ext (WriteScalar _ _ rhs) -> countL3For rhs
    Ext (WriteTaggedCursor _ rhs) -> countL3For rhs
    Ext (WriteCursorMutable _ rhs) -> countL3For rhs
    Ext (WriteList _ rhs _) -> countL3For rhs
    Ext (WriteVector _ rhs _) -> countL3For rhs
    Ext (AddCursor _ rhs) -> countL3For rhs
    Ext (BumpCursorMutable _ rhs) -> countL3For rhs
    Ext (AddrOfCursor rhs) -> countL3For rhs
    Ext (LetAvail _ bod) -> countL3For bod
    Ext (Assert rhs) -> countL3For rhs
    Ext _ -> 0

containsScalarCountRead :: Exp3 -> Bool
containsScalarCountRead ex =
  case ex of
    LetE (_, _, _, rhs) bod -> containsScalarCountRead rhs || containsScalarCountRead bod
    IfE a b c -> any containsScalarCountRead [a, b, c]
    CaseE scrt brs -> containsScalarCountRead scrt || any (containsScalarCountRead . (\(_, _, rhs) -> rhs)) brs
    Ext ReadScalarCount{} -> True
    Ext ReadScalarCountFirstFooter{} -> True
    Ext ReadScalarCountNextFooter{} -> True
    Ext (ForE _ bound body) -> containsScalarCountRead bound || containsScalarCountRead body
    Ext (WhileCursor _ bod) -> containsScalarCountRead bod
    Ext (WriteScalar _ _ rhs) -> containsScalarCountRead rhs
    Ext (WriteTaggedCursor _ rhs) -> containsScalarCountRead rhs
    Ext (WriteCursorMutable _ rhs) -> containsScalarCountRead rhs
    Ext (WriteList _ rhs _) -> containsScalarCountRead rhs
    Ext (WriteVector _ rhs _) -> containsScalarCountRead rhs
    Ext (AddCursor _ rhs) -> containsScalarCountRead rhs
    Ext (BumpCursorMutable _ rhs) -> containsScalarCountRead rhs
    Ext (AddrOfCursor rhs) -> containsScalarCountRead rhs
    Ext (LetAvail _ bod) -> containsScalarCountRead bod
    Ext (Assert rhs) -> containsScalarCountRead rhs
    Ext _ -> False
    VarE{} -> False
    LitE{} -> False
    CharE{} -> False
    FloatE{} -> False
    LitSymE{} -> False
    AppE _ _ _ args -> any containsScalarCountRead args
    PrimAppE _ args -> any containsScalarCountRead args
    ProjE _ e -> containsScalarCountRead e
    MkProdE ls -> any containsScalarCountRead ls
    DataConE _ _ args -> any containsScalarCountRead args
    TimeIt e _ _ -> containsScalarCountRead e
    WithArenaE _ e -> containsScalarCountRead e
    SpawnE _ _ args -> any containsScalarCountRead args
    SyncE -> False
    MapE (_, _, e1) e2 -> containsScalarCountRead e1 || containsScalarCountRead e2
    FoldE (_, _, e1) (_, _, e2) e3 -> any containsScalarCountRead [e1, e2, e3]

containsScalarCountWrite :: Exp3 -> Bool
containsScalarCountWrite ex =
  case ex of
    LetE (_, _, _, rhs) bod -> containsScalarCountWrite rhs || containsScalarCountWrite bod
    IfE a b c -> any containsScalarCountWrite [a, b, c]
    CaseE scrt brs -> containsScalarCountWrite scrt || any (containsScalarCountWrite . (\(_, _, rhs) -> rhs)) brs
    Ext ScalarCountBump{} -> True
    Ext ScalarCountSet{} -> True
    Ext (ForE _ bound body) -> containsScalarCountWrite bound || containsScalarCountWrite body
    Ext (WhileCursor _ bod) -> containsScalarCountWrite bod
    Ext (WriteScalar _ _ rhs) -> containsScalarCountWrite rhs
    Ext (WriteTaggedCursor _ rhs) -> containsScalarCountWrite rhs
    Ext (WriteCursorMutable _ rhs) -> containsScalarCountWrite rhs
    Ext (WriteList _ rhs _) -> containsScalarCountWrite rhs
    Ext (WriteVector _ rhs _) -> containsScalarCountWrite rhs
    Ext (AddCursor _ rhs) -> containsScalarCountWrite rhs
    Ext (BumpCursorMutable _ rhs) -> containsScalarCountWrite rhs
    Ext (AddrOfCursor rhs) -> containsScalarCountWrite rhs
    Ext (LetAvail _ bod) -> containsScalarCountWrite bod
    Ext (Assert rhs) -> containsScalarCountWrite rhs
    Ext _ -> False
    VarE{} -> False
    LitE{} -> False
    CharE{} -> False
    FloatE{} -> False
    LitSymE{} -> False
    AppE _ _ _ args -> any containsScalarCountWrite args
    PrimAppE _ args -> any containsScalarCountWrite args
    ProjE _ e -> containsScalarCountWrite e
    MkProdE ls -> any containsScalarCountWrite ls
    DataConE _ _ args -> any containsScalarCountWrite args
    TimeIt e _ _ -> containsScalarCountWrite e
    WithArenaE _ e -> containsScalarCountWrite e
    SpawnE _ _ args -> any containsScalarCountWrite args
    SyncE -> False
    MapE (_, _, e1) e2 -> containsScalarCountWrite e1 || containsScalarCountWrite e2
    FoldE (_, _, e1) (_, _, e2) e3 -> any containsScalarCountWrite [e1, e2, e3]

containsL3WhileCursor :: Exp3 -> Bool
containsL3WhileCursor ex =
  case ex of
    LetE (_, _, _, rhs) bod -> containsL3WhileCursor rhs || containsL3WhileCursor bod
    IfE a b c -> any containsL3WhileCursor [a, b, c]
    CaseE scrt brs -> containsL3WhileCursor scrt || any (containsL3WhileCursor . (\(_, _, rhs) -> rhs)) brs
    Ext (WhileCursor _ _) -> True
    Ext (ForE _ bound body) -> containsL3WhileCursor bound || containsL3WhileCursor body
    Ext (WriteScalar _ _ rhs) -> containsL3WhileCursor rhs
    Ext (WriteTaggedCursor _ rhs) -> containsL3WhileCursor rhs
    Ext (WriteCursorMutable _ rhs) -> containsL3WhileCursor rhs
    Ext (WriteList _ rhs _) -> containsL3WhileCursor rhs
    Ext (WriteVector _ rhs _) -> containsL3WhileCursor rhs
    Ext (AddCursor _ rhs) -> containsL3WhileCursor rhs
    Ext (BumpCursorMutable _ rhs) -> containsL3WhileCursor rhs
    Ext (AddrOfCursor rhs) -> containsL3WhileCursor rhs
    Ext (LetAvail _ bod) -> containsL3WhileCursor bod
    Ext (Assert rhs) -> containsL3WhileCursor rhs
    Ext _ -> False
    VarE{} -> False
    LitE{} -> False
    CharE{} -> False
    FloatE{} -> False
    LitSymE{} -> False
    AppE _ _ _ args -> any containsL3WhileCursor args
    PrimAppE _ args -> any containsL3WhileCursor args
    ProjE _ e -> containsL3WhileCursor e
    MkProdE ls -> any containsL3WhileCursor ls
    DataConE _ _ args -> any containsL3WhileCursor args
    TimeIt e _ _ -> containsL3WhileCursor e
    WithArenaE _ e -> containsL3WhileCursor e
    SpawnE _ _ args -> any containsL3WhileCursor args
    SyncE -> False
    MapE (_, _, e1) e2 -> containsL3WhileCursor e1 || containsL3WhileCursor e2
    FoldE (_, _, e1) (_, _, e2) e3 -> any containsL3WhileCursor [e1, e2, e3]

containsWriteTagPacked :: Exp3 -> Bool
containsWriteTagPacked ex =
  case ex of
    LetE (_, _, _, rhs) bod -> containsWriteTagPacked rhs || containsWriteTagPacked bod
    IfE a b c -> any containsWriteTagPacked [a, b, c]
    CaseE scrt brs -> containsWriteTagPacked scrt || any (containsWriteTagPacked . (\(_, _, rhs) -> rhs)) brs
    Ext (WriteTagPacked _ rhs) -> containsWriteTagPacked rhs || True
    Ext (ForE _ bound body) -> containsWriteTagPacked bound || containsWriteTagPacked body
    Ext (WhileCursor _ bod) -> containsWriteTagPacked bod
    Ext (WriteScalar _ _ rhs) -> containsWriteTagPacked rhs
    Ext (WriteTaggedCursor _ rhs) -> containsWriteTagPacked rhs
    Ext (WriteCursorMutable _ rhs) -> containsWriteTagPacked rhs
    Ext (WriteList _ rhs _) -> containsWriteTagPacked rhs
    Ext (WriteVector _ rhs _) -> containsWriteTagPacked rhs
    Ext (AddCursor _ rhs) -> containsWriteTagPacked rhs
    Ext (BumpCursorMutable _ rhs) -> containsWriteTagPacked rhs
    Ext (AddrOfCursor rhs) -> containsWriteTagPacked rhs
    Ext (LetAvail _ bod) -> containsWriteTagPacked bod
    Ext (Assert rhs) -> containsWriteTagPacked rhs
    Ext _ -> False
    VarE{} -> False
    LitE{} -> False
    CharE{} -> False
    FloatE{} -> False
    LitSymE{} -> False
    AppE _ _ _ args -> any containsWriteTagPacked args
    PrimAppE _ args -> any containsWriteTagPacked args
    ProjE _ e -> containsWriteTagPacked e
    MkProdE ls -> any containsWriteTagPacked ls
    DataConE _ _ args -> any containsWriteTagPacked args
    TimeIt e _ _ -> containsWriteTagPacked e
    WithArenaE _ e -> containsWriteTagPacked e
    SpawnE _ _ args -> any containsWriteTagPacked args
    SyncE -> False
    MapE (_, _, e1) e2 -> containsWriteTagPacked e1 || containsWriteTagPacked e2
    FoldE (_, _, e1) (_, _, e2) e3 -> any containsWriteTagPacked [e1, e2, e3]

case_detects_fully_factored_can_vectorize_candidate :: Assertion
case_detects_fully_factored_can_vectorize_candidate =
  let prg = listProg FullyFactored [CanVectorize]
      fd = (fundefs prg) M.! "add1List"
   in Just (LoopifyCandidate "add1List" "List" ["Nil", "Cons"]) @=?
        loopifyCandidateInfo (ddefs prg) fd

case_requires_annotation :: Assertion
case_requires_annotation =
  let prg = listProg FullyFactored []
      fd = (fundefs prg) M.! "add1List"
   in Nothing @=? loopifyCandidateInfo (ddefs prg) fd

case_auto_detection_allows_unannotated_candidate :: Assertion
case_auto_detection_allows_unannotated_candidate =
  let prg = listProg FullyFactored []
      fd = (fundefs prg) M.! "add1List"
   in Just (LoopifyCandidate "add1List" "List" ["Nil", "Cons"]) @=?
        loopifyCandidateInfoWith True (ddefs prg) fd

case_auto_detection_skips_generated_packed_helpers :: Assertion
case_auto_detection_skips_generated_packed_helpers =
  let fd0 = cursorizedMutableLoopifyFun
      fd = fd0 { funName = "_copy_List", funMeta = (funMeta fd0) { funOpt = [] } }
      prg = Prog (M.fromList [("List", cursorizedListDDef)]) (M.fromList [("_copy_List", fd)]) Nothing
   in Nothing @=? loopifyCandidateInfoWith True (ddefs prg) fd

case_auto_loopification_rewrites_unannotated_cursorized_fast_path :: Assertion
case_auto_loopification_rewrites_unannotated_cursorized_fast_path =
  let prg = withCountedProducer "List" (withoutFunOpts "fastAdd1ListMut" cursorizedMutableLoopifyProg)
      Prog {fundefs = fds} = runnerWithCountsAuto prg
      fd = fds M.! "fastAdd1ListMut"
   in do
        assertBool "expected auto loopification to emit ForE" (containsL3For (funBody fd))
        assertBool "expected auto loopification to stamp CanVectorize metadata" (CanVectorize `elem` funOpt (funMeta fd))

case_auto_loopification_requires_counted_producer :: Assertion
case_auto_loopification_requires_counted_producer =
  let Prog {fundefs = fds} = runnerWithCountsAuto (withoutFunOpts "fastAdd1ListMut" cursorizedMutableLoopifyProg)
      fd = fds M.! "fastAdd1ListMut"
   in assertBool
        "expected auto loopification to stay recursive without scalar-count producer metadata"
        (not (containsL3For (funBody fd)))

case_requires_fully_factored_layout :: Assertion
case_requires_fully_factored_layout =
  let prg = listProg Linear [CanVectorize]
      fd = (fundefs prg) M.! "add1List"
   in Nothing @=? loopifyCandidateInfo (ddefs prg) fd

case_rejects_mixed_tycons :: Assertion
case_rejects_mixed_tycons =
  let fd = (fundefs mixedProg) M.! "mix"
   in Nothing @=? loopifyCandidateInfo (ddefs mixedProg) fd

case_first_milestone_is_identity :: Assertion
case_first_milestone_is_identity =
  let prg = listProg FullyFactored [CanVectorize]
   in prg @=? runner prg

case_rewrites_supported_cursorized_fast_path :: Assertion
case_rewrites_supported_cursorized_fast_path =
  let Prog {fundefs = fds} = runnerWithCounts (withCountedProducer "List" cursorizedLoopifyProg)
      fd = fds M.! "fastAdd1List"
   in do
        assertBool "expected loopified fast path to contain ForE" (containsL3For (funBody fd))
        assertBool "expected loopified fast path to read scalar-count metadata" (containsScalarCountRead (funBody fd))
        assertBool "expected loopified fast path to copy dcon tags from input" (containsWriteTagPacked (funBody fd))

case_rewrites_supported_mutable_cursorized_fast_path :: Assertion
case_rewrites_supported_mutable_cursorized_fast_path =
  let Prog {fundefs = fds} = runnerWithCounts (withCountedProducer "List" cursorizedMutableLoopifyProg)
      fd = fds M.! "fastAdd1ListMut"
  in do
        assertBool "expected mutable loopified fast path to contain ForE" (containsL3For (funBody fd))
        3 @=? countL3For (funBody fd)
        assertBool "expected mutable loopified fast path to contain WhileCursor" (containsL3WhileCursor (funBody fd))
        assertBool "expected mutable loopified fast path to read scalar-count metadata" (containsScalarCountRead (funBody fd))
        assertBool "expected mutable loopified fast path to write scalar-count metadata" (containsScalarCountWrite (funBody fd))
        assertBool "expected mutable loopified fast path to copy dcon tags from input" (containsWriteTagPacked (funBody fd))

case_can_disable_scalar_loop_fusion :: Assertion
case_can_disable_scalar_loop_fusion =
  let Prog {fundefs = fds} = runnerWithCountsNoFusion (withCountedProducer "List" cursorizedMutableLoopifyProg)
      fd = fds M.! "fastAdd1ListMut"
   in do
        assertBool "expected mutable loopified fast path to still contain ForE" (containsL3For (funBody fd))
        3 @=? countL3For (funBody fd)
        assertBool "expected mutable loopified fast path to still read scalar-count metadata" (containsScalarCountRead (funBody fd))

case_post_selective_loop_fusion_fuses_scalar_loops :: Assertion
case_post_selective_loop_fusion_fuses_scalar_loops =
  let Prog {fundefs = fds} = runnerWithCountsThenFusion (withCountedProducer "List" cursorizedMutableLoopifyProg)
      fd = fds M.! "fastAdd1ListMut"
   in do
        assertBool "expected post-selective loop fusion to preserve ForE loops" (containsL3For (funBody fd))
        2 @=? countL3For (funBody fd)

case_rewrites_mutable_fast_path_with_extra_cursor_array_argument :: Assertion
case_rewrites_mutable_fast_path_with_extra_cursor_array_argument =
  let Prog {fundefs = fds} = runnerWithCounts (withCountedProducer "List" cursorizedMutableLoopifyExtraCursorProg)
      fd = fds M.! "fastAdd1ListMutExtra"
   in do
        assertBool "expected extra cursor-array argument not to block loopification" (containsL3For (funBody fd))
        assertBool "expected loopified fast path to read scalar-count metadata" (containsScalarCountRead (funBody fd))

case_detects_parent_child_dependency_in_primitive_rhs :: Assertion
case_detects_parent_child_dependency_in_primitive_rhs =
  let body =
        LetE
          ("child_sum", [], IntTy, AppE "sumList" TailModuloCons [] [VarE "inEnds", VarE "outEnds", VarE "outCurs", VarE "inCurs"])
          (LetE
             ("total", [], IntTy, PrimAppE AddP [VarE "child_sum", LitE 1])
             (VarE "total"))
   in assertBool "expected primitive use of child result to be rejected" $
        hasParentChildDependency "sumList" body

case_rejects_parent_child_dependent_traversal :: Assertion
case_rejects_parent_child_dependent_traversal =
  let Prog {fundefs = fds} = runnerWithCounts (withCountedProducer "List" cursorizedMutableParentChildDependentProg)
      fd = fds M.! "badParentChildMut"
   in assertBool
        "expected parent-child dependent traversal to remain recursive"
        (not (containsL3For (funBody fd)))

case_rewrites_supported_mutable_tree_fast_path :: Assertion
case_rewrites_supported_mutable_tree_fast_path =
  let Prog {fundefs = fds} = runnerWithCounts (withCountedProducer "Tree" cursorizedMutableTreeLoopifyProg)
      fd = fds M.! "fastAdd1TreeMut"
  in do
        assertBool "expected mutable tree loopified fast path to contain ForE" (containsL3For (funBody fd))
        assertBool "expected mutable tree loopified fast path to contain WhileCursor" (containsL3WhileCursor (funBody fd))
        assertBool "expected mutable tree loopified fast path to read scalar-count metadata" (containsScalarCountRead (funBody fd))
        assertBool "expected mutable tree loopified fast path to write scalar-count metadata" (containsScalarCountWrite (funBody fd))
        assertBool "expected mutable tree loopified fast path to copy dcon tags from input" (containsWriteTagPacked (funBody fd))

case_rewrites_realistic_mutable_cursorized_fast_path :: Assertion
case_rewrites_realistic_mutable_cursorized_fast_path =
  let Prog {fundefs = fds} = runnerWithCounts (withCountedProducer "List" cursorizedRealisticMutableLoopifyProg)
      fd = fds M.! "fastAdd1ListRealisticMut"
   in do
        assertBool "expected realistic mutable loopified fast path to contain ForE" (containsL3For (funBody fd))
        assertBool "expected realistic mutable loopified fast path to contain WhileCursor" (containsL3WhileCursor (funBody fd))
        assertBool "expected realistic mutable loopified fast path to read scalar-count metadata" (containsScalarCountRead (funBody fd))
        assertBool "expected realistic mutable loopified fast path to copy dcon tags from input" (containsWriteTagPacked (funBody fd))

case_lower_supports_footer_counts_and_for_loops :: Assertion
case_lower_supports_footer_counts_and_for_loops =
  let T.Prog {T.fundefs = [fd]} = runLowering loopIrProg
      prims = collectTailPrims (T.funBody fd)
   in do
        assertBool "expected scalar-count get primitive" (T.ScalarCountGet `elem` prims)
        assertBool "expected first-footer primitive" (T.ScalarCountFirstFooter `elem` prims)
        assertBool "expected next-footer primitive" (T.ScalarCountNextFooter `elem` prims)
        assertBool "expected lowered for-loop tail" (containsForLoop (T.funBody fd))

loopifyTraversalsTests :: TestTree
loopifyTraversalsTests = $(testGroupGenerator)
