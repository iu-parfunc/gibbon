{-# LANGUAGE TemplateHaskell #-}

module ScalarCountPropagation
  ( scalarCountPropagationTests
  ) where

import qualified Data.Map as M

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Gibbon.Common
import Gibbon.DynFlags
import Gibbon.Language
import qualified Gibbon.L3.Syntax as L3
import Gibbon.Passes.ScalarCountPropagation

runnerEnabled :: L3.Prog3 -> L3.Prog3
runnerEnabled prg =
  fst $
    runPassM
      (defaultConfig {dynflags = gopt_set Opt_StoreScalarFieldCounts (dynflags defaultConfig)})
      0
      (propagateScalarCounts prg)

runnerDisabled :: L3.Prog3 -> L3.Prog3
runnerDisabled prg =
  fst $ defaultPackedRunPassM $ propagateScalarCounts prg

case_disabled_by_default :: Assertion
case_disabled_by_default =
  countCopies (runnerDisabled propagationProg) @?= 0

case_inserts_one_copy_after_producer_call :: Assertion
case_inserts_one_copy_after_producer_call =
  countCopies (runnerEnabled propagationProg) @?= 1

case_does_not_rewrite_recursive_body :: Assertion
case_does_not_rewrite_recursive_body =
  case M.lookup "producer" (fundefs (runnerEnabled propagationProg)) of
    Just fd -> countCopiesExp (funBody fd) @?= 0
    Nothing -> assertFailure "missing producer function"

propagationProg :: L3.Prog3
propagationProg =
  L3.Prog
    (M.fromList [("List", listDDef)])
    (M.fromList [("producer", producerFun)])
    (Just (mainBody, L3.ProdTy []))

listDDef :: L3.DDef3
listDDef =
  DDef
    { tyName = "List"
    , tyArgs = []
    , dataCons =
        [ ("Nil", [])
        , ("Cons", [(False, L3.IntTy), (False, L3.FloatTy), (True, L3.PackedTy "List" ())])
        ]
    , memLayout = FullyFactored
    }

-- | A cursorized, shape-preserving SoA map: each constructor branch writes its
-- own tag exactly once and recurses once per packed field.  Matching the SoA
-- cursor ABI alone is deliberately not enough for the pass to propagate
-- counts, so the fixture has to be a real map.
producerFun :: L3.FunDef3
producerFun =
  L3.FunDef
    "producer"
    ["inEnds", "outEnds", "outCurs", "inCurs"]
    ( replicate 4 (L3.CursorArrayTy 3)
    , L3.ProdTy []
    )
    producerBody
    (FunMeta Rec NoInline False [])

producerBody :: L3.Exp3
producerBody =
  L3.mkLets
    [ ("in_dcon_loc", [], L3.MutCursorTy, L3.Ext $ L3.AddrOfCursor (L3.Ext $ L3.IndexCursorArray "inCurs" 0))
    , ("out_dcon_loc", [], L3.MutCursorTy, L3.Ext $ L3.AddrOfCursor (L3.Ext $ L3.IndexCursorArray "outCurs" 0))
    , ("in_int_loc", [], L3.MutCursorTy, L3.Ext $ L3.AddrOfCursor (L3.Ext $ L3.IndexCursorArray "inCurs" 1))
    , ("out_int_loc", [], L3.MutCursorTy, L3.Ext $ L3.AddrOfCursor (L3.Ext $ L3.IndexCursorArray "outCurs" 1))
    , ("dcur", [], L3.CursorTy, L3.Ext $ L3.DerefMutCursor "in_dcon_loc")
    ]
    (L3.CaseE
      (L3.VarE "dcur")
      [ ("Nil", [], tagOnlyBranch "Nil")
      , ("Cons", [], consBranch)
      ])
  where
    tagOnlyBranch dcon =
      L3.mkLets
        [ ("nil_out_dcon_cur", [], L3.CursorTy, L3.Ext $ L3.DerefMutCursor "out_dcon_loc")
        , ("nil_write_tag", [], L3.CursorTy, L3.Ext $ L3.WriteTag dcon "nil_out_dcon_cur")
        , ("nil_bump_in", [], L3.ProdTy [], L3.Ext $ L3.BumpCursorMutable "in_dcon_loc" (L3.LitE 1))
        , ("nil_bump_out", [], L3.ProdTy [], L3.Ext $ L3.BumpCursorMutable "out_dcon_loc" (L3.LitE 1))
        ]
        (L3.MkProdE [])

    consBranch =
      L3.mkLets
        [ ("in_int_cur", [], L3.CursorTy, L3.Ext $ L3.DerefMutCursor "in_int_loc")
        , ("read_int_pair", [], L3.ProdTy [L3.IntTy, L3.CursorTy], L3.Ext $ L3.ReadScalar L3.IntS "in_int_cur")
        , ("i", [], L3.IntTy, L3.ProjE 0 (L3.VarE "read_int_pair"))
        , ("plus1", [], L3.IntTy, L3.PrimAppE AddP [L3.VarE "i", L3.LitE 1])
        , ("out_int_cur", [], L3.CursorTy, L3.Ext $ L3.DerefMutCursor "out_int_loc")
        , ("write_int", [], L3.CursorTy, L3.Ext $ L3.WriteScalar L3.IntS "out_int_cur" (L3.VarE "plus1"))
        , ("out_dcon_cur", [], L3.CursorTy, L3.Ext $ L3.DerefMutCursor "out_dcon_loc")
        , ("write_tag", [], L3.CursorTy, L3.Ext $ L3.WriteTag "Cons" "out_dcon_cur")
        , ("bump_int_in", [], L3.ProdTy [], L3.Ext $ L3.BumpCursorMutable "in_int_loc" (L3.LitE 8))
        , ("bump_int_out", [], L3.ProdTy [], L3.Ext $ L3.BumpCursorMutable "out_int_loc" (L3.LitE 8))
        , ("bump_dcon_in", [], L3.ProdTy [], L3.Ext $ L3.BumpCursorMutable "in_dcon_loc" (L3.LitE 1))
        , ("bump_dcon_out", [], L3.ProdTy [], L3.Ext $ L3.BumpCursorMutable "out_dcon_loc" (L3.LitE 1))
        , ("recur", [], L3.ProdTy [], L3.AppE "producer" TailModuloCons [] (map L3.VarE ["inEnds", "outEnds", "outCurs", "inCurs"]))
        ]
        (L3.MkProdE [])

mainBody :: L3.Exp3
mainBody =
  L3.LetE
    ("call", [], L3.ProdTy [], L3.AppE "producer" UnknownTailType [] args)
    (L3.VarE "call")
  where
    args = map L3.VarE ["inEnds", "outEnds", "outCurs", "inCurs"]

countCopies :: L3.Prog3 -> Int
countCopies L3.Prog{fundefs, mainExp} =
  sum (map (countCopiesExp . funBody) (M.elems fundefs)) +
  maybe 0 (countCopiesExp . fst) mainExp

countCopiesExp :: L3.Exp3 -> Int
countCopiesExp ex =
  case ex of
    L3.LetE (_, _, _, rhs) bod -> countCopiesExp rhs + countCopiesExp bod
    L3.IfE a b c -> sum (map countCopiesExp [a,b,c])
    L3.CaseE scrt brs ->
      countCopiesExp scrt + sum [ countCopiesExp rhs | (_, _, rhs) <- brs ]
    L3.MkProdE ls -> sum (map countCopiesExp ls)
    L3.ProjE _ e -> countCopiesExp e
    L3.PrimAppE _ args -> sum (map countCopiesExp args)
    L3.TimeIt e _ _ -> countCopiesExp e
    L3.WithArenaE _ e -> countCopiesExp e
    L3.SpawnE _ _ args -> sum (map countCopiesExp args)
    L3.MapE (_, _, rhs) bod -> countCopiesExp rhs + countCopiesExp bod
    L3.FoldE (_, _, rhs1) (_, _, rhs2) bod ->
      sum (map countCopiesExp [rhs1, rhs2, bod])
    L3.DataConE _ _ args -> sum (map countCopiesExp args)
    L3.Ext ext -> countCopiesExt ext
    _ -> 0

countCopiesExt :: L3.E3Ext () L3.Ty3 -> Int
countCopiesExt ext =
  case ext of
    L3.ScalarCountCopyAll{} -> 1
    L3.ForE _ bound bod -> countCopiesExp bound + countCopiesExp bod
    L3.WhileCursor _ bod -> countCopiesExp bod
    L3.WriteScalar _ _ rhs -> countCopiesExp rhs
    L3.WriteTagPacked _ rhs -> countCopiesExp rhs
    L3.WriteTaggedCursor _ rhs -> countCopiesExp rhs
    L3.WriteCursorMutable _ rhs -> countCopiesExp rhs
    L3.WriteList _ rhs _ -> countCopiesExp rhs
    L3.WriteVector _ rhs _ -> countCopiesExp rhs
    L3.AddCursor _ rhs -> countCopiesExp rhs
    L3.BumpCursorMutable _ rhs -> countCopiesExp rhs
    L3.AddrOfCursor rhs -> countCopiesExp rhs
    L3.LetAvail _ bod -> countCopiesExp bod
    L3.Assert rhs -> countCopiesExp rhs
    L3.WriteCursorSelectiveIndirection _ _ _ mask -> countCopiesExp mask
    _ -> 0

scalarCountPropagationTests :: TestTree
scalarCountPropagationTests = testGroup "ScalarCountPropagation" [tests]

tests :: TestTree
tests = $(testGroupGenerator)
