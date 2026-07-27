{-# LANGUAGE TemplateHaskell #-}

module VectorizeTraversals
  ( vectorizeTraversalsTests
  ) where

import qualified Data.Map as M

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Gibbon.Common
import Gibbon.DynFlags
import Gibbon.Language
import qualified Gibbon.L3.Syntax as L3
import Gibbon.Passes.VectorizeTraversals

runnerInt32 :: L3.Prog3 -> L3.Prog3
runnerInt32 prg =
  fst $
    runPassM
      (defaultConfig {dynflags = gopt_set Opt_Int32 (gopt_set Opt_EnableVectorization (dynflags defaultConfig))})
      0
      (vectorizeTraversals prg)

runner64 :: L3.Prog3 -> L3.Prog3
runner64 prg =
  fst $
    runPassM
      (defaultConfig {dynflags = gopt_set Opt_EnableVectorization (dynflags defaultConfig)})
      0
      (vectorizeTraversals prg)

case_int_add_vectorizes_64_bit :: Assertion
case_int_add_vectorizes_64_bit =
  let body = funBodyOf "intAdd64" (runner64 vectorizeProg)
   in do
        countVecAdds body @?= 2
        countVecStores body @?= 2

case_int_select_vectorizes :: Assertion
case_int_select_vectorizes =
  countVecStores (funBodyOf "intSelect" (runnerInt32 vectorizeProg)) @?= 1

case_mixed_int_mask_float_select_stays_scalar :: Assertion
case_mixed_int_mask_float_select_stays_scalar =
  countVecStores (funBodyOf "mixedSelect" (runnerInt32 vectorizeProg)) @?= 0

case_unsupported_write_keeps_whole_loop_scalar :: Assertion
case_unsupported_write_keeps_whole_loop_scalar =
  countVecStores (funBodyOf "partialUnsupported" (runnerInt32 vectorizeProg)) @?= 0

-- Both arms of a `VecSelect` are evaluated, so a division the scalar program
-- guarded must not be speculated: leave the loop scalar.
case_guarded_division_stays_scalar :: Assertion
case_guarded_division_stays_scalar =
  countVecStores (funBodyOf "guardedDiv" (runner64 vectorizeProg)) @?= 0

-- A loop-invariant division must not be hoisted in front of the loop (it would
-- run even for a zero trip count); it stays inside the loop as a vector op.
case_invariant_division_is_not_hoisted :: Assertion
case_invariant_division_is_not_hoisted =
  let body = funBodyOf "invariantDiv" (runner64 vectorizeProg)
   in do
        countHoistedPartialPrims body @?= 0
        countVecStores body @?= 2

vectorizeProg :: L3.Prog3
vectorizeProg =
  L3.Prog
    M.empty
    (M.fromList
       [ ("intAdd64", intAdd64Fun)
       , ("intSelect", intSelectFun)
       , ("mixedSelect", mixedSelectFun)
       , ("partialUnsupported", partialUnsupportedFun)
       , ("guardedDiv", guardedDivFun)
       , ("invariantDiv", invariantDivFun)
       ])
    Nothing

guardedDivFun :: L3.FunDef3
guardedDivFun =
  L3.FunDef
    "guardedDiv"
    []
    ([], L3.ProdTy [])
    (loopBody guardedDivLoopBody)
    (FunMeta TailRec NoInline False [CanVectorize])

guardedDivLoopBody :: L3.Exp3
guardedDivLoopBody =
  intWriteLoopBody 8 "guard_in" "guard_out" $
    L3.IfE
      (L3.PrimAppE EqIntP [L3.VarE "x", L3.LitE 0])
      (L3.LitE 0)
      (L3.PrimAppE DivP [L3.VarE "x", L3.VarE "k"])

invariantDivFun :: L3.FunDef3
invariantDivFun =
  L3.FunDef
    "invariantDiv"
    []
    ([], L3.ProdTy [])
    (loopBody invariantDivLoopBody)
    (FunMeta TailRec NoInline False [CanVectorize])

invariantDivLoopBody :: L3.Exp3
invariantDivLoopBody =
  intWriteLoopBody 8 "inv_in" "inv_out" $
    L3.PrimAppE AddP [L3.VarE "x", L3.PrimAppE DivP [L3.VarE "k", L3.VarE "m"]]

intAdd64Fun :: L3.FunDef3
intAdd64Fun =
  L3.FunDef
    "intAdd64"
    []
    ([], L3.ProdTy [])
    (loopBody intAdd64LoopBody)
    (FunMeta TailRec NoInline False [CanVectorize])

intSelectFun :: L3.FunDef3
intSelectFun =
  L3.FunDef
    "intSelect"
    []
    ([], L3.ProdTy [])
    (loopBody intSelectLoopBody)
    (FunMeta TailRec NoInline False [CanVectorize])

mixedSelectFun :: L3.FunDef3
mixedSelectFun =
  L3.FunDef
    "mixedSelect"
    []
    ([], L3.ProdTy [])
    (loopBody mixedSelectLoopBody)
    (FunMeta TailRec NoInline False [CanVectorize])

partialUnsupportedFun :: L3.FunDef3
partialUnsupportedFun =
  L3.FunDef
    "partialUnsupported"
    []
    ([], L3.ProdTy [])
    (loopBody partialUnsupportedLoopBody)
    (FunMeta TailRec NoInline False [CanVectorize])

loopBody :: L3.Exp3 -> L3.Exp3
loopBody body =
  L3.LetE
    ("loop", [], L3.ProdTy [], L3.Ext $ L3.ForE "i" (L3.LitE 8) body)
    (L3.MkProdE [])

intAdd64LoopBody :: L3.Exp3
intAdd64LoopBody =
  intWriteLoopBody 8 "int64_in" "int64_out" (L3.PrimAppE AddP [L3.VarE "x", L3.LitE 1])

intSelectLoopBody :: L3.Exp3
intSelectLoopBody =
  intWriteLoopBody 4 "int_in" "int_out" intSelectExpr

intWriteLoopBody :: Int -> Var -> Var -> L3.Exp3 -> L3.Exp3
intWriteLoopBody bumpBytes inRef outRef writeExpr =
  L3.mkLets
    [ ("in_cur", [], L3.CursorTy, L3.Ext $ L3.DerefMutCursor inRef)
    , ("read_pair", [], L3.ProdTy [L3.IntTy, L3.CursorTy], L3.Ext $ L3.ReadScalar L3.IntS "in_cur")
    , ("x", [], L3.IntTy, L3.ProjE 0 (L3.VarE "read_pair"))
    , ("out_cur", [], L3.CursorTy, L3.Ext $ L3.DerefMutCursor outRef)
    , ("write", [], L3.CursorTy, L3.Ext $ L3.WriteScalar L3.IntS "out_cur" writeExpr)
    , ("bump_in", [], L3.ProdTy [], L3.Ext $ L3.BumpCursorMutable inRef (L3.LitE bumpBytes))
    , ("bump_out", [], L3.ProdTy [], L3.Ext $ L3.BumpCursorMutable outRef (L3.LitE bumpBytes))
    ]
    (L3.MkProdE [])

intSelectExpr :: L3.Exp3
intSelectExpr =
  L3.IfE
    (L3.PrimAppE EqIntP [L3.VarE "x", L3.LitE 0])
    (L3.PrimAppE AddP [L3.VarE "x", L3.LitE 1])
    (L3.VarE "x")

mixedSelectLoopBody :: L3.Exp3
mixedSelectLoopBody =
  L3.mkLets
    [ ("int_cur", [], L3.CursorTy, L3.Ext $ L3.DerefMutCursor "int_in")
    , ("int_pair", [], L3.ProdTy [L3.IntTy, L3.CursorTy], L3.Ext $ L3.ReadScalar L3.IntS "int_cur")
    , ("x", [], L3.IntTy, L3.ProjE 0 (L3.VarE "int_pair"))
    , ("float_cur", [], L3.CursorTy, L3.Ext $ L3.DerefMutCursor "float_in")
    , ("float_pair", [], L3.ProdTy [L3.FloatTy, L3.CursorTy], L3.Ext $ L3.ReadScalar L3.FloatS "float_cur")
    , ("f", [], L3.FloatTy, L3.ProjE 0 (L3.VarE "float_pair"))
    , ("out_cur", [], L3.CursorTy, L3.Ext $ L3.DerefMutCursor "float_out")
    , ("write", [], L3.CursorTy, L3.Ext $ L3.WriteScalar L3.FloatS "out_cur" mixedSelectExpr)
    , ("bump_int", [], L3.ProdTy [], L3.Ext $ L3.BumpCursorMutable "int_in" (L3.LitE 4))
    , ("bump_float", [], L3.ProdTy [], L3.Ext $ L3.BumpCursorMutable "float_in" (L3.LitE 4))
    , ("bump_out", [], L3.ProdTy [], L3.Ext $ L3.BumpCursorMutable "float_out" (L3.LitE 4))
    ]
    (L3.MkProdE [])

mixedSelectExpr :: L3.Exp3
mixedSelectExpr =
  L3.IfE
    (L3.PrimAppE EqIntP [L3.VarE "x", L3.LitE 0])
    (L3.PrimAppE FAddP [L3.VarE "f", L3.FloatE 1.0])
    (L3.VarE "f")

partialUnsupportedLoopBody :: L3.Exp3
partialUnsupportedLoopBody =
  L3.mkLets
    [ ("good", [], L3.ProdTy [], intWriteLoopBody 4 "partial_int_in" "partial_int_out" (L3.PrimAppE AddP [L3.VarE "x", L3.LitE 1]))
    , ("float_cur", [], L3.CursorTy, L3.Ext $ L3.DerefMutCursor "partial_float_in")
    , ("float_pair", [], L3.ProdTy [L3.FloatTy, L3.CursorTy], L3.Ext $ L3.ReadScalar L3.FloatS "float_cur")
    , ("f", [], L3.FloatTy, L3.ProjE 0 (L3.VarE "float_pair"))
    , ("float_out_cur", [], L3.CursorTy, L3.Ext $ L3.DerefMutCursor "partial_float_out")
    , ("bad_write", [], L3.CursorTy, L3.Ext $ L3.WriteScalar L3.FloatS "float_out_cur" (L3.PrimAppE EqFloatP [L3.VarE "f", L3.FloatE 0.0]))
    , ("bump_float_in", [], L3.ProdTy [], L3.Ext $ L3.BumpCursorMutable "partial_float_in" (L3.LitE 4))
    , ("bump_float_out", [], L3.ProdTy [], L3.Ext $ L3.BumpCursorMutable "partial_float_out" (L3.LitE 4))
    ]
    (L3.MkProdE [])

funBodyOf :: Var -> L3.Prog3 -> L3.Exp3
funBodyOf fn prg =
  case M.lookup fn (fundefs prg) of
    Just fd -> funBody fd
    Nothing -> error $ "Missing function in test program: " ++ sdoc fn

countVecStores :: L3.Exp3 -> Int
countVecStores = countExt p
  where
    p L3.VecStore{} = True
    p _ = False

countVecAdds :: L3.Exp3 -> Int
countVecAdds = countExt p
  where
    p L3.VecAdd{} = True
    p _ = False

-- | Count partial (potentially trapping) primitives evaluated outside every
-- loop body, i.e. the positions a loop with a zero trip count would still run.
countHoistedPartialPrims :: L3.Exp3 -> Int
countHoistedPartialPrims ex =
  case ex of
    L3.LetE (_, _, _, rhs) bod -> countHoistedPartialPrims rhs + countHoistedPartialPrims bod
    L3.Ext (L3.ForE _ bound _) -> countHoistedPartialPrims bound
    L3.Ext (L3.WhileCursor _ bod) -> countHoistedPartialPrims bod
    -- The pass's own `bound / stride` and `bound % stride` binds divide by a
    -- non-zero literal and can never trap; they are not what this counts.
    L3.PrimAppE p args
      | p `elem` [DivP, ModP, FDivP] && not (dividesByNonZeroLit args) ->
          1 + sum (map countHoistedPartialPrims args)
      | otherwise -> sum (map countHoistedPartialPrims args)
    L3.IfE a b c -> sum (map countHoistedPartialPrims [a, b, c])
    L3.MkProdE ls -> sum (map countHoistedPartialPrims ls)
    L3.ProjE _ e -> countHoistedPartialPrims e
    _ -> 0

dividesByNonZeroLit :: [L3.Exp3] -> Bool
dividesByNonZeroLit [_, L3.LitE n] = n /= 0
dividesByNonZeroLit _ = False

countExt :: (L3.E3Ext () L3.Ty3 -> Bool) -> L3.Exp3 -> Int
countExt p ex =
  case ex of
    L3.LetE (_, _, _, rhs) bod -> countExt p rhs + countExt p bod
    L3.IfE a b c -> sum (map (countExt p) [a, b, c])
    L3.CaseE scrt brs -> countExt p scrt + sum [countExt p rhs | (_, _, rhs) <- brs]
    L3.AppE _ _ _ args -> sum (map (countExt p) args)
    L3.PrimAppE _ args -> sum (map (countExt p) args)
    L3.MkProdE ls -> sum (map (countExt p) ls)
    L3.ProjE _ e -> countExt p e
    L3.DataConE _ _ args -> sum (map (countExt p) args)
    L3.TimeIt e _ _ -> countExt p e
    L3.WithArenaE _ e -> countExt p e
    L3.SpawnE _ _ args -> sum (map (countExt p) args)
    L3.MapE (_, _, e1) e2 -> countExt p e1 + countExt p e2
    L3.FoldE (_, _, e1) (_, _, e2) e3 -> sum (map (countExt p) [e1, e2, e3])
    L3.Ext ext
      | p ext -> 1 + goExt ext
      | otherwise -> goExt ext
    _ -> 0
  where
    goExt ext =
      case ext of
        L3.ForE _ bound bod -> countExt p bound + countExt p bod
        L3.WhileCursor _ bod -> countExt p bod
        L3.WhileCursorEnd _ _ bod -> countExt p bod
        L3.WriteScalar _ _ rhs -> countExt p rhs
        L3.WriteTagPacked _ rhs -> countExt p rhs
        L3.WriteTaggedCursor _ rhs -> countExt p rhs
        L3.WriteCursorMutable _ rhs -> countExt p rhs
        L3.WriteList _ rhs _ -> countExt p rhs
        L3.WriteVector _ rhs _ -> countExt p rhs
        L3.AddCursor _ rhs -> countExt p rhs
        L3.BumpCursorMutable _ rhs -> countExt p rhs
        L3.AddrOfCursor rhs -> countExt p rhs
        L3.LetAvail _ bod -> countExt p bod
        L3.Assert rhs -> countExt p rhs
        L3.WriteCursorSelectiveIndirection _ _ _ mask -> countExt p mask
        L3.VecBroadcast _ _ rhs -> countExt p rhs
        L3.VecAdd _ _ a b -> countExt p a + countExt p b
        L3.VecSub _ _ a b -> countExt p a + countExt p b
        L3.VecMul _ _ a b -> countExt p a + countExt p b
        L3.VecDiv _ _ a b -> countExt p a + countExt p b
        L3.VecMod _ _ a b -> countExt p a + countExt p b
        L3.VecEq _ _ a b -> countExt p a + countExt p b
        L3.VecSelect _ _ m a b -> sum (map (countExt p) [m, a, b])
        L3.VecStore _ _ _ rhs -> countExt p rhs
        _ -> 0

vectorizeTraversalsTests :: TestTree
vectorizeTraversalsTests = testGroup "VectorizeTraversals" [tests]

tests :: TestTree
tests = $(testGroupGenerator)
