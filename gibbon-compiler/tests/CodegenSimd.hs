{-# LANGUAGE TemplateHaskell #-}

-- | Backend-level tests for the SSE2 SIMD lowering in "Gibbon.Passes.Codegen".
--
-- These drive `codegenProg` end-to-end on tiny hand-built L4 programs, so they
-- observe the C text and the lowering errors a real compile would see.
module CodegenSimd
  ( codegenSimdTests
  ) where

import Control.Exception (ErrorCall, evaluate, try)
import qualified Data.List as L
import qualified Data.Map as M

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Gibbon.Common
import Gibbon.DynFlags
import Gibbon.Language (FunMeta(..), FunRec(..), FunInline(..))
import qualified Gibbon.L3.Syntax as L3
import Gibbon.L4.Syntax
import Gibbon.Passes.Codegen (codegenProg)

--------------------------------------------------------------------------------
-- Harness

int32Config :: Config
int32Config = defaultConfig {dynflags = gopt_set Opt_Int32 (dynflags defaultConfig)}

config64 :: Config
config64 = defaultConfig

-- | An otherwise empty L4 program whose single function body is @tal@.
progWith :: Tail -> Prog
progWith tal =
  Prog
    { infoTable = M.empty
    , symbolTable = M.empty
    , fundefs =
        [ FunDecl
            { funName = "simd_probe"
            , funArgs = []
            , funRetTy = ProdTy []
            , funBody = tal
            , isPure = False
            , funMeta = FunMeta NotRec NoInline False []
            }
        ]
    , mainExp = Nothing
    }

emptyProg :: Prog
emptyProg = (progWith (RetValsT [])) {fundefs = []}

-- | Fully force the generated C so lowering errors surface here.
genC :: Config -> Prog -> IO (Either ErrorCall String)
genC cfg prg = do
  r <- try (codegenProg cfg prg >>= \s -> evaluate (length s) >> pure s)
  pure r

genC' :: Config -> Prog -> IO String
genC' cfg prg = do
  r <- genC cfg prg
  case r of
    Right s -> pure s
    Left e -> assertFailure ("codegenProg unexpectedly failed: " ++ show e)

-- | A one-op body: broadcast a literal into a @(scalar, lanes)@ register.
broadcastBody :: L3.Scalar -> Int -> Tail
broadcastBody scalar lanes =
  LetPrimCallT
    [("simd_v", SimdTy (scalarToTy scalar) lanes)]
    (VecBroadcast scalar lanes)
    [IntTriv 1]
    (RetValsT [])

--------------------------------------------------------------------------------
-- F1: the int64x2 scalar-spill helpers must declare 64-bit spill arrays.
--
-- `_mm_storeu_si128` writes 16 bytes into `av` / `bv`.  Spelling them `GibInt`
-- made them 8 bytes wide under `--int32` (an 8-byte stack overflow per call);
-- the helper block is emitted unconditionally into every generated .c, in both
-- widths, so pin the type in both.

spillHelpers :: [String]
spillHelpers =
  ["gib_vec_mul_int64x2", "gib_vec_div_int64x2", "gib_vec_mod_int64x2", "gib_vec_eq_int64x2"]

-- | Body of the named `static inline` helper in the generated C.
helperBody :: String -> String -> String
helperBody nam src =
  case L.findIndex (nam `L.isInfixOf`) ls of
    Nothing -> error $ "helper not found in generated C: " ++ nam
    Just i -> unlines (take 8 (drop i ls))
  where
    ls = lines src

case_int64x2_spill_arrays_are_64_bit_under_int32 :: Assertion
case_int64x2_spill_arrays_are_64_bit_under_int32 = do
  src <- genC' int32Config emptyProg
  mapM_
    (\h -> do
       let b = helperBody h src
       assertBool (h ++ ": spill arrays must be int64_t, got:\n" ++ b)
         ("int64_t av[2], bv[2];" `L.isInfixOf` b)
       assertBool (h ++ ": spill arrays must not be GibInt (8 bytes under --int32), got:\n" ++ b)
         (not ("GibInt av[2]" `L.isInfixOf` b)))
    spillHelpers

case_int64x2_spill_arrays_are_64_bit_at_64_bit :: Assertion
case_int64x2_spill_arrays_are_64_bit_at_64_bit = do
  src <- genC' config64 emptyProg
  mapM_
    (\h -> do
       let b = helperBody h src
       assertBool (h ++ ": spill arrays must be int64_t, got:\n" ++ b)
         ("int64_t av[2], bv[2];" `L.isInfixOf` b)
       assertBool (h ++ ": spill arrays must not be GibInt, got:\n" ++ b)
         (not ("GibInt av[2]" `L.isInfixOf` b)))
    spillHelpers

--------------------------------------------------------------------------------
-- F2: `ITERS:` / `SIZE:` are printed from GibInt-typed RTS getters, so the
-- conversion specifier has to follow the active integer width.

timedProg :: Prog
timedProg =
  progWith
    (LetTimedT
       True
       []
       (RetValsT [])
       (RetValsT []))

-- | Just the ITERS:/SIZE: printf lines, so a failure report is readable.
paramPrints :: String -> String
paramPrints src =
  unlines (filter (\l -> "ITERS: " `L.isInfixOf` l || "SIZE: " `L.isInfixOf` l) (lines src))

case_iters_and_size_use_percent_d_under_int32 :: Assertion
case_iters_and_size_use_percent_d_under_int32 = do
  src <- genC' int32Config timedProg
  let got = paramPrints src
  assertBool ("ITERS: must print with %d under --int32, got:\n" ++ got)
    ("printf(\"ITERS: %d\\n\"" `L.isInfixOf` src)
  assertBool ("SIZE: must print with %d under --int32, got:\n" ++ got)
    ("printf(\"SIZE: %d\\n\"" `L.isInfixOf` src)
  assertBool ("no %ld may remain on the ITERS/SIZE prints under --int32, got:\n" ++ got)
    (not ("ITERS: %ld" `L.isInfixOf` src) && not ("SIZE: %ld" `L.isInfixOf` src))

case_iters_and_size_use_percent_ld_at_64_bit :: Assertion
case_iters_and_size_use_percent_ld_at_64_bit = do
  src <- genC' config64 timedProg
  let got = paramPrints src
  assertBool ("ITERS: must still print with %ld at 64-bit, got:\n" ++ got)
    ("printf(\"ITERS: %ld\\n\"" `L.isInfixOf` src)
  assertBool ("SIZE: must still print with %ld at 64-bit, got:\n" ++ got)
    ("printf(\"SIZE: %ld\\n\"" `L.isInfixOf` src)

--------------------------------------------------------------------------------
-- F3: lowering must reject a SIMD op whose lanes do not tile a 128-bit
-- register at the active integer width.

-- | `(IntS, 2)` selects the `int64x2` helper family.  Under `--int32` a lane is
-- 4 bytes, so 2 lanes cover 8 of the register's 16 bytes: the helpers would
-- spill past the caller's buffer and the cursor stride would be wrong.  This is
-- exactly the combination that only equation *ordering* in `vectorLanes` keeps
-- unreachable today, so lowering must refuse it rather than emit it.
case_int64x2_is_rejected_under_int32 :: Assertion
case_int64x2_is_rejected_under_int32 = do
  r <- genC int32Config (progWith (broadcastBody L3.IntS 2))
  case r of
    Left e ->
      assertBool ("error must explain the 128-bit register invariant, got: " ++ show e)
        ("128-bit register" `L.isInfixOf` show e)
    Right src ->
      assertFailure $
        "codegen accepted (IntS,2) under --int32 and emitted:\n" ++
        unlines (filter ("gib_vec_broadcast_int64x2(" `L.isInfixOf`) (lines src))

-- | The mirror image: 4 lanes of 8-byte `GibInt` is a 256-bit vector, which
-- this SSE2 backend cannot express either.
case_int32x4_is_rejected_at_64_bit :: Assertion
case_int32x4_is_rejected_at_64_bit = do
  r <- genC config64 (progWith (broadcastBody L3.IntS 4))
  case r of
    Left e ->
      assertBool ("error must explain the 128-bit register invariant, got: " ++ show e)
        ("128-bit register" `L.isInfixOf` show e)
    Right _ -> assertFailure "codegen accepted (IntS,4) at 64-bit"

-- | And every combination the vectorizer can actually produce must still lower.
-- This is the false-positive guard for the assertion above.
case_all_currently_valid_lane_counts_still_lower :: Assertion
case_all_currently_valid_lane_counts_still_lower = do
  mapM_
    (\(cfg, cfgName, scalar, lanes) -> do
       r <- genC cfg (progWith (broadcastBody scalar lanes))
       case r of
         Right _ -> pure ()
         Left e ->
           assertFailure $
             "valid combination rejected: " ++ cfgName ++ " " ++ show (scalar, lanes) ++ ": " ++ show e)
    [ (config64, "64-bit", L3.IntS, 2)
    , (int32Config, "--int32", L3.IntS, 4)
    , (config64, "64-bit", L3.SymS, 2)
    , (int32Config, "--int32", L3.SymS, 2)
    , (config64, "64-bit", L3.FloatS, 4)
    , (int32Config, "--int32", L3.FloatS, 4)
    , (config64, "64-bit", L3.CharS, 16)
    , (int32Config, "--int32", L3.CharS, 16)
    , (config64, "64-bit", L3.BoolS, 16)
    , (int32Config, "--int32", L3.BoolS, 16)
    ]

--------------------------------------------------------------------------------

codegenSimdTests :: TestTree
codegenSimdTests = testGroup "CodegenSimd" [tests]

tests :: TestTree
tests = $(testGroupGenerator)
