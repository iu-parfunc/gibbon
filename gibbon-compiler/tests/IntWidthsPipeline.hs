{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Narrow-width integers driven through the REAL production pass sequence.
--
-- These tests exist because directly-constructed L3 IR proves nothing about
-- the passes that actually build it.  Everything here goes through
-- 'Gibbon.Compiler.passesThroughL3' -- the same function 'passes' uses -- so
-- the ordering cannot drift from production.
--
-- They stop at a verified L3 program: nothing here says anything about
-- whether the generated C is correct for a narrow width, which is exercised
-- separately by the Codegen/RTS test suites.
module IntWidthsPipeline (intWidthPipelineTests) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad.State.Strict (evalStateT)
import Data.List (isInfixOf, sort)
import qualified Data.Map as M
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Gibbon.Common
import Gibbon.Compiler (Config(..), Input(..), defaultConfig, parseInput,
                        passesThroughL3, CompileState(..))
import Gibbon.DynFlags
import Gibbon.L1.Syntax
import qualified Gibbon.L1.Typecheck as L1
import Control.Monad.Except (runExcept)
import qualified Gibbon.L3.Syntax as L3
import qualified Gibbon.L3.Typecheck as L3
import qualified Gibbon.L4.Syntax as L4

--------------------------------------------------------------------------------
-- Harness: source text -> production L3
--------------------------------------------------------------------------------

-- | Compile a source string through the production pass sequence, stopping
-- immediately before 'Gibbon.Passes.Lower.lower'.
--
-- 'dflagOpts' are turned on so the same programs can be run down the packed
-- AoS path, the no-RAN path, the loopification path, and so on.
srcToL3With :: [GeneralFlag] -> String -> IO L3.Prog3
srcToL3With dflagOpts src = do
  tmp <- getTemporaryDirectory
  let fp = tmp </> ("gibbon-intwidth-pipe-" ++ show (abs (hashStr (show dflagOpts ++ src))) ++ ".hs")
  writeFile fp src
  let dflags0 = dynflags defaultConfig
      dflags1 = foldl (flip gopt_set) dflags0 dflagOpts
      cfg = defaultConfig { dynflags = dflags1 }
  ((l0, cnt0), _fp') <- parseInput cfg Haskell fp
  removeFile fp
  -- 'v' is the interpreter-value type threaded through CompileState; it is
  -- unused here (result = Nothing), so pin it to keep inference happy.
  evalStateT (passesThroughL3 cfg l0)
             (CompileState { cnt = cnt0, result = Nothing } :: CompileState Var)
  where
    hashStr :: String -> Int
    hashStr = foldl (\acc c -> acc * 33 + fromEnum c) 5381

-- | The ordinary packed (Gibbon2) path.
srcToL3 :: String -> IO L3.Prog3
srcToL3 = srcToL3With [Opt_Packed]

-- | Force an L3 program, catching the internal errors passes raise.
forceL3 :: IO L3.Prog3 -> IO (Either String L3.Prog3)
forceL3 act = do
  r <- try (act >>= \p -> evaluate (length (sdoc p)) >> pure p)
  pure $ case r of
           Left (e :: SomeException) -> Left (show e)
           Right p -> Right p

okL3 :: String -> [GeneralFlag] -> String -> IO L3.Prog3
okL3 lbl flags src = do
  r <- forceL3 (srcToL3With flags src)
  case r of
    Left e  -> assertFailure (lbl ++ ": expected this to reach L3, but it failed:\n" ++ e)
    Right p -> pure p

-- | Expect a source program to fail before reaching L3, and return the error.
badL3 :: String -> [GeneralFlag] -> String -> IO String
badL3 lbl flags src = do
  r <- forceL3 (srcToL3With flags src)
  case r of
    Left e  -> pure e
    Right p -> assertFailure (lbl ++ ": expected this to fail, but it reached L3:\n" ++ sdoc p)

--------------------------------------------------------------------------------
-- Inspecting an L3 program
--------------------------------------------------------------------------------

l3Exps :: L3.Prog3 -> [L3.Exp3]
l3Exps Prog{fundefs,mainExp} =
  concatMap (subExps . funBody) (M.elems fundefs) ++ maybe [] (subExps . fst) mainExp

subExps :: L3.Exp3 -> [L3.Exp3]
subExps ex = ex : rest
  where
    rest =
      case ex of
        LetE (_,_,_,rhs) b -> subExps rhs ++ subExps b
        IfE a b c  -> subExps a ++ subExps b ++ subExps c
        MkProdE ls -> concatMap subExps ls
        ProjE _ e  -> subExps e
        CaseE sc brs -> subExps sc ++ concatMap (\(_,_,r) -> subExps r) brs
        DataConE _ _ args -> concatMap subExps args
        AppE _ _ _ args   -> concatMap subExps args
        PrimAppE _ args   -> concatMap subExps args
        TimeIt e _ _   -> subExps e
        WithArenaE _ e -> subExps e
        SpawnE _ _ args -> concatMap subExps args
        Ext e3 -> extSubExps e3
        _ -> []

extSubExps :: L3.E3Ext () L3.Ty3 -> [L3.Exp3]
extSubExps e =
  case e of
    L3.WriteScalar _ _ rhs -> subExps rhs
    _ -> []

-- | Every scalar descriptor a ReadScalar/WriteScalar node carries.
readScalars, writeScalars :: L3.Prog3 -> [L3.Scalar]
readScalars p  = [ s | Ext (L3.ReadScalar s _)    <- l3Exps p ]
writeScalars p = [ s | Ext (L3.WriteScalar s _ _) <- l3Exps p ]

-- | Integer widths appearing in every let binder of the program.
binderIntWidths :: L3.Prog3 -> [IntWidth]
binderIntWidths Prog{fundefs,mainExp} =
  concatMap (goE . funBody) (M.elems fundefs) ++ maybe [] (goE . fst) mainExp
  where
    goE ex = [ w | LetE (_,_,ty,_) _ <- subExps ex, IntTy w <- [ty] ]

-- | Integer widths in the declared field types of every datatype.
ddefIntWidths :: L3.Prog3 -> [IntWidth]
ddefIntWidths Prog{ddefs} =
  sort [ w | dd <- M.elems ddefs, (_dcon, flds) <- dataCons dd
           , (_boxed, ty) <- flds, IntTy w <- [ty] ]

litWidths :: L3.Prog3 -> [(IntWidth, Integer)]
litWidths p = [ (litWidth ann, n) | LitE ann n <- l3Exps p ]

primWidths :: L3.Prog3 -> [IntWidth]
primWidths p = [ intPrimWidth a | PrimAppE pr _ <- l3Exps p
                                , Just a <- [intPrimAnnOf pr]
                                , isIntArithPrim pr || isIntCmpPrim pr ]

vecScalars :: L3.Prog3 -> [L3.Scalar]
vecScalars p = concatMap f (l3Exps p)
  where
    f (Ext e) =
      case e of
        L3.VecBroadcast s _ _   -> [s]
        L3.VecLoad s _ _        -> [s]
        L3.VecAdd s _ _ _       -> [s]
        L3.VecSub s _ _ _       -> [s]
        L3.VecMul s _ _ _       -> [s]
        L3.VecDiv s _ _ _       -> [s]
        L3.VecMod s _ _ _       -> [s]
        L3.VecCmp s _ _ _ _     -> [s]
        L3.VecSelect s _ _ _ _  -> [s]
        L3.VecStore s _ _ _     -> [s]
        _ -> []
    f _ = []

-- | Run the real L3 typechecker on a production L3 program.
assertL3Typechecks :: String -> L3.Prog3 -> Assertion
assertL3Typechecks lbl p = do
  r <- try (evaluate (length (sdoc (fst (defaultPackedRunPassM (L3.tcProg True p))))))
  case r of
    Left (e :: SomeException) -> assertFailure (lbl ++ ": L3 typechecking failed:\n" ++ show e)
    Right _ -> pure ()

--------------------------------------------------------------------------------
-- Source programs
--------------------------------------------------------------------------------

-- | A packed datatype with all four widths, plus a recursive datatype so the
-- traversal/cursorization paths are exercised.  Does real work: constructs,
-- pattern-matches, same-width arithmetic, comparison, and printing.
mixedSrc :: String
mixedSrc = unlines
  [ "data Mixed = Mixed Int8 Int16 Int32 Int64"
  , ""
  , "data Chain = Nil | Link Int8 Int32 Chain"
  , ""
  , "mkMixed :: Int8 -> Mixed"
  , "mkMixed a = Mixed a 300 70000 5000000000"
  , ""
  , "bumpMixed :: Mixed -> Mixed"
  , "bumpMixed m ="
  , "  case m of"
  , "    Mixed a b c d -> Mixed (a + 1) (b + 2) (c + 3) (d + 4)"
  , ""
  , "firstIsBig :: Mixed -> Bool"
  , "firstIsBig m ="
  , "  case m of"
  , "    Mixed a _b _c _d -> a > 10"
  , ""
  , "mkChain :: Int8 -> Chain"
  , "mkChain n ="
  , "  if n < 1"
  , "  then Nil"
  , "  else Link n 7 (mkChain (n - 1))"
  , ""
  , "sumChain8 :: Chain -> Int8"
  , "sumChain8 c ="
  , "  case c of"
  , "    Nil -> 0"
  , "    Link a _b rst -> a + (sumChain8 rst)"
  , ""
  , "gibbon_main ="
  , "  let m :: Mixed"
  , "      m = bumpMixed (mkMixed 3)"
  , "      c :: Chain"
  , "      c = mkChain 4"
  , "      s :: Int8"
  , "      s = sumChain8 c"
  , "  in if firstIsBig m then 1 else 0"
  ]

-- | The same shape, but with the SoA (fully factored) memory layout.
--
-- This used to write the annotation as
-- @{-# ANN Narrow (Layout \"SoA\") #-}@, which is not a recognized pragma
-- form and -- worse -- silently vanished entirely because it led the file
-- (see 'Gibbon.HaskellFrontend.desugarModule'): the type defaulted to
-- Linear/AoS with no diagnostic, so this test was never exercising SoA at
-- all despite its name. Now that both bugs are fixed (an unrecognized
-- annotation errors loudly; a leading one is no longer dropped), this is
-- genuine SoA -- which currently means it hits the (not yet fixed in this
-- source tree) Cursorize @AfterVariable@ limitation for narrow fields.  Kept
-- named/shaped exactly as it was; see 'Gibbon.Passes.Cursorize'.
soaSrc :: String
soaSrc = unlines
  [ "{-# ANN type Narrow \"Factored\" #-}"
  , "data Narrow = NNil | NCons Int8 Int32 Narrow"
  , ""
  , "mkNarrow :: Int32 -> Narrow"
  , "mkNarrow n ="
  , "  if n < 1"
  , "  then NNil"
  , "  else NCons 2 n (mkNarrow (n - 1))"
  , ""
  , "sumNarrow :: Narrow -> Int32"
  , "sumNarrow xs ="
  , "  case xs of"
  , "    NNil -> 0"
  , "    NCons _a b rst -> b + (sumNarrow rst)"
  , ""
  , "gibbon_main ="
  , "  let xs :: Narrow"
  , "      xs = mkNarrow 5"
  , "      t :: Int32"
  , "      t = sumNarrow xs"
  , "  in 0"
  ]

-- | A 64-bit traversal that the vectorizer should still be eligible for.
wideVecSrc :: String
wideVecSrc = unlines
  [ "data WideList = WNil | WCons Int WideList"
  , ""
  , "mkWide :: Int -> WideList"
  , "mkWide n = if n < 1 then WNil else WCons n (mkWide (n - 1))"
  , ""
  , "addOneWide :: WideList -> WideList"
  , "addOneWide xs ="
  , "  case xs of"
  , "    WNil -> WNil"
  , "    WCons a rst -> WCons (a + 1) (addOneWide rst)"
  , ""
  , "gibbon_main ="
  , "  let xs :: WideList"
  , "      xs = addOneWide (mkWide 10)"
  , "  in 0"
  ]

-- | The same traversal at Int32, which must NOT be vectorized.
narrowVecSrc :: String
narrowVecSrc = unlines
  [ "data NarrowList = NLNil | NLCons Int32 NarrowList"
  , ""
  , "mkNL :: Int32 -> NarrowList"
  , "mkNL n = if n < 1 then NLNil else NLCons n (mkNL (n - 1))"
  , ""
  , "addOneNL :: NarrowList -> NarrowList"
  , "addOneNL xs ="
  , "  case xs of"
  , "    NLNil -> NLNil"
  , "    NLCons a rst -> NLCons (a + 1) (addOneNL rst)"
  , ""
  , "gibbon_main ="
  , "  let xs :: NarrowList"
  , "      xs = addOneNL (mkNL 10)"
  , "  in 0"
  ]

--------------------------------------------------------------------------------
-- Shared assertions
--------------------------------------------------------------------------------

-- | Everything a narrow program must still be true of once it reaches L3.
assertNarrowSurvives :: String -> [IntWidth] -> L3.Prog3 -> Assertion
assertNarrowSurvives lbl expectedFieldWidths p = do
  -- (a) datatype field types keep their widths
  assertBool (lbl ++ ": datatype field widths were " ++ show (ddefIntWidths p)
              ++ ", expected to contain " ++ show expectedFieldWidths)
             (all (`elem` ddefIntWidths p) expectedFieldWidths)
  -- (b) some let binder carries each narrow width
  let bws = binderIntWidths p
  mapM_ (\w -> assertBool (lbl ++ ": no L3 binder of width " ++ show w
                           ++ "; binder widths were " ++ show bws)
                          (w `elem` bws))
        (filter (/= W64) expectedFieldWidths)
  -- (c) every ReadScalar/WriteScalar descriptor is a well-formed scalar, and
  --     the narrow widths show up among them
  let rs = readScalars p ++ writeScalars p
      intWs = [ w | L3.IntS w <- rs ]
  mapM_ (\w -> assertBool (lbl ++ ": no scalar read/write at width " ++ show w
                           ++ "; scalar int widths were " ++ show intWs)
                          (w `elem` intWs))
        (filter (/= W64) expectedFieldWidths)
  -- (d) the program typechecks at L3
  assertL3Typechecks lbl p

--------------------------------------------------------------------------------
-- AoS / packed
--------------------------------------------------------------------------------

case_aos_packed_narrow_reaches_l3 :: Assertion
case_aos_packed_narrow_reaches_l3 = do
  p <- okL3 "aos" [Opt_Packed] mixedSrc
  assertNarrowSurvives "aos" [W8,W16,W32,W64] p

case_aos_no_ran_narrow_reaches_l3 :: Assertion
case_aos_no_ran_narrow_reaches_l3 = do
  p <- okL3 "aos-noRAN" [Opt_Packed, Opt_No_RAN] mixedSrc
  assertNarrowSurvives "aos-noRAN" [W8,W16,W32,W64] p

case_aos_literal_widths_survive :: Assertion
case_aos_literal_widths_survive = do
  p <- okL3 "aos-lits" [Opt_Packed] mixedSrc
  let lws = litWidths p
  mapM_ (\(w,n) -> assertBool ("literal " ++ show n ++ " should be " ++ show w
                               ++ " at L3; literals were " ++ show lws)
                              ((w,n) `elem` lws))
        [(W16, 300), (W32, 70000), (W64, 5000000000)]

case_aos_prim_widths_survive :: Assertion
case_aos_prim_widths_survive = do
  p <- okL3 "aos-prims" [Opt_Packed] mixedSrc
  let pws = primWidths p
  mapM_ (\w -> assertBool ("no width-" ++ show w ++ " integer primitive at L3; "
                           ++ "primitive widths were " ++ show pws)
                          (w `elem` pws))
        [W8, W16, W32]

--------------------------------------------------------------------------------
-- SoA, loopification, selective buffer sharing
--------------------------------------------------------------------------------

case_soa_narrow_reaches_l3 :: Assertion
case_soa_narrow_reaches_l3 = do
  p <- okL3 "soa" [Opt_Packed] soaSrc
  assertNarrowSurvives "soa" [W8,W32] p

case_loopify_narrow_reaches_l3 :: Assertion
case_loopify_narrow_reaches_l3 = do
  p <- okL3 "loopify" [Opt_Packed, Opt_EnableLoopification] soaSrc
  assertNarrowSurvives "loopify" [W8,W32] p

case_selective_narrow_reaches_l3 :: Assertion
case_selective_narrow_reaches_l3 = do
  p <- okL3 "selective" [Opt_Packed, Opt_EnableLoopification, Opt_EnableSelectiveBufferSharing] soaSrc
  assertNarrowSurvives "selective" [W8,W32] p

--------------------------------------------------------------------------------
-- Vectorization policy
--------------------------------------------------------------------------------

-- | A 64-bit traversal keeps whatever vectorization it had.
case_w64_vectorization_still_eligible :: Assertion
case_w64_vectorization_still_eligible = do
  p <- okL3 "w64-vec" [Opt_Packed, Opt_EnableLoopification, Opt_EnableVectorization] wideVecSrc
  let vs = vecScalars p
  assertBool ("every vector node must be a supported scalar; got " ++ show vs)
             (all (\s -> case s of { L3.IntS W64 -> True ; L3.IntS _ -> False ; _ -> True }) vs)
  assertL3Typechecks "w64-vec" p

-- | A narrow traversal must be left scalar: no vector node may carry an
-- unsupported narrow integer scalar, and the fallback must still typecheck.
case_narrow_vectorization_falls_back_to_scalar :: Assertion
case_narrow_vectorization_falls_back_to_scalar = do
  p <- okL3 "narrow-vec" [Opt_Packed, Opt_EnableLoopification, Opt_EnableVectorization] narrowVecSrc
  let vs = vecScalars p
      bad = [ s | s@(L3.IntS w) <- vs, w /= W64 ]
  assertBool ("narrow integer SIMD must not be produced, but found " ++ show bad) (null bad)
  -- and the scalar reads/writes are still there at the right width
  let intWs = [ w | L3.IntS w <- readScalars p ++ writeScalars p ]
  assertBool ("expected W32 scalar reads/writes on the fallback path; got " ++ show intWs)
             (W32 `elem` intWs)
  assertL3Typechecks "narrow-vec" p

--------------------------------------------------------------------------------
-- Size / layout agreement at L3
--------------------------------------------------------------------------------

-- | The scalar descriptors agree with type-driven sizes.
case_scalar_widths_match_type_sizes :: Assertion
case_scalar_widths_match_type_sizes = do
  p <- okL3 "sizes" [Opt_Packed] mixedSrc
  let ws = [ w | L3.IntS w <- readScalars p ++ writeScalars p ]
  mapM_ (\w -> assertBool ("scalar width " ++ show w ++ " should size to "
                           ++ show (intWidthBytes w) ++ " bytes")
                          (sizeOfTy (L3.scalarToTy (L3.IntS w) :: L3.Ty3) == Just (intWidthBytes w)))
        ws
  (1,2,4,8) @=? (intWidthBytes W8, intWidthBytes W16, intWidthBytes W32, intWidthBytes W64)

--------------------------------------------------------------------------------
-- mkScalar/scalarToTy are exact width-preserving inverses on every width,
-- not just W64: no L3 -> L4 width erasure.
--------------------------------------------------------------------------------

-- | Every width lowers through mkScalar/scalarToTy exactly -- no erasure.
-- (Whether generated C is correct for a narrow width is exercised by the
-- Codegen/RTS test suites; this only tests the L4 representation invariant.)
case_scalar_to_ty_exact_on_every_width :: Assertion
case_scalar_to_ty_exact_on_every_width =
  mapM_ (\w -> L4.IntTy w @=? L4.scalarToTy (L3.IntS w))
        [W8, W16, W32, W64]

case_mk_scalar_exact_on_every_width :: Assertion
case_mk_scalar_exact_on_every_width =
  mapM_ (\w -> L3.IntS w @=? L4.mkScalar (L4.IntTy w))
        [W8, W16, W32, W64]

--------------------------------------------------------------------------------
-- L3 negative tests: malformed scalar reads/writes
--------------------------------------------------------------------------------

l3tc :: L3.Exp3 -> Either (L1.TCError L3.Exp3) L3.Ty3
l3tc = runExcept . L3.tcExp True M.empty env
  where env = Env2 (M.fromList [("cur", CursorTy), ("i8", IntTy W8), ("i16", IntTy W16)]) M.empty

-- | ReadScalar (IntS W8) bound at IntTy W16 must be rejected.
case_l3_rejects_read_scalar_width_mismatch :: Assertion
case_l3_rejects_read_scalar_width_mismatch =
  case l3tc (LetE ("v", [], ProdTy [IntTy W16, CursorTy],
                   Ext (L3.ReadScalar (L3.IntS W8) "cur")) (VarE "v")) of
    Left _  -> pure ()
    Right t -> assertFailure ("expected a type error, got " ++ show t)

-- | ReadScalar at the matching width is accepted.
case_l3_accepts_read_scalar_matching_width :: Assertion
case_l3_accepts_read_scalar_matching_width =
  case l3tc (Ext (L3.ReadScalar (L3.IntS W8) "cur")) of
    Right t -> ProdTy [IntTy W8, CursorTy] @=? t
    Left e  -> assertFailure ("expected this to typecheck: " ++ show e)

-- | WriteScalar (IntS W8) supplied an Int16 expression must be rejected.
case_l3_rejects_write_scalar_width_mismatch :: Assertion
case_l3_rejects_write_scalar_width_mismatch =
  case l3tc (Ext (L3.WriteScalar (L3.IntS W8) "cur" (VarE "i16"))) of
    Left _  -> pure ()
    Right t -> assertFailure ("expected a type error, got " ++ show t)

case_l3_accepts_write_scalar_matching_width :: Assertion
case_l3_accepts_write_scalar_matching_width =
  case l3tc (Ext (L3.WriteScalar (L3.IntS W8) "cur" (VarE "i8"))) of
    Right t -> CursorTy @=? t
    Left e  -> assertFailure ("expected this to typecheck: " ++ show e)

--------------------------------------------------------------------------------
-- Layout-annotation pragma parsing
--------------------------------------------------------------------------------

-- | The canonical, recognized memory-layout annotation forms, and the
-- diagnostics for two ways to get it wrong.  These exist because both a
-- silent Linear/AoS fallback and a silently-vanishing leading pragma were
-- real, discovered bugs (see 'Gibbon.HaskellFrontend.desugarModule' and
-- 'parseAnnotation'): a test's filename or comments saying "SoA" was not
-- previously evidence that it was.

factoredSrc :: String
factoredSrc = unlines
  [ "data Solo = SNil | SCons Int64 Solo"
  , "{-# ANN type Solo \"Factored\" #-}"
  , "gibbon_main = 0"
  ]

linearSrc :: String
linearSrc = unlines
  [ "data Solo = SNil | SCons Int64 Solo"
  , "{-# ANN type Solo \"Linear\" #-}"
  , "gibbon_main = 0"
  ]

unsupportedLayoutSyntaxSrc :: String
unsupportedLayoutSyntaxSrc = unlines
  [ "data Solo = SNil | SCons Int64 Solo"
  , "{-# ANN Solo (Layout \"SoA\") #-}"
  , "gibbon_main = 0"
  ]

unknownLayoutStringSrc :: String
unknownLayoutStringSrc = unlines
  [ "data Solo = SNil | SCons Int64 Solo"
  , "{-# ANN type Solo \"Bogus\" #-}"
  , "gibbon_main = 0"
  ]

-- | Look up a DDef's memory layout by type-constructor name.
ddefLayout :: L3.Prog3 -> String -> MemoryLayout
ddefLayout Prog{ddefs} tycon = memLayout (lookupDDef ddefs tycon)

case_factored_annotation_produces_fully_factored :: Assertion
case_factored_annotation_produces_fully_factored = do
  p <- okL3 "factored" [Opt_Packed] factoredSrc
  FullyFactored @=? ddefLayout p "Solo"

case_linear_annotation_produces_linear :: Assertion
case_linear_annotation_produces_linear = do
  p <- okL3 "linear" [Opt_Packed] linearSrc
  Linear @=? ddefLayout p "Solo"

case_unsupported_layout_syntax_fails_clearly :: Assertion
case_unsupported_layout_syntax_fails_clearly = do
  msg <- badL3 "unsupported-layout-syntax" [Opt_Packed] unsupportedLayoutSyntaxSrc
  assertBool ("expected a message naming the accepted annotation forms, got:\n" ++ msg)
             ("Factored" `isInfixOf` msg && "Unsupported" `isInfixOf` msg)

case_unknown_layout_string_fails_clearly :: Assertion
case_unknown_layout_string_fails_clearly = do
  msg <- badL3 "unknown-layout-string" [Opt_Packed] unknownLayoutStringSrc
  assertBool ("expected a message naming the accepted annotation forms, got:\n" ++ msg)
             ("Factored" `isInfixOf` msg && "Unsupported" `isInfixOf` msg)

-- | A leading pragma (the very first thing in the file, before the
-- declaration it names) used to be silently dropped by haskell-src-exts's
-- module- vs. decl-level pragma classification, with no diagnostic, and the
-- named type defaulted to Linear as if unannotated.  This is the same
-- 'factoredSrc' program with the pragma moved before the datatype, which
-- must produce the identical FullyFactored layout, not the previous silent
-- Linear fallback.
factoredSrcLeadingPragma :: String
factoredSrcLeadingPragma = unlines
  [ "{-# ANN type Solo \"Factored\" #-}"
  , "data Solo = SNil | SCons Int64 Solo"
  , "gibbon_main = 0"
  ]

case_leading_factored_annotation_is_not_dropped :: Assertion
case_leading_factored_annotation_is_not_dropped = do
  p <- okL3 "leading-factored" [Opt_Packed] factoredSrcLeadingPragma
  FullyFactored @=? ddefLayout p "Solo"

-- | A genuinely factored (SoA) datatype's constructor function threads a
-- cursor *array* (one slot per scalar field buffer, plus the tag), not a
-- single flat cursor the way AoS/Linear does -- this is the representation
-- 'Gibbon.Passes.Cursorize' must respect.  Checked at Int64 specifically
-- (not a narrow width) so this test exercises only the layout
-- representation, independent of the narrow-SoA Cursorize limitation
-- tracked elsewhere.
factoredTwoFieldSrc :: String
factoredTwoFieldSrc = unlines
  [ "data Pair = PNil | PCons Int64 Int64 Pair"
  , "{-# ANN type Pair \"Factored\" #-}"
  , "mkPair :: Int64 -> Pair"
  , "mkPair n = if n < 1 then PNil else PCons n n (mkPair (n - 1))"
  , "gibbon_main ="
  , "  let p :: Pair"
  , "      p = mkPair 3"
  , "  in 0"
  ]

case_factored_datatype_uses_cursor_array_representation :: Assertion
case_factored_datatype_uses_cursor_array_representation = do
  p@Prog{fundefs} <- okL3 "factored-two-field" [Opt_Packed] factoredTwoFieldSrc
  FullyFactored @=? ddefLayout p "Pair"
  let mkPairArgTys = case [ argTys | (fn,L3.FunDef{funTy = (argTys,_)}) <- M.toList fundefs
                                    , "mkPair" `isInfixOf` fromVar fn ] of
        (argTys:_) -> argTys
        [] -> error ("mkPair not found in L3 fundefs: " ++ show (M.keys fundefs))
      isCursorArray t = case t of { CursorArrayTy{} -> True; _ -> False }
  assertBool ("expected mkPair's L3 signature to thread a CursorArrayTy for the "
              ++ "factored constructor result, got argument types: " ++ show mkPairArgTys)
             (any isCursorArray mkPairArgTys)

intWidthPipelineTests :: TestTree
intWidthPipelineTests = $(testGroupGenerator)
