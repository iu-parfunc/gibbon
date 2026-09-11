{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Focused tests for integer-width defaulting in the frontend and L0.
--
-- Design being tested: bare `Int` desugars to 'IntTy W64' in
-- 'Gibbon.HaskellFrontend.desugarType'; every explicit spelling
-- (Int8/16/32/64, and `Int 8`..`Int 64`) is a different pattern in the same
-- function and selects its own width unconditionally. Unconstrained literals
-- and width-sensitive primitives default to W64 in 'Gibbon.L0.Typecheck'.
-- Compiler-internal W64 values (mkLitE64, intS64, the *P64 helpers) are built
-- with a concrete width and never pass through that defaulting path.
--
-- There used to be a `--int32` flag that changed what a bare `Int` meant for
-- the whole compile; it has been removed (integer width is a source-language
-- type now), so this module no longer has a second dynflags configuration to
-- test against. 'case_cli_rejects_int32_flag' checks that the flag itself is
-- rejected rather than silently accepted.
--
-- These tests deliberately stop at L1: they exercise only frontend and L0
-- type/literal/primitive width selection. Generated-C correctness is
-- exercised elsewhere (see the Codegen/RTS test suites); this module makes
-- no claim about it.
module IntWidthsCompat (intWidthsCompatTests) where

import Control.Exception (SomeException, evaluate, try)
import qualified Data.Map as M
import Data.List (isInfixOf)
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))

import Options.Applicative (execParserPure, defaultPrefs, info, helper, renderFailure, ParserResult(..))

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Gibbon.Common
import Gibbon.HaskellFrontend (parseFile)
import Gibbon.Passes.Freshen (freshNames)
import qualified Gibbon.L0.Specialize2 as L0
import qualified Gibbon.L0.Syntax as L0
import qualified Gibbon.L0.Typecheck as L0
import Gibbon.L1.Syntax
import Gibbon.Compiler (configWithArgs, int32TombstoneOption)

--------------------------------------------------------------------------------
-- Harness: source text -> L1
--------------------------------------------------------------------------------

srcToL1 :: String -> IO Prog1
srcToL1 src = do
  tmp <- getTemporaryDirectory
  let fp = tmp </> ("gibbon-intwidth-compat-" ++ show (abs (hashStr src)) ++ ".hs")
  writeFile fp src
  parsed <- parseFile defaultConfig fp
  removeFile fp
  pure $ fst $ runPassM defaultConfig 0 (parsed >>= l0Passes)
  where
    hashStr :: String -> Int
    hashStr = foldl (\acc c -> acc * 33 + fromEnum c) 5381

    l0Passes :: L0.Prog0 -> PassM Prog1
    l0Passes p0 = do
      p1 <- freshNames p0
      p2 <- L0.tcProg p1
      p3 <- L0.bindLambdas p2
      p4 <- L0.monomorphize p3
      p5 <- L0.specLambdas p4
      p6 <- L0.desugarL0 p5
      p7 <- L0.floatOutCase p6
      pure (L0.toL1 p7)

forceL1 :: IO Prog1 -> IO (Either String Prog1)
forceL1 act = do
  r <- try (act >>= \p -> evaluate (length (sdoc p)) >> pure p)
  pure $ case r of
           Left (e :: SomeException) -> Left (show e)
           Right p -> Right p

okProg :: IO Prog1 -> IO Prog1
okProg act = do
  r <- forceL1 act
  case r of
    Left e  -> assertFailure ("expected this to compile, but it failed:\n" ++ e)
    Right p -> pure p

badProg :: IO Prog1 -> IO String
badProg act = do
  r <- forceL1 act
  case r of
    Left e  -> pure e
    Right p -> assertFailure ("expected a type error, but it compiled to:\n" ++ sdoc p)

--------------------------------------------------------------------------------
-- Inspecting an L1 program
--------------------------------------------------------------------------------

l1Exps :: Prog1 -> [Exp1]
l1Exps Prog{fundefs,mainExp} =
  concatMap (subExps . funBody) (M.elems fundefs) ++ maybe [] (subExps . fst) mainExp

subExps :: Exp1 -> [Exp1]
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
        _ -> []

-- | Widths of every let binder in the program.
binderWidths :: Prog1 -> [IntWidth]
binderWidths p = [ w | LetE (_,_,ty,_) _ <- l1Exps p, IntTy w <- [ty] ]

-- | Widths of every literal, keyed by value (values are chosen distinct per
-- test so this is unambiguous).
litWidthOf :: Prog1 -> Integer -> [IntWidth]
litWidthOf p v = [ litWidth ann | LitE ann n <- l1Exps p, n == v ]

-- | Widths of every arithmetic/comparison primitive.
primWidths :: Prog1 -> [IntWidth]
primWidths p = [ intPrimWidth a | PrimAppE pr _ <- l1Exps p, Just a <- [intPrimAnnOf pr]
                                 , isIntArithPrim pr || isIntCmpPrim pr ]

-- | The declared argument/result widths of a top-level function's arrow type.
funArgResWidths :: Prog1 -> Var -> ([IntWidth],[IntWidth])
funArgResWidths Prog{fundefs} fn =
  case M.lookup fn fundefs of
    Nothing -> ([],[])
    Just FunDef{funTy=(argTys,resTy)} -> ([w | IntTy w <- argTys], [w | IntTy w <- [resTy]])

--------------------------------------------------------------------------------
-- Bare Int
--------------------------------------------------------------------------------

case_bare_int_is_w64 :: Assertion
case_bare_int_is_w64 = do
  p <- okProg $ srcToL1 $ unlines
       [ "f :: Int -> Int"
       , "f x = x"
       , "gibbon_main = f 1"
       ]
  ([W64],[W64]) @=? funArgResWidths p "f"

--------------------------------------------------------------------------------
-- Explicit widths
--------------------------------------------------------------------------------

case_explicit_int32_is_w32 :: Assertion
case_explicit_int32_is_w32 = do
  p <- okProg $ srcToL1 "f :: Int32 -> Int32\nf x = x\ngibbon_main = f 1"
  ([W32],[W32]) @=? funArgResWidths p "f"

case_explicit_int64_is_w64 :: Assertion
case_explicit_int64_is_w64 = do
  p <- okProg $ srcToL1 "f :: Int64 -> Int64\nf x = x\ngibbon_main = f 1"
  ([W64],[W64]) @=? funArgResWidths p "f"

case_promoted_int_32_is_w32 :: Assertion
case_promoted_int_32_is_w32 = do
  p <- okProg $ srcToL1 $ unlines
       [ "{-# LANGUAGE DataKinds #-}"
       , "f :: (Int 32) -> (Int 32)"
       , "f x = x"
       , "gibbon_main = f 1"
       ]
  ([W32],[W32]) @=? funArgResWidths p "f"

--------------------------------------------------------------------------------
-- Unconstrained literal / primitive defaulting
--------------------------------------------------------------------------------

case_unconstrained_literal_defaults_w64 :: Assertion
case_unconstrained_literal_defaults_w64 = do
  p <- okProg $ srcToL1 "gibbon_main = 91"
  [W64] @=? litWidthOf p 91

case_contextual_int32_literal_is_w32 :: Assertion
case_contextual_int32_literal_is_w32 = do
  p <- okProg $ srcToL1 $ unlines
       [ "f :: Int32"
       , "f = 93"
       , "gibbon_main = f"
       ]
  [W32] @=? litWidthOf p 93

case_arith_defaults_w64 :: Assertion
case_arith_defaults_w64 = do
  p <- okProg $ srcToL1 "gibbon_main = 1 + 2"
  [W64] @=? primWidths p

--------------------------------------------------------------------------------
-- Compiler-internal W64 literals/primitives
--------------------------------------------------------------------------------

-- | A String literal desugars via 'mkLitE64'/compiler-internal vector-alloc
-- primitives (see 'Gibbon.HaskellFrontend.desugarLiteral'); those must stay
-- W64 regardless of anything a source program does with bare `Int`.
case_compiler_internal_vec_alloc_stays_w64 :: Assertion
case_compiler_internal_vec_alloc_stays_w64 = do
  p <- okProg $ srcToL1 $ unlines
       [ "gibbon_main = let v :: Vector Char"
       , "                  v = valloc 0"
       , "              in 0"
       ]
  let ws = [ w | PrimAppE (VAllocP _) [LitE ann _] <- l1Exps p, LitWidth w <- [ann] ]
  assertBool ("expected a W64 vector-alloc size, got " ++ show ws) (ws == [W64])

--------------------------------------------------------------------------------
-- Mixed bare-Int / explicit-Int32 arithmetic
--------------------------------------------------------------------------------

-- | Bare `Int` (W64) and explicit `Int32` are different widths; mixing them
-- in arithmetic is the ordinary mixed-width type error, exactly as mixing
-- Int8 and Int64 already is.
case_mixed_bare_int_and_explicit_int32_rejected :: Assertion
case_mixed_bare_int_and_explicit_int32_rejected = do
  msg <- badProg $ srcToL1 $ unlines
         [ "f :: Int -> Int32 -> Int32"
         , "f x y = x + y"
         , "gibbon_main = 0"
         ]
  assertBool ("expected a width-mismatch diagnostic, got:\n" ++ msg)
             ("match" `isInfixOf` msg)

--------------------------------------------------------------------------------
-- gRecoverType / intWidthBytes: sanity that the per-width byte-count table is
-- exactly what Codegen/RTS rely on.
--------------------------------------------------------------------------------

case_int_width_bytes :: Assertion
case_int_width_bytes = do
  1 @=? intWidthBytes W8
  2 @=? intWidthBytes W16
  4 @=? intWidthBytes W32
  8 @=? intWidthBytes W64

--------------------------------------------------------------------------------
-- --int32 is rejected, not silently accepted
--------------------------------------------------------------------------------

-- | '--int32' used to be a whole-program 32-bit-backend switch. It has been
-- removed; the CLI must reject it with an actionable message rather than
-- silently ignoring it or accepting it as a no-op.
case_cli_rejects_int32_flag :: Assertion
case_cli_rejects_int32_flag = do
  let parser = info (helper <*> int32TombstoneOption <*> configWithArgs) mempty
  case execParserPure defaultPrefs parser ["--int32", "some-file.hs"] of
    Failure failure -> do
      let (msg, _) = renderFailure failure "gibbon"
      assertBool ("expected the removal message, got:\n" ++ msg)
                  ("--int32 has been removed" `isInfixOf` msg)
    other -> assertFailure ("expected --int32 to be rejected by the parser, got: " ++ show (fmap (const ()) other))

intWidthsCompatTests :: TestTree
intWidthsCompatTests = $(testGroupGenerator)
