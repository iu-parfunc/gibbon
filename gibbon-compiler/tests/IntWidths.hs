{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for variable-width integers: the typed literal representation and
-- direct contextual literal inference at L0.
--
-- These tests deliberately stop at L1.  L4 still erases integer widths, so a
-- successful C run would say nothing about narrow-width code generation; what
-- matters here is the width each literal carries in the typed IR.
module IntWidths (intWidthTests) where

import Control.Exception (SomeException, evaluate, try)
import qualified Data.List as L
import Data.List (isInfixOf)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Except (runExcept)
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

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
import qualified Gibbon.L1.Typecheck as L1
import qualified Gibbon.L2.Syntax as L2
import qualified Gibbon.L2.Typecheck as L2
import qualified Gibbon.L3.Syntax as L3
import qualified Gibbon.L3.Typecheck as L3
import Gibbon.Passes.InferLocations (inferLocs)

--------------------------------------------------------------------------------
-- Harness: source text -> L1
--------------------------------------------------------------------------------

-- | Run the frontend and the L0 pipeline on a source string, stopping at L1.
srcToL1 :: String -> IO Prog1
srcToL1 src = do
  tmp <- getTemporaryDirectory
  -- A stable name per source string keeps parallel test runs from colliding.
  let fp = tmp </> ("gibbon-intwidth-" ++ show (abs (hashStr src)) ++ ".hs")
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
      -- The REAL L1 typechecker runs here, exactly as the compiler pipeline
      -- does.  Integer primitives (including PrintInt in derived printers) are
      -- width-polymorphic now, so a datatype with a narrow field must survive
      -- this.
      L1.tcProg (L0.toL1 p7)

-- | Force the whole L1 program, catching the internal errors that the L0
-- typechecker and the L0->L1 boundary raise.
forceL1 :: String -> IO (Either String Prog1)
forceL1 src = do
  r <- try (srcToL1 src >>= \p -> evaluate (length (sdoc p)) >> pure p)
  pure $ case r of
           Left (e :: SomeException) -> Left (show e)
           Right p                   -> Right p

-- | Every integer literal in a program, in traversal order, paired with its
-- annotation.
litsOf :: Prog1 -> [(LitAnn, Integer)]
litsOf Prog{fundefs,mainExp} =
  concatMap (goE . funBody) (M.elems fundefs)
  ++ maybe [] (goE . fst) mainExp
  where
    goE :: Exp1 -> [(LitAnn, Integer)]
    goE ex =
      case ex of
        LitE ann n -> [(ann, n)]
        VarE{}     -> []
        CharE{}    -> []
        FloatE{}   -> []
        LitSymE{}  -> []
        AppE _ _ _ args    -> concatMap goE args
        PrimAppE _ args    -> concatMap goE args
        LetE (_,_,_,rhs) b -> goE rhs ++ goE b
        IfE a b c  -> goE a ++ goE b ++ goE c
        MkProdE ls -> concatMap goE ls
        ProjE _ e  -> goE e
        CaseE s brs -> goE s ++ concatMap (\(_,_,r) -> goE r) brs
        DataConE _ _ args -> concatMap goE args
        TimeIt e _ _   -> goE e
        WithArenaE _ e -> goE e
        SpawnE _ _ args -> concatMap goE args
        SyncE      -> []
        MapE (_,_,e1) e2 -> goE e1 ++ goE e2
        FoldE (_,_,e1) (_,_,e2) e3 -> goE e1 ++ goE e2 ++ goE e3
        Ext _      -> []

-- | Widths of the literals whose value is one of the given values.  Values are
-- used as the key because the L0 pipeline is free to reorder or duplicate
-- bindings, but it may not change what a literal means.
widthOf :: Prog1 -> Integer -> [IntWidth]
widthOf p v = [ litWidth ann | (ann, n) <- litsOf p, n == v ]

assertWidth :: String -> Integer -> IntWidth -> Prog1 -> Assertion
assertWidth lbl v w p =
  case widthOf p v of
    []  -> assertFailure (lbl ++ ": no literal with value " ++ show v
                          ++ " in L1; literals were " ++ show (litsOf p))
    ws  -> assertBool (lbl ++ ": literal " ++ show v ++ " has widths " ++ show ws
                       ++ ", expected all " ++ show w)
                      (all (== w) ws)

-- | Compile a program that is expected to typecheck.
okProg :: String -> IO Prog1
okProg src = do
  r <- forceL1 src
  case r of
    Left e  -> assertFailure ("expected this to compile, but it failed:\n" ++ e)
    Right p -> pure p

-- | Compile a program that is expected to be rejected, and return the message.
badProg :: String -> IO String
badProg src = do
  r <- forceL1 src
  case r of
    Left e  -> pure e
    Right p -> assertFailure ("expected a type error, but it compiled to:\n" ++ sdoc p)


-- | Every width-sensitive integer primitive in a program, in traversal order.
primsOf :: Prog1 -> [Prim Ty1]
primsOf Prog{fundefs,mainExp} =
  concatMap (goE . funBody) (M.elems fundefs) ++ maybe [] (goE . fst) mainExp
  where
    goE :: Exp1 -> [Prim Ty1]
    goE ex =
      case ex of
        PrimAppE pr args -> [pr | isWidthSensitivePrim pr] ++ concatMap goE args
        VarE{} -> [] ; LitE{} -> [] ; CharE{} -> [] ; FloatE{} -> [] ; LitSymE{} -> []
        AppE _ _ _ args    -> concatMap goE args
        LetE (_,_,_,rhs) b -> goE rhs ++ goE b
        IfE a b c  -> goE a ++ goE b ++ goE c
        MkProdE ls -> concatMap goE ls
        ProjE _ e  -> goE e
        CaseE sc brs -> goE sc ++ concatMap (\(_,_,r) -> goE r) brs
        DataConE _ _ args -> concatMap goE args
        TimeIt e _ _   -> goE e
        WithArenaE _ e -> goE e
        SpawnE _ _ args -> concatMap goE args
        SyncE      -> []
        MapE (_,_,e1) e2 -> goE e1 ++ goE e2
        FoldE (_,_,e1) (_,_,e2) e3 -> goE e1 ++ goE e2 ++ goE e3
        Ext _      -> []

-- | Widths of the arithmetic primitives in a program.
arithWidths :: Prog1 -> [IntWidth]
arithWidths p = [ intPrimWidth a | pr <- primsOf p, isIntArithPrim pr
                                 , Just a <- [intPrimAnnOf pr] ]

cmpWidths :: Prog1 -> [IntWidth]
cmpWidths p = [ intPrimWidth a | pr <- primsOf p, isIntCmpPrim pr
                               , Just a <- [intPrimAnnOf pr] ]

printIntWidths :: Prog1 -> [IntWidth]
printIntWidths p = [ intPrimWidth a | PrintInt a <- primsOf p ]

-- | Source for a one-argument function doing `x <op> y` at a given width.
arithSrc :: String -> String -> String
arithSrc ty op = unlines
  [ "f :: " ++ ty ++ " -> " ++ ty ++ " -> " ++ ty
  , "f x y = x " ++ op ++ " y"
  , ""
  , "gibbon_main = 0"
  ]

--------------------------------------------------------------------------------
-- Direct contextual inference
--------------------------------------------------------------------------------

-- | Constructor fields, both spellings of the width, and bare Int.
case_lit_constructor_fields :: Assertion
case_lit_constructor_fields = do
  p <- okProg $ unlines
       [ "data W = W Int8 Int16 Int32 Int64 Int"
       , ""
       , "mkW :: W"
       , "mkW = W 1 2 3 4 5"
       , ""
       , "gibbon_main = mkW"
       ]
  assertWidth "field 1" 1 W8  p
  assertWidth "field 2" 2 W16 p
  assertWidth "field 3" 3 W32 p
  assertWidth "field 4" 4 W64 p
  assertWidth "field 5" 5 W64 p

-- | The parameterized spelling @Int 8@ .. @Int 64@ must behave identically.
case_lit_constructor_fields_promoted :: Assertion
case_lit_constructor_fields_promoted = do
  p <- okProg $ unlines
       [ "{-# LANGUAGE DataKinds #-}"
       , "data W = W (Int 8) (Int 16) (Int 32) (Int 64)"
       , ""
       , "mkW :: W"
       , "mkW = W 11 22 33 44"
       , ""
       , "gibbon_main = mkW"
       ]
  assertWidth "field 11" 11 W8  p
  assertWidth "field 22" 22 W16 p
  assertWidth "field 33" 33 W32 p
  assertWidth "field 44" 44 W64 p

-- | A literal passed directly to a function expecting each width.
case_lit_function_argument :: Assertion
case_lit_function_argument = do
  p <- okProg $ unlines
       [ "data W = W Int8 Int16 Int32 Int64"
       , ""
       , "f8 :: Int8 -> W"
       , "f8 a = W a 0 0 0"
       , ""
       , "f16 :: Int16 -> W"
       , "f16 a = W 0 a 0 0"
       , ""
       , "f32 :: Int32 -> W"
       , "f32 a = W 0 0 a 0"
       , ""
       , "f64 :: Int64 -> W"
       , "f64 a = W 0 0 0 a"
       , ""
       , "gibbon_main = if True then f8 101 else (if True then f16 102 else"
       , "         (if True then f32 103 else f64 104))"
       ]
  assertWidth "arg 101" 101 W8  p
  assertWidth "arg 102" 102 W16 p
  assertWidth "arg 103" 103 W32 p
  assertWidth "arg 104" 104 W64 p

-- | A literal returned directly from a width-specific function.
case_lit_function_result :: Assertion
case_lit_function_result = do
  p <- okProg $ unlines
       [ "data W = W Int8 Int16 Int32"
       , ""
       , "r8 :: Int8"
       , "r8 = 51"
       , ""
       , "r16 :: Int16"
       , "r16 = 52"
       , ""
       , "r32 :: Int32"
       , "r32 = 53"
       , ""
       , "gibbon_main = W r8 r16 r32"
       ]
  assertWidth "ret 51" 51 W8  p
  assertWidth "ret 52" 52 W16 p
  assertWidth "ret 53" 53 W32 p

-- | Annotated local bindings for each width.
case_lit_annotated_let :: Assertion
case_lit_annotated_let = do
  p <- okProg $ unlines
       [ "data W = W Int8 Int16 Int32 Int64"
       , ""
       , "mkW :: W"
       , "mkW ="
       , "  let a :: Int8"
       , "      a = 61"
       , "      b :: Int16"
       , "      b = 62"
       , "      c :: Int32"
       , "      c = 63"
       , "      d :: Int64"
       , "      d = 64"
       , "  in W a b c d"
       , ""
       , "gibbon_main = mkW"
       ]
  assertWidth "let 61" 61 W8  p
  assertWidth "let 62" 62 W16 p
  assertWidth "let 63" 63 W32 p
  assertWidth "let 64" 64 W64 p

-- | Tuple components, when the expected product type is known.
case_lit_tuple_components :: Assertion
case_lit_tuple_components = do
  p <- okProg $ unlines
       [ "f :: (Int8, Int32) -> Int"
       , "f p = 0"
       , ""
       , "gibbon_main = f (71, 72)"
       ]
  assertWidth "tuple 71" 71 W8  p
  assertWidth "tuple 72" 72 W32 p

-- | An unconstrained literal defaults to Int64, and bare Int stays Int64.
case_lit_defaults_to_w64 :: Assertion
case_lit_defaults_to_w64 = do
  p <- okProg $ unlines
       [ "gibbon_main = 81"
       ]
  assertWidth "default 81" 81 W64 p

-- | No literal reaches L1 unresolved.  'litWidth' errors on an unresolved
-- annotation, so forcing every literal's width is the check.
case_no_unresolved_literal_reaches_l1 :: Assertion
case_no_unresolved_literal_reaches_l1 = do
  p <- okProg $ unlines
       [ "data W = W Int8 Int16 Int32 Int64 Int"
       , ""
       , "g :: Int8 -> Int"
       , "g a = 1"
       , ""
       , "h :: Int32"
       , "h = let x :: Int32"
       , "        x = 92"
       , "    in x"
       , ""
       , "gibbon_main = g 91 + 93"
       ]
  r <- try (evaluate (sum (map (fromEnum . litWidth . fst) (litsOf p))))
  case r of
    Left (e :: SomeException) -> assertFailure ("an unresolved literal reached L1: " ++ show e)
    Right _ -> pure ()

--------------------------------------------------------------------------------
-- Negative literals
--------------------------------------------------------------------------------

-- | A negated literal is one signed literal, so the low end of every width is
-- reachable.  If @-128@ were still desugared to @0 - 128@ this would fail with
-- @128@ out of range for Int8.
case_negative_literals_all_widths :: Assertion
case_negative_literals_all_widths = do
  p <- okProg $ unlines
       [ "data W = W Int8 Int16 Int32 Int64"
       , ""
       , "mkW :: W"
       , "mkW = W (-128) (-32768) (-2147483648) (-9223372036854775808)"
       , ""
       , "gibbon_main = mkW"
       ]
  assertWidth "neg int8"  (-128) W8  p
  assertWidth "neg int16" (-32768) W16 p
  assertWidth "neg int32" (-2147483648) W32 p
  assertWidth "neg int64" (-9223372036854775808) W64 p

-- | Negating a non-literal still means subtraction.
case_negation_of_nonliteral_is_subtraction :: Assertion
case_negation_of_nonliteral_is_subtraction = do
  p <- okProg $ unlines
       [ "gibbon_main = let x :: Int"
       , "                  x = 7"
       , "              in -x"
       ]
  -- The desugaring is still `0 - x`, so a SubP and its compiler-generated
  -- zero must both be present.
  assertBool ("expected a subtraction, got:\n" ++ sdoc p) ("SubP" `isInfixOf` sdoc p)
  assertWidth "sub zero" 0 W64 p

--------------------------------------------------------------------------------
-- Boundaries
--------------------------------------------------------------------------------

-- | Build a one-field program at a given width holding a given literal.
boundarySrc :: String -> String -> String
boundarySrc ty lit = unlines
  [ "data B = B " ++ ty
  , ""
  , "mkB :: B"
  , "mkB = B (" ++ lit ++ ")"
  , ""
  , "gibbon_main = mkB"
  ]

assertAccepts :: String -> String -> Assertion
assertAccepts ty lit = do
  _ <- okProg (boundarySrc ty lit)
  pure ()

assertRejects :: String -> String -> Assertion
assertRejects ty lit = do
  msg <- badProg (boundarySrc ty lit)
  assertBool ("expected an out-of-range diagnostic mentioning the literal, got:\n" ++ msg)
             ("out of range" `isInfixOf` msg)
  assertBool ("diagnostic should name the literal " ++ lit ++ ", got:\n" ++ msg)
             (lit `isInfixOf` msg)

case_boundary_int8 :: Assertion
case_boundary_int8 = do
  assertAccepts "Int8" "-128"
  assertAccepts "Int8" "127"
  assertRejects "Int8" "-129"
  assertRejects "Int8" "128"

case_boundary_int16 :: Assertion
case_boundary_int16 = do
  assertAccepts "Int16" "-32768"
  assertAccepts "Int16" "32767"
  assertRejects "Int16" "-32769"
  assertRejects "Int16" "32768"

case_boundary_int32 :: Assertion
case_boundary_int32 = do
  assertAccepts "Int32" "-2147483648"
  assertAccepts "Int32" "2147483647"
  assertRejects "Int32" "-2147483649"
  assertRejects "Int32" "2147483648"

case_boundary_int64 :: Assertion
case_boundary_int64 = do
  assertAccepts "Int64" "-9223372036854775808"
  assertAccepts "Int64" "9223372036854775807"
  assertRejects "Int64" "-9223372036854775809"
  assertRejects "Int64" "9223372036854775808"

-- | A defaulted (unconstrained) literal is range-checked against Int64 too,
-- which is only possible because the parser's arbitrary-precision value
-- survives to the check instead of being truncated by 'fromIntegral'.
case_boundary_defaulted_int64 :: Assertion
case_boundary_defaulted_int64 = do
  msg <- badProg $ unlines
         [ "gibbon_main = 9223372036854775808"
         ]
  assertBool ("expected an out-of-range diagnostic, got:\n" ++ msg)
             ("out of range" `isInfixOf` msg)

-- | The diagnostic must name the value, the type and the valid range.
case_range_diagnostic_content :: Assertion
case_range_diagnostic_content = do
  msg <- badProg (boundarySrc "Int8" "200")
  assertBool ("missing the literal value:\n"  ++ msg) ("200"  `isInfixOf` msg)
  assertBool ("missing the integer type:\n"   ++ msg) ("Int8" `isInfixOf` msg)
  assertBool ("missing the range low end:\n"  ++ msg) ("-128" `isInfixOf` msg)
  assertBool ("missing the range high end:\n" ++ msg) ("127"  `isInfixOf` msg)

--------------------------------------------------------------------------------
-- Negative typing
--------------------------------------------------------------------------------

-- | Contextual literal typing must not become a general conversion: an
-- expected type that is not an integer type still rejects the literal.
case_literal_where_bool_expected :: Assertion
case_literal_where_bool_expected = do
  _ <- badProg $ unlines
       [ "data B = B Bool"
       , ""
       , "gibbon_main = B 1"
       ]
  pure ()

case_literal_where_float_expected :: Assertion
case_literal_where_float_expected = do
  _ <- badProg $ unlines
       [ "data F = F Float"
       , ""
       , "gibbon_main = F 1"
       ]
  pure ()

-- | Widths never mix implicitly, literals or not.
case_mixed_width_arithmetic_rejected :: Assertion
case_mixed_width_arithmetic_rejected = do
  _ <- badProg $ unlines
       [ "f :: Int8 -> Int32 -> Int"
       , "f a b = 0"
       , ""
       , "gibbon_main = let a :: Int8"
       , "                  a = 1"
       , "                  b :: Int32"
       , "                  b = 2"
       , "              in f (a + b) b"
       ]
  pure ()

-- | An already-typed narrow integer is never widened implicitly.
case_no_implicit_widening :: Assertion
case_no_implicit_widening = do
  _ <- badProg $ unlines
       [ "wide :: Int64 -> Int"
       , "wide a = 0"
       , ""
       , "gibbon_main = let a :: Int8"
       , "                  a = 1"
       , "              in wide a"
       ]
  pure ()

--------------------------------------------------------------------------------
-- Type recovery reads the stored width
--------------------------------------------------------------------------------

ddfs :: DDefs Ty1
ddfs = M.empty

env0 :: Env2 Var Ty1
env0 = Env2 M.empty M.empty

-- | 'gRecoverType' must report the literal's stored width, not a hard-coded
-- W64.
case_recover_type_uses_stored_width :: Assertion
case_recover_type_uses_stored_width = do
  IntTy W8  @=? gRecoverType ddfs env0 (LitE (LitWidth W8)  1 :: Exp1)
  IntTy W16 @=? gRecoverType ddfs env0 (LitE (LitWidth W16) 1 :: Exp1)
  IntTy W32 @=? gRecoverType ddfs env0 (LitE (LitWidth W32) 1 :: Exp1)
  IntTy W64 @=? gRecoverType ddfs env0 (LitE (LitWidth W64) 1 :: Exp1)

-- | And so must the compiler-internal literal helper.
case_mk_lit_e64_is_w64 :: Assertion
case_mk_lit_e64_is_w64 =
  LitE (LitWidth W64) 7 @=? (mkLitE64 7 :: Exp1)

--------------------------------------------------------------------------------
-- Width arithmetic helpers
--------------------------------------------------------------------------------

case_int_width_ranges :: Assertion
case_int_width_ranges = do
  (-128, 127)                                       @=? intWidthRange W8
  (-32768, 32767)                                   @=? intWidthRange W16
  (-2147483648, 2147483647)                         @=? intWidthRange W32
  (-9223372036854775808, 9223372036854775807)       @=? intWidthRange W64
  True  @=? intWidthFits W8 (-128)
  True  @=? intWidthFits W8 127
  False @=? intWidthFits W8 (-129)
  False @=? intWidthFits W8 128


--------------------------------------------------------------------------------
-- Width-homogeneous integer primitives
--------------------------------------------------------------------------------

-- | Arithmetic on two typed operands, at every width.
case_arith_all_widths :: Assertion
case_arith_all_widths =
  mapM_ (\(ty,w) ->
           mapM_ (\op -> do p <- okProg (arithSrc ty op)
                            [w] @=? arithWidths p)
                 ["+","-","*"])
        [("Int8",W8),("Int16",W16),("Int32",W32),("Int64",W64),("Int",W64)]

-- | Comparisons on two typed operands, at every width; result is Bool.
case_cmp_all_widths :: Assertion
case_cmp_all_widths =
  mapM_ (\(ty,w) ->
           mapM_ (\op -> do
                    p <- okProg $ unlines
                         [ "f :: " ++ ty ++ " -> " ++ ty ++ " -> Bool"
                         , "f x y = x " ++ op ++ " y"
                         , ""
                         , "gibbon_main = 0" ]
                    [w] @=? cmpWidths p)
                 ["<",">","<=",">=","=="])
        [("Int8",W8),("Int16",W16),("Int32",W32),("Int64",W64),("Int",W64)]

-- | A typed operand plus an unresolved literal, in both operand orders: the
-- literal takes the operand's width and so does the primitive.
case_typed_operand_plus_literal :: Assertion
case_typed_operand_plus_literal =
  mapM_ (\(ty,w) -> do
           pl <- okProg $ unlines
                 [ "f :: " ++ ty ++ " -> " ++ ty
                 , "f x = x + 3"
                 , "gibbon_main = 0" ]
           [w] @=? arithWidths pl
           assertWidth ("lit right " ++ ty) 3 w pl
           pr <- okProg $ unlines
                 [ "f :: " ++ ty ++ " -> " ++ ty
                 , "f x = 4 + x"
                 , "gibbon_main = 0" ]
           [w] @=? arithWidths pr
           assertWidth ("lit left " ++ ty) 4 w pr)
        [("Int8",W8),("Int16",W16),("Int32",W32),("Int64",W64)]

-- | An expected result width is pushed into `1 + 2`, where neither operand
-- carries a width of its own.
case_expected_width_into_two_literals :: Assertion
case_expected_width_into_two_literals =
  mapM_ (\(ty,w) -> do
           p <- okProg $ unlines
                [ "f :: " ++ ty
                , "f = 1 + 2"
                , "gibbon_main = 0" ]
           [w] @=? arithWidths p
           assertWidth ("lhs " ++ ty) 1 w p
           assertWidth ("rhs " ++ ty) 2 w p)
        [("Int8",W8),("Int16",W16),("Int32",W32),("Int64",W64)]

-- | Two unconstrained literals with no expected integer context default to
-- W64, and so does the operation.
case_two_unconstrained_literals_default :: Assertion
case_two_unconstrained_literals_default = do
  p <- okProg $ unlines
       [ "gibbon_main = 21 + 22" ]
  [W64] @=? arithWidths p
  assertWidth "lhs" 21 W64 p
  assertWidth "rhs" 22 W64 p

-- | A comparison whose operands are both unresolved literals defaults to W64
-- (the Bool result cannot select a width).
case_cmp_two_literals_default :: Assertion
case_cmp_two_literals_default = do
  p <- okProg $ unlines
       [ "gibbon_main = if 31 < 32 then 1 else 2" ]
  [W64] @=? cmpWidths p

-- | Range checking happens at the width the primitive context chose, not at
-- W64: 200 fits in an Int64 but not an Int8.
case_literal_range_checked_in_prim_context :: Assertion
case_literal_range_checked_in_prim_context = do
  msg <- badProg $ unlines
         [ "f :: Int8 -> Int8"
         , "f x = x + 200"
         , "gibbon_main = 0" ]
  assertBool ("expected an out-of-range diagnostic, got:\n" ++ msg)
             ("out of range" `isInfixOf` msg)
  assertBool ("diagnostic should name Int8:\n" ++ msg) ("Int8" `isInfixOf` msg)

-- | Mixed-width arithmetic on two typed operands names both widths.
case_mixed_width_arith_names_both :: Assertion
case_mixed_width_arith_names_both = do
  msg <- badProg $ unlines
         [ "bad1 :: Int8 -> Int16 -> Int8"
         , "bad1 x y = x + y"
         , "gibbon_main = 0" ]
  assertBool ("should name Int8:\n"  ++ msg) ("Int8"  `isInfixOf` msg)
  assertBool ("should name Int16:\n" ++ msg) ("Int16" `isInfixOf` msg)

-- | Mixed-width comparison is rejected too.
case_mixed_width_cmp_rejected :: Assertion
case_mixed_width_cmp_rejected = do
  msg <- badProg $ unlines
         [ "bad2 :: Int8 -> Int32 -> Bool"
         , "bad2 x y = x == y"
         , "gibbon_main = 0" ]
  assertBool ("should name Int8:\n"  ++ msg) ("Int8"  `isInfixOf` msg)
  assertBool ("should name Int32:\n" ++ msg) ("Int32" `isInfixOf` msg)

-- | A typed W64 expression combined with a typed W8 expression fails even
-- though every value would fit.
case_w64_plus_w8_rejected :: Assertion
case_w64_plus_w8_rejected = do
  _ <- badProg $ unlines
       [ "bad3 :: Int -> Int8 -> Int"
       , "bad3 x y = x + y"
       , "gibbon_main = 0" ]
  pure ()

-- | A non-integer operand is a typed error, not a width puzzle.
case_noninteger_operand_rejected :: Assertion
case_noninteger_operand_rejected = do
  _ <- badProg $ unlines
       [ "bad4 :: Int8 -> Bool -> Int8"
       , "bad4 x y = x + y"
       , "gibbon_main = 0" ]
  pure ()

--------------------------------------------------------------------------------
-- PrintInt and derived printers
--------------------------------------------------------------------------------

-- | printint accepts every integer width and keeps the operand's width.
case_printint_all_widths :: Assertion
case_printint_all_widths =
  mapM_ (\(ty,w) -> do
           p <- okProg $ unlines
                [ "f :: " ++ ty ++ " -> ()"
                , "f x = printint x"
                , "gibbon_main = 0" ]
           assertBool ("expected a PrintInt at " ++ show w ++ ", got "
                       ++ show (printIntWidths p))
                      (w `elem` printIntWidths p))
        [("Int8",W8),("Int16",W16),("Int32",W32),("Int64",W64),("Int",W64)]

-- | The generated printer for a narrow datatype field prints at the FIELD's
-- width and survives the real L1 typechecker.  This is the blocker that
-- previously made narrow datatypes unusable.
case_generated_printer_narrow_fields :: Assertion
case_generated_printer_narrow_fields = do
  p <- okProg $ unlines
       [ "data B = B Int8 Int16 Int32 Int64"
       , ""
       , "mkB :: B"
       , "mkB = B 7 8 9 10"
       , ""
       , "gibbon_main = mkB"
       ]
  let ws = printIntWidths p
  mapM_ (\w -> assertBool ("derived printer is missing a PrintInt at " ++ show w
                           ++ "; found " ++ show ws)
                          (w `elem` ws))
        [W8,W16,W32,W64]

-- | A single narrow field, end to end through the real L1 typechecker.
case_narrow_datatype_l1_typechecks :: Assertion
case_narrow_datatype_l1_typechecks = do
  p <- okProg $ unlines
       [ "data B = B Int8"
       , ""
       , "mkB :: B"
       , "mkB = B 7"
       , ""
       , "gibbon_main = mkB"
       ]
  assertWidth "narrow field" 7 W8 p
  assertBool "derived printer should print at W8" (W8 `elem` printIntWidths p)

--------------------------------------------------------------------------------
-- The annotation survives lowering
--------------------------------------------------------------------------------

-- | L0 -> L1: no unresolved primitive annotation reaches L1.
case_no_unresolved_prim_reaches_l1 :: Assertion
case_no_unresolved_prim_reaches_l1 = do
  p <- okProg $ unlines
       [ "f :: Int8 -> Int8"
       , "f x = x + 1"
       , ""
       , "g :: Int32 -> Bool"
       , "g y = y > 5"
       , ""
       , "gibbon_main = 1 + 2"
       ]
  let unresolved = [ pr | pr <- primsOf p, Just IntPrimUnresolved <- [intPrimAnnOf pr] ]
  assertBool ("unresolved primitives reached L1: " ++ show unresolved) (null unresolved)
  [W8, W32, W64] @=? L.sort (map intPrimWidth
                              [a | pr <- primsOf p, Just a <- [intPrimAnnOf pr]
                                 , not (isPrintInt pr)])

isPrintInt :: Prim ty -> Bool
isPrintInt p = case p of { PrintInt{} -> True ; _ -> False }

-- | L1 -> L2: run the real location-inference pass on a narrow program and
-- check the annotation survives, then typecheck the L2 program.
case_prim_width_survives_l1_to_l2 :: Assertion
case_prim_width_survives_l1_to_l2 = do
  p1 <- okProg $ unlines
        [ "data B = B Int8"
        , ""
        , "addOne :: Int8 -> Int8"
        , "addOne x = x + 1"
        , ""
        , "gibbon_main = addOne 5"
        ]
  let (p2, _) = defaultPackedRunPassM (inferLocs p1)
      ws = [ intPrimWidth a | pr <- l2Prims p2, isIntArithPrim pr
                            , Just a <- [intPrimAnnOf pr] ]
  assertBool ("expected a W8 arithmetic primitive in L2, found " ++ show ws)
             (W8 `elem` ws)
  -- And the L2 typechecker accepts it.
  r <- try (evaluate (length (sdoc (fst (defaultPackedRunPassM (L2.tcProg p2))))))
  case r of
    Left (e :: SomeException) -> assertFailure ("L2 typechecking failed: " ++ show e)
    Right _ -> pure ()

-- | Width-sensitive primitives of an L2 program.
l2Prims :: L2.Prog2 -> [Prim L2.Ty2]
l2Prims Prog{fundefs,mainExp} =
  concatMap (goE . funBody) (M.elems fundefs) ++ maybe [] (goE . fst) mainExp
  where
    goE ex =
      case ex of
        PrimAppE pr args -> [pr | isWidthSensitivePrim pr] ++ concatMap goE args
        AppE _ _ _ args    -> concatMap goE args
        LetE (_,_,_,rhs) b -> goE rhs ++ goE b
        IfE a b c  -> goE a ++ goE b ++ goE c
        MkProdE ls -> concatMap goE ls
        ProjE _ e  -> goE e
        CaseE sc brs -> goE sc ++ concatMap (\(_,_,r) -> goE r) brs
        DataConE _ _ args -> concatMap goE args
        TimeIt e _ _   -> goE e
        WithArenaE _ e -> goE e
        SpawnE _ _ args -> concatMap goE args
        _ -> []

--------------------------------------------------------------------------------
-- Malformed IR: annotation and operands disagree
--------------------------------------------------------------------------------
--
-- These build the IR directly, bypassing L0, to prove that a downstream
-- typechecker validates the annotation rather than re-deriving a width from
-- the operands (or assuming W64).
--
-- NB: these stop at L3.  L4 is still width-less, so nothing here says anything
-- about narrow-width code generation, which remains unsafe.

ddfsW :: DDefs Ty1
ddfsW = M.empty

envW8 :: Env2 Var Ty1
envW8 = Env2 (M.fromList [("a", IntTy W8), ("b", IntTy W8),
                          ("c", IntTy W16), ("d", IntTy W16)]) M.empty

l1tc :: Exp1 -> Either (L1.TCError Exp1) Ty1
l1tc = runExcept . L1.tcExp ddfsW envW8

-- | Well-formed: W8 AddP over W8 operands.
case_l1_accepts_matching_annotation :: Assertion
case_l1_accepts_matching_annotation =
  case l1tc (PrimAppE (AddP (IntPrimWidth W8)) [VarE "a", VarE "b"]) of
    Right t -> IntTy W8 @=? t
    Left e  -> assertFailure ("expected this to typecheck: " ++ show e)

-- | Malformed: W8 AddP over W16 operands.
case_l1_rejects_annotation_operand_mismatch :: Assertion
case_l1_rejects_annotation_operand_mismatch =
  case l1tc (PrimAppE (AddP (IntPrimWidth W8)) [VarE "c", VarE "d"]) of
    Left _  -> pure ()
    Right t -> assertFailure ("expected a type error, got " ++ show t)

-- | Malformed: operands of different widths.
case_l1_rejects_heterogeneous_operands :: Assertion
case_l1_rejects_heterogeneous_operands =
  case l1tc (PrimAppE (AddP (IntPrimWidth W8)) [VarE "a", VarE "c"]) of
    Left _  -> pure ()
    Right t -> assertFailure ("expected a type error, got " ++ show t)

-- | Malformed: comparison annotation disagrees with operands.
case_l1_rejects_cmp_mismatch :: Assertion
case_l1_rejects_cmp_mismatch =
  case l1tc (PrimAppE (LtP (IntPrimWidth W32)) [VarE "a", VarE "b"]) of
    Left _  -> pure ()
    Right t -> assertFailure ("expected a type error, got " ++ show t)

-- | Malformed: PrintInt annotated at the wrong width.
case_l1_rejects_printint_mismatch :: Assertion
case_l1_rejects_printint_mismatch =
  case l1tc (PrimAppE (PrintInt (IntPrimWidth W32)) [VarE "a"]) of
    Left _  -> pure ()
    Right t -> assertFailure ("expected a type error, got " ++ show t)

-- | Malformed: an unresolved annotation must never typecheck past L0.
case_l1_rejects_unresolved_annotation :: Assertion
case_l1_rejects_unresolved_annotation =
  case l1tc (PrimAppE (AddP IntPrimUnresolved) [VarE "a", VarE "b"]) of
    Left _  -> pure ()
    Right t -> assertFailure ("expected a type error, got " ++ show t)

-- L2 ---------------------------------------------------------------------

l2tc :: L2.Exp2 -> Either L2.TCError (L2.Ty2, L2.LocationTypeState)
l2tc e = runExcept (L2.tcExp M.empty env M.empty (L2.ConstraintSet S.empty)
                             (L2.RegionSet S.empty) (L2.LocationTypeState M.empty) e)
  where env = Env2 (M.fromList [(fromVarToFreeVarsTy "a", (IntTy W8 :: L2.Ty2)),
                                (fromVarToFreeVarsTy "b", (IntTy W8 :: L2.Ty2)),
                                (fromVarToFreeVarsTy "c", (IntTy W16 :: L2.Ty2))]) M.empty

case_l2_accepts_matching_annotation :: Assertion
case_l2_accepts_matching_annotation =
  case l2tc (PrimAppE (AddP (IntPrimWidth W8)) [VarE "a", VarE "b"]) of
    Right (t,_) -> (IntTy W8 :: L2.Ty2) @=? t
    Left e      -> assertFailure ("expected this to typecheck: " ++ show e)

case_l2_rejects_annotation_operand_mismatch :: Assertion
case_l2_rejects_annotation_operand_mismatch =
  case l2tc (PrimAppE (AddP (IntPrimWidth W16)) [VarE "a", VarE "b"]) of
    Left _      -> pure ()
    Right (t,_) -> assertFailure ("expected a type error, got " ++ show t)

case_l2_rejects_unresolved_annotation :: Assertion
case_l2_rejects_unresolved_annotation =
  case l2tc (PrimAppE (AddP IntPrimUnresolved) [VarE "a", VarE "b"]) of
    Left _      -> pure ()
    Right (t,_) -> assertFailure ("expected a type error, got " ++ show t)

-- L3 ---------------------------------------------------------------------

l3tc :: L3.Exp3 -> Either (L1.TCError L3.Exp3) L3.Ty3
l3tc = runExcept . L3.tcExp True M.empty env
  where env = Env2 (M.fromList [("a", IntTy W8), ("b", IntTy W8),
                                ("c", IntTy W16)]) M.empty

case_l3_accepts_matching_annotation :: Assertion
case_l3_accepts_matching_annotation =
  case l3tc (PrimAppE (AddP (IntPrimWidth W8)) [VarE "a", VarE "b"]) of
    Right t -> IntTy W8 @=? t
    Left e  -> assertFailure ("expected this to typecheck: " ++ show e)

case_l3_rejects_annotation_operand_mismatch :: Assertion
case_l3_rejects_annotation_operand_mismatch =
  case l3tc (PrimAppE (AddP (IntPrimWidth W16)) [VarE "a", VarE "b"]) of
    Left _  -> pure ()
    Right t -> assertFailure ("expected a type error, got " ++ show t)

case_l3_rejects_heterogeneous_operands :: Assertion
case_l3_rejects_heterogeneous_operands =
  case l3tc (PrimAppE (AddP (IntPrimWidth W8)) [VarE "a", VarE "c"]) of
    Left _  -> pure ()
    Right t -> assertFailure ("expected a type error, got " ++ show t)

case_l3_rejects_unresolved_annotation :: Assertion
case_l3_rejects_unresolved_annotation =
  case l3tc (PrimAppE (AddP IntPrimUnresolved) [VarE "a", VarE "b"]) of
    Left _  -> pure ()
    Right t -> assertFailure ("expected a type error, got " ++ show t)

-- | Recovery must use the annotation, not a W64 fallback.
case_recover_type_uses_prim_annotation :: Assertion
case_recover_type_uses_prim_annotation = do
  IntTy W8  @=? gRecoverType ddfs env0 (PrimAppE (AddP (IntPrimWidth W8))  [LitE (LitWidth W8) 1, LitE (LitWidth W8) 2] :: Exp1)
  IntTy W16 @=? gRecoverType ddfs env0 (PrimAppE (MulP (IntPrimWidth W16)) [LitE (LitWidth W16) 1, LitE (LitWidth W16) 2] :: Exp1)
  IntTy W32 @=? gRecoverType ddfs env0 (PrimAppE (SubP (IntPrimWidth W32)) [LitE (LitWidth W32) 1, LitE (LitWidth W32) 2] :: Exp1)
  BoolTy    @=? gRecoverType ddfs env0 (PrimAppE (LtP  (IntPrimWidth W8))  [LitE (LitWidth W8) 1, LitE (LitWidth W8) 2] :: Exp1)

-- | primArgsTy / primRetTy read the annotation.
case_prim_arg_ret_tys_use_annotation :: Assertion
case_prim_arg_ret_tys_use_annotation = do
  [IntTy W8, IntTy W8] @=? (primArgsTy (AddP (IntPrimWidth W8)) :: [Ty1])
  IntTy W8             @=? (primRetTy  (AddP (IntPrimWidth W8)) :: Ty1)
  [IntTy W16, IntTy W16] @=? (primArgsTy (GtEqP (IntPrimWidth W16)) :: [Ty1])
  BoolTy               @=? (primRetTy  (GtEqP (IntPrimWidth W16)) :: Ty1)
  [IntTy W32]          @=? (primArgsTy (PrintInt (IntPrimWidth W32)) :: [Ty1])

intWidthTests :: TestTree
intWidthTests = $(testGroupGenerator)

-- Silence the unused-import warning for a helper kept for local debugging.
_unusedUnsafePerformIO :: IO a -> a
_unusedUnsafePerformIO = unsafePerformIO
