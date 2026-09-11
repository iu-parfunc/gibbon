{-# LANGUAGE TemplateHaskell #-}

-- | Tests for the example harness's correctness oracle (VW-03).
--
-- These exercise both the pure comparator and the /real/ file-to-answer entry
-- point 'compareOutputFiles' that @TestRunner.runTestAgainstAnswer@ calls, so
-- there is no second implementation that could drift.
--
-- Every "wrong value plus timing output" case here was ACCEPTED before the fix
-- (the old 'diff' threw away any difference whose diff text mentioned a timing
-- token), and every "timing word appears in ordinary data" case had that data
-- silently deleted (the old 'isTimingLine' was an unanchored 'isInfixOf').
module OutputCompareTests where

import           System.Directory
import           System.FilePath
import           System.IO.Unsafe ( unsafePerformIO )

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.TH

import           OutputCompare

--------------------------------------------------------------------------------
-- Helpers

-- | Pure comparison, answer-file semantics.
cmp :: String -> String -> Maybe String
cmp = compareNormalized "answer file"

accepts :: String -> String -> Assertion
accepts expected actual =
    case cmp expected actual of
      Nothing  -> pure ()
      Just msg -> assertFailure ("expected these to compare equal:\n" ++ msg)

rejects :: String -> String -> Assertion
rejects expected actual =
    case cmp expected actual of
      Nothing -> assertFailure $
        "value difference was silently accepted!\n  expected: " ++ show expected
        ++ "\n  actual:   " ++ show actual
      Just _  -> pure ()

-- | Run the same file-to-answer path the example harness uses.
viaFiles :: String -> String -> String -> IO (Maybe String)
viaFiles tag expected actual = do
    let dir = testTmpDir </> tag
    createDirectoryIfMissing True dir
    let ap = dir </> "prog.out"
        ep = dir </> "prog.ans"
    writeFile ap actual
    writeFile ep expected
    compareOutputFiles ap ep

testTmpDir :: FilePath
testTmpDir = unsafePerformIO $ do
    tmp <- getTemporaryDirectory
    pure (tmp </> "gibbon-output-compare-tests")
{-# NOINLINE testTmpDir #-}

fileAccepts :: String -> String -> String -> Assertion
fileAccepts tag expected actual = do
    r <- viaFiles tag expected actual
    case r of
      Nothing  -> pure ()
      Just msg -> assertFailure ("expected these files to compare equal:\n" ++ msg)

fileRejects :: String -> String -> String -> Assertion
fileRejects tag expected actual = do
    r <- viaFiles tag expected actual
    case r of
      Nothing -> assertFailure $
        "file comparison silently accepted a value difference (" ++ tag ++ ")"
      Just _  -> pure ()

-- Realistic timing blocks, copied from real emitters / answer files.
cTiming :: String
cTiming = unlines [ "ITER TIMES: [0.000000]"
                  , "ITERS: 1"
                  , "SIZE: 1"
                  , "BATCHTIME: 4.200000e-07"
                  , "SELFTIMED: 4.200000e-07" ]

cTiming' :: String
cTiming' = unlines [ "ITER TIMES: [0.000013]"
                   , "ITERS: 1"
                   , "SIZE: 1"
                   , "BATCHTIME: 1.337000e-05"
                   , "SELFTIMED: 1.337000e-05" ]

--------------------------------------------------------------------------------
-- The grammar itself

case_grammar_accepts_real_emitter_forms :: Assertion
case_grammar_accepts_real_emitter_forms = do
    -- C RTS: gib_print_timing_array, "%f" joined by ", "
    Just (IterTimes ["0.000000"])              @=? recognizeTimingRecord "ITER TIMES: [0.000000]"
    Just (IterTimes ["0.000000","1.250000"])   @=? recognizeTimingRecord "ITER TIMES: [0.000000, 1.250000]"
    Just (IterTimes [])                        @=? recognizeTimingRecord "ITER TIMES: []"
    -- C codegen: printf("%e")
    Just (BatchTime "4.200000e-07")            @=? recognizeTimingRecord "BATCHTIME: 4.200000e-07"
    Just (SelfTimed "1.170000e-07")            @=? recognizeTimingRecord "SELFTIMED: 1.170000e-07"
    -- Haskell interpreters: show (tm :: Double)
    Just (SelfTimed "0.0")                     @=? recognizeTimingRecord "SELFTIMED: 0.0"
    Just (BatchTime "1.234e-2")                @=? recognizeTimingRecord "BATCHTIME: 1.234e-2"
    Just (SelfTimed "3.5e7")                   @=? recognizeTimingRecord "SELFTIMED: 3.5e7"
    -- a negative reading is still a timing record
    Just (SelfTimed "-0.000000")               @=? recognizeTimingRecord "SELFTIMED: -0.000000"

case_grammar_allows_surrounding_horizontal_space_and_crlf :: Assertion
case_grammar_allows_surrounding_horizontal_space_and_crlf = do
    True @=? isTimingRecordLine "   SELFTIMED: 1.0e-2"
    True @=? isTimingRecordLine "\tBATCHTIME: 1.0e-2   "
    True @=? isTimingRecordLine "SELFTIMED: 1.0e-2\r"
    True @=? isTimingRecordLine "ITER TIMES: [0.000000]\r"

case_grammar_rejects_malformed_payloads :: Assertion
case_grammar_rejects_malformed_payloads = do
    Nothing @=? recognizeTimingRecord "SELFTIMED:"
    Nothing @=? recognizeTimingRecord "SELFTIMED: "
    Nothing @=? recognizeTimingRecord "SELFTIMED: abc"
    Nothing @=? recognizeTimingRecord "SELFTIMED: 4.2.3"
    Nothing @=? recognizeTimingRecord "SELFTIMED: 42"        -- no emitter drops the point
    Nothing @=? recognizeTimingRecord "SELFTIMED: .5"
    Nothing @=? recognizeTimingRecord "SELFTIMED: 1.0e"
    Nothing @=? recognizeTimingRecord "SELFTIMED: +1.0"
    -- no audited emitter can produce a non-finite reading; if one appears it is
    -- an anomaly and must be compared, not deleted.
    Nothing @=? recognizeTimingRecord "SELFTIMED: NaN"
    Nothing @=? recognizeTimingRecord "SELFTIMED: Infinity"
    Nothing @=? recognizeTimingRecord "SELFTIMED: nan"
    Nothing @=? recognizeTimingRecord "SELFTIMED: inf"
    -- malformed lists
    Nothing @=? recognizeTimingRecord "ITER TIMES: [0.000000"
    Nothing @=? recognizeTimingRecord "ITER TIMES: 0.000000]"
    Nothing @=? recognizeTimingRecord "ITER TIMES: [0.000000,]"
    Nothing @=? recognizeTimingRecord "ITER TIMES: [oops]"

case_grammar_rejects_near_miss_prefixes_and_suffixes :: Assertion
case_grammar_rejects_near_miss_prefixes_and_suffixes = do
    Nothing @=? recognizeTimingRecord "MY SELFTIMED: 1.0e-2"
    Nothing @=? recognizeTimingRecord "SELFTIMEDX: 1.0e-2"
    Nothing @=? recognizeTimingRecord "SELFTIMED 1.0e-2"
    Nothing @=? recognizeTimingRecord "SELFTIMED: 1.0e-2 extra"
    Nothing @=? recognizeTimingRecord "SELFTIMED: 1.0e-2 SELFTIMED: 1.0e-2"
    Nothing @=? recognizeTimingRecord "ITER TIMES: [0.0] and more"
    Nothing @=? recognizeTimingRecord "result SELFTIMED: 3.0"

case_iters_and_size_are_not_timing_records :: Assertion
case_iters_and_size_are_not_timing_records = do
    Nothing @=? recognizeTimingRecord "ITERS: 1"
    Nothing @=? recognizeTimingRecord "SIZE: 1"
    -- and therefore a wrong iteration count or size parameter still fails
    rejects ("ITERS: 1\nSIZE: 1\n42\n") ("ITERS: 2\nSIZE: 1\n42\n")
    rejects ("ITERS: 1\nSIZE: 1\n42\n") ("ITERS: 1\nSIZE: 25\n42\n")

--------------------------------------------------------------------------------
-- The invariant: timing may vary, values may not

case_same_value_different_timing_passes :: Assertion
case_same_value_different_timing_passes =
    accepts (cTiming ++ "5151\n") (cTiming' ++ "5151\n")

case_same_value_timing_present_on_one_side_only :: Assertion
case_same_value_timing_present_on_one_side_only = do
    -- Only the nondeterministic records are ignorable, so a side that has them
    -- and a side that does not still agree.
    accepts "5151\n" ("SELFTIMED: 4.200000e-07\n5151\n")
    accepts ("ITER TIMES: [0.000000]\nBATCHTIME: 1.0e-2\nSELFTIMED: 1.0e-2\n5151\n")
            "5151\n"

-- | ITERS/SIZE are deterministic, so a one-sided ITERS/SIZE block is a real
-- difference.  This is the behaviour that exposes answer files recorded before
-- a program grew an @iterate@, and it is deliberate: see
-- Note [Timing-output grammar].
case_one_sided_iters_size_block_is_a_real_difference :: Assertion
case_one_sided_iters_size_block_is_a_real_difference = do
    rejects "5151\n" (cTiming ++ "5151\n")
    accepts (cTiming ++ "5151\n") (cTiming' ++ "5151\n")

case_wrong_scalar_value_with_timing_fails :: Assertion
case_wrong_scalar_value_with_timing_fails = do
    rejects (cTiming ++ "5151\n") (cTiming' ++ "5152\n")
    rejects ("BATCHTIME: 4.2e-7\n42\n") ("BATCHTIME: 4.2e-7\n43\n")
    rejects ("SELFTIMED: 4.2e-7\n42\n") ("SELFTIMED: 4.2e-7\n43\n")
    rejects ("ITER TIMES: [0.0]\n42\n")  ("ITER TIMES: [0.0]\n43\n")

case_wrong_printer_text_with_timing_fails :: Assertion
case_wrong_printer_text_with_timing_fails =
    rejects (cTiming ++ "(Node 1 2 (Leaf 3) (Leaf 4))\n")
            (cTiming' ++ "(Node 1 2 (Leaf 3) (Leaf 5))\n")

case_missing_value_with_timing_fails :: Assertion
case_missing_value_with_timing_fails =
    rejects (cTiming ++ "1\n2\n3\n") (cTiming' ++ "1\n2\n")

case_duplicated_value_with_timing_fails :: Assertion
case_duplicated_value_with_timing_fails =
    rejects (cTiming ++ "42\n") (cTiming' ++ "42\n42\n")

case_reordered_values_with_timing_fails :: Assertion
case_reordered_values_with_timing_fails =
    rejects (cTiming ++ "1\n2\n") (cTiming' ++ "2\n1\n")

case_timing_token_embedded_in_program_data_is_compared :: Assertion
case_timing_token_embedded_in_program_data_is_compared = do
    -- the pre-fix baseline path deleted these lines entirely
    rejects "result SELFTIMED: 3\n" "result SELFTIMED: 4\n"
    rejects "the BATCHTIME word 7\n" "the BATCHTIME word 8\n"
    rejects "ITER TIMES: [0.0] trailing 1\n" "ITER TIMES: [0.0] trailing 2\n"
    -- and equal embedded text still passes
    accepts "result SELFTIMED: 3\n" "result SELFTIMED: 3\n"

case_malformed_timing_line_is_retained_and_compared :: Assertion
case_malformed_timing_line_is_retained_and_compared = do
    rejects "SELFTIMED: abc\n42\n" "SELFTIMED: xyz\n42\n"
    accepts "SELFTIMED: abc\n42\n" "SELFTIMED: abc\n42\n"

case_multiple_timing_records_all_removed :: Assertion
case_multiple_timing_records_all_removed =
    accepts (concat [ "SELFTIMED: 1.0e-2\n", "SELFTIMED: 2.0e-2\n"
                    , "ITER TIMES: [1.000000, 2.000000]\n", "7\n" ])
            (concat [ "SELFTIMED: 9.0e-9\n", "SELFTIMED: 8.0e-9\n"
                    , "ITER TIMES: [3.000000, 4.000000, 5.000000]\n", "7\n" ])

case_crlf_input_compares_like_lf :: Assertion
case_crlf_input_compares_like_lf = do
    accepts "SELFTIMED: 1.0e-2\n42\n" "SELFTIMED: 2.0e-2\r\n42\r\n"
    rejects "SELFTIMED: 1.0e-2\n42\n" "SELFTIMED: 2.0e-2\r\n43\r\n"

case_timing_free_outputs_unchanged :: Assertion
case_timing_free_outputs_unchanged = do
    accepts "42\n" "42\n"
    rejects "42\n" "43\n"
    -- the established whitespace policy: blank lines and layout are ignored
    accepts "(sum: 5)\n\n42\n" "(sum: 5)\n42\n"
    accepts "1 2 3\n" "1\n2\n3\n"
    -- ..but token boundaries are not erased
    rejects "1 2\n" "12\n"

--------------------------------------------------------------------------------
-- The real file-to-answer path used by runTestAgainstAnswer

case_file_path_accepts_timing_only_difference :: Assertion
case_file_path_accepts_timing_only_difference =
    fileAccepts "timing-only" (cTiming ++ "5151\n") (cTiming' ++ "5151\n")

case_file_path_rejects_wrong_value_with_batchtime :: Assertion
case_file_path_rejects_wrong_value_with_batchtime =
    fileRejects "batchtime" "42\n" "BATCHTIME: 4.200000e-07\n43\n"

case_file_path_rejects_wrong_value_with_selftimed :: Assertion
case_file_path_rejects_wrong_value_with_selftimed =
    fileRejects "selftimed" "42\n" "SELFTIMED: 4.200000e-07\n43\n"

case_file_path_rejects_wrong_value_with_iter_times :: Assertion
case_file_path_rejects_wrong_value_with_iter_times =
    fileRejects "itertimes" "42\n" "ITER TIMES: [0.000000]\n43\n"

case_file_path_rejects_duplicate_and_reorder_with_timing :: Assertion
case_file_path_rejects_duplicate_and_reorder_with_timing = do
    fileRejects "dup"     "42\n"    "SELFTIMED: 1.0e-2\n42\n42\n"
    fileRejects "reorder" "1\n2\n"  "SELFTIMED: 1.0e-2\n2\n1\n"

case_file_path_rejects_timing_free_wrong_value :: Assertion
case_file_path_rejects_timing_free_wrong_value =
    fileRejects "plain" "42\n" "43\n"

case_file_path_reports_missing_answer_file :: Assertion
case_file_path_reports_missing_answer_file = do
    let dir = testTmpDir </> "missing"
    createDirectoryIfMissing True dir
    let ap = dir </> "prog.out"
    writeFile ap "42\n"
    r <- compareOutputFiles ap (dir </> "nope.ans")
    case r of
      Nothing -> assertFailure "a missing answer file must not be a pass"
      Just m  -> assertBool ("unexpected message: " ++ m)
                            (take 21 m == "File does not exist: ")

case_file_path_diagnostic_names_the_differing_token :: Assertion
case_file_path_diagnostic_names_the_differing_token = do
    r <- viaFiles "diagnostic" (cTiming ++ "1\n2\n3\n") (cTiming' ++ "1\n9\n3\n")
    case r of
      Nothing  -> assertFailure "value difference was accepted"
      Just msg -> do
        assertBool "diagnostic should locate the differing token"
                   (substr "expected \"2\", got \"9\"" msg)
        assertBool "diagnostic should not leak nondeterministic timing numbers"
                   (not (substr "4.200000e-07" msg))

substr :: String -> String -> Bool
substr needle hay = any (pre needle) (suffixes hay)
  where
    suffixes s = s : case s of { [] -> [] ; (_:t) -> suffixes t }
    pre [] _ = True
    pre _ [] = False
    pre (x:xs) (y:ys) = x == y && pre xs ys

--------------------------------------------------------------------------------

outputCompareTests :: TestTree
outputCompareTests = $(testGroupGenerator)
