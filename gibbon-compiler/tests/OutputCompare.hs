-- | The single correctness oracle for Gibbon's example harness.
--
-- Note [Comparing answers]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
-- A Gibbon program that uses @iterate@ / @timeit@ prints timing records
-- interleaved with its ordinary output.  Those records are nondeterministic:
-- re-running the same binary produces different numbers.  Everything else a
-- program prints is deterministic and is the thing the test suite exists to
-- check.
--
-- This module implements exactly that split.  The invariant it enforces:
--
--   * a /whole line/ that is nothing but a recognized timing record is
--     removed from both sides before comparing;
--   * every remaining token, its multiplicity, and its order must agree.
--
-- The presence of timing output never authorizes ignoring an unrelated value
-- difference.  Anything that is not recognized as a complete timing record --
-- a malformed payload, a near-miss prefix, a record with extra text glued to
-- it, or an ordinary data line that merely mentions @SELFTIMED@ -- is
-- retained and compared like any other output.
--
-- Historically this was two different comparisons: 'runTestAgainstAnswer'
-- shelled out to @diff@ and then threw the result away if the diff text
-- mentioned any timing token anywhere (VW-03: a wrong value plus a timing line
-- was silently accepted), while the baseline path deleted any line containing
-- @SELFTIMED@ as a substring (which silently deleted ordinary data).  Both now
-- go through 'compareNormalized' below.
--
-- Note [Timing-output grammar]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The grammar below is derived from the actual emitters, not invented:
--
--   * @gibbon-rts/rts-c/gibbon_rts.c:gib_print_timing_array@
--       @printf("ITER TIMES: [")@, @"%f"@ elements joined by @", "@, @"]\n"@.
--       The list is empty when the vector is empty.
--   * @Gibbon.Passes.Codegen@ (LetTimedT)
--       @printf("BATCHTIME: %e\n", ..)@ and @printf("SELFTIMED: %e\n", ..)@.
--   * @Gibbon.L1.Interp@, @Gibbon.L2.Interp@, @Gibbon.L4.Interp@
--       @"BATCHTIME: " ++ show (tm :: Double)@ and likewise @SELFTIMED@.
--
-- so the payload grammar is the union of C @%f@, C @%e@ and Haskell @show@
-- for a 'Double', all of which always emit a decimal point:
--
-- @
--   num  := '-'? digit+ '.' digit+ ( [eE] [+-]? digit+ )?
-- @
--
-- Deliberately /not/ accepted, because no audited emitter can produce them and
-- accepting them would let real output disappear: a missing decimal point,
-- @NaN@, @Infinity@, @inf@, @nan@, a leading @+@, or an empty payload.  Both
-- interpreters compute @nanoseconds / 10e9@ and the C side computes a
-- difference of two @CLOCK_MONOTONIC_RAW@ timespecs, so neither can be
-- non-finite.  A non-finite reading is therefore a real anomaly and must
-- surface as a mismatch rather than vanish.
--
-- @ITERS:@ and @SIZE:@ are deliberately /not/ timing records.  They are
-- deterministic run configuration, they are recorded in answer files, and a
-- wrong iteration count or size parameter is exactly the kind of bad run this
-- oracle should catch.
module OutputCompare
  ( -- * Timing-record grammar
    TimingRecord(..)
  , recognizeTimingRecord
  , isTimingRecordLine
    -- * Normalization
  , normalizeOutput
  , normalizedTokens
    -- * Comparison
  , compareNormalized
  , compareOutputFiles
  ) where

import           Control.Exception ( SomeException, try )
import           Control.Monad ( guard )
import           Data.Char ( isDigit )
import           Data.List ( stripPrefix )
import           System.Directory ( doesFileExist )
import           System.FilePath ( takeDirectory, takeFileName, (</>) )
import           System.Process ( readProcessWithExitCode )

--------------------------------------------------------------------------------
-- The grammar
--------------------------------------------------------------------------------

-- | A complete, well-formed timing record occupying an entire line.  The
-- payloads are kept as the literal text that was matched so that callers can
-- report what was ignored.
data TimingRecord
    = IterTimes [String]   -- ^ @ITER TIMES: [..]@ (possibly empty)
    | BatchTime String     -- ^ @BATCHTIME: <num>@
    | SelfTimed String     -- ^ @SELFTIMED: <num>@
  deriving (Eq, Show)

-- | Horizontal whitespace.  @\\r@ is included so CRLF input behaves like LF
-- input; vertical whitespace cannot occur inside a line.
isHoriz :: Char -> Bool
isHoriz c = c == ' ' || c == '\t' || c == '\r'

trimHoriz :: String -> String
trimHoriz = dropWhileEnd' . dropWhile isHoriz
  where dropWhileEnd' = reverse . dropWhile isHoriz . reverse

-- | Recognize an /entire/ line as a timing record.  Leading and trailing
-- horizontal whitespace is permitted; nothing else is.  Returns 'Nothing' for
-- anything the audited emitters cannot produce, which is then compared
-- normally.
recognizeTimingRecord :: String -> Maybe TimingRecord
recognizeTimingRecord raw
    | Just r <- stripPrefix "ITER TIMES:" s = IterTimes <$> parseTimeList (dropWhile isHoriz r)
    | Just r <- stripPrefix "BATCHTIME:"  s = BatchTime <$> parseWholeNum (dropWhile isHoriz r)
    | Just r <- stripPrefix "SELFTIMED:"  s = SelfTimed <$> parseWholeNum (dropWhile isHoriz r)
    | otherwise                             = Nothing
  where
    s = trimHoriz raw

isTimingRecordLine :: String -> Bool
isTimingRecordLine = maybe False (const True) . recognizeTimingRecord

-- | @[]@, or @[num, num, ..]@.  Nothing may follow the closing bracket.
parseTimeList :: String -> Maybe [String]
parseTimeList s0 = do
    s1 <- stripPrefix "[" s0
    let (inside, rest) = break (== ']') s1
    guard ("]" == rest)
    if all isHoriz inside
      then Just []
      else mapM (parseWholeNum . trimHoriz) (splitOnComma inside)

splitOnComma :: String -> [String]
splitOnComma s = case break (== ',') s of
                   (chunk, [])       -> [chunk]
                   (chunk, _:remain) -> chunk : splitOnComma remain

-- | The whole string must be one number, with no trailing text.
parseWholeNum :: String -> Maybe String
parseWholeNum s = do
    rest <- pNum s
    guard (null rest)
    pure s

-- | Consume one number, returning what is left.  See Note [Timing-output
-- grammar] for why the decimal point is mandatory and why there is no @+@,
-- @NaN@ or @Infinity@ case.
pNum :: String -> Maybe String
pNum s0 = do
    let s1 = case s0 of
               ('-':r) -> r
               _       -> s0
    s2 <- digits1 s1
    s3 <- stripPrefix "." s2
    s4 <- digits1 s3
    case s4 of
      (c:r) | c == 'e' || c == 'E' ->
        let r1 = case r of
                   (sg:rr) | sg == '+' || sg == '-' -> rr
                   _                                -> r
        in digits1 r1
      _ -> pure s4
  where
    digits1 s = case span isDigit s of
                  ([], _)   -> Nothing
                  (_,  rest) -> Just rest

--------------------------------------------------------------------------------
-- Normalization and comparison
--------------------------------------------------------------------------------

-- | Drop whole-line timing records; keep everything else verbatim.
normalizeOutput :: String -> String
normalizeOutput = unlines . filter (not . isTimingRecordLine) . lines

-- | The comparison unit.
--
-- This is the harness's long-established whitespace policy, unchanged: the
-- answer path used @diff -B -w@ (ignore blank lines, ignore whitespace) and
-- the baseline path used @words@.  Tokenizing keeps both properties while
-- being strictly /stronger/ than @diff -w@, which would consider @\"a b\"@ and
-- @\"ab\"@ equal; token boundaries are meaningful in Gibbon's packed printers,
-- so they are preserved.
normalizedTokens :: String -> [String]
normalizedTokens = words . normalizeOutput

-- | The one comparison used by every caller.  'Nothing' means the outputs
-- agree on every deterministic token, in order.
--
-- @what@ names the expected side for the diagnostic ("answer file",
-- "baseline", ..).
compareNormalized :: String -> String -> String -> Maybe String
compareNormalized what expected actual
    | expTok == actTok = Nothing
    | otherwise = Just $ unlines $
        [ "Output differed from the " ++ what
          ++ " after removing timing records (ITER TIMES/BATCHTIME/SELFTIMED)."
        , firstDifference expTok actTok
        , "Expected normalized output:"
        , normalizeOutput expected
        , "Actual normalized output:"
        , normalizeOutput actual
        ]
  where
    expTok = normalizedTokens expected
    actTok = normalizedTokens actual

-- | Point at the first token that differs, with a little context, so a large
-- output does not have to be eyeballed.
firstDifference :: [String] -> [String] -> String
firstDifference expTok actTok = go (0 :: Int) expTok actTok
  where
    go i (e:es) (a:as)
      | e == a    = go (i+1) es as
      | otherwise = "First difference at token " ++ show i ++ ": expected "
                    ++ show e ++ ", got " ++ show a
                    ++ ctx i
    go i [] (a:_) = "Extra output: expected " ++ show i
                    ++ " tokens, got more, starting with " ++ show a ++ ctx i
    go i (e:_) [] = "Missing output: expected token " ++ show i ++ " ("
                    ++ show e ++ ") but output ended" ++ ctx i
    go _ [] []    = "No token difference (this should be unreachable)"

    ctx i = "\n  expected around here: " ++ unwords (window i expTok)
         ++ "\n  actual   around here: " ++ unwords (window i actTok)
    window i = take 7 . drop (max 0 (i - 3))

--------------------------------------------------------------------------------
-- The file-to-answer path
--------------------------------------------------------------------------------

-- | Compare a program's captured output file against an answer file.  This is
-- the exact function 'TestRunner.runTestAgainstAnswer' calls, so a unit test
-- that calls it exercises the real path.
--
-- Correctness is decided entirely by 'compareNormalized'.  The external @diff@
-- is only a renderer, run after inequality has already been established, over
-- the /normalized/ text; if it cannot run, the pure diagnostic still stands.
compareOutputFiles :: FilePath -> FilePath -> IO (Maybe String)
compareOutputFiles actualPath expectedPath = do
    expectedExists <- doesFileExist expectedPath
    if not expectedExists
      then pure (Just ("File does not exist: " ++ expectedPath))
      else do
        actual   <- readFile actualPath
        expected <- readFile expectedPath
        case compareNormalized "answer file" expected actual of
          Nothing  -> pure Nothing
          Just msg -> do
            rendered <- renderDiff actualPath expected actual
            pure (Just (msg ++ rendered))

-- | Best-effort pretty diff of the normalized texts, for readability only.
renderDiff :: FilePath -> String -> String -> IO String
renderDiff actualPath expected actual = do
    r <- try attempt :: IO (Either SomeException String)
    pure (either (const "") id r)
  where
    attempt = do
      -- Written beside the actual output, which always lives in the harness's
      -- writable tempdir; the answer file's directory may be read-only.
      let dir = takeDirectory actualPath
          ep  = dir </> takeFileName actualPath ++ ".normalized-expected"
          ap  = dir </> takeFileName actualPath ++ ".normalized-actual"
      writeFile ep (normalizeOutput expected)
      writeFile ap (normalizeOutput actual)
      (_, out, _) <- readProcessWithExitCode "diff" ["-u", ep, ap] ""
      pure $ if null out
             then ""
             else "Normalized diff (expected vs actual):\n" ++ out
