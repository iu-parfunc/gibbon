{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

-- | The compiler pipeline, assembled from several passes.

module Packed.FirstOrder.Compiler
    ( -- * Compiler entrypoints
      compile, compileCmd
     -- * Configuration options and parsing
     , Config (..), Mode(..), Input(..)
     , configParser, configWithArgs, defaultConfig
    )
  where

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad.State.Strict
import           Data.Set as S hiding (map)
import           Options.Applicative
import           Packed.FirstOrder.Common
import qualified Packed.FirstOrder.HaskellFrontend as HS
import qualified Packed.FirstOrder.L1_Source   as L1
import qualified Packed.FirstOrder.L2_Traverse as L2
import qualified Packed.FirstOrder.L3_Target   as L3
import qualified Packed.FirstOrder.SExpFrontend as SExp
import qualified Packed.FirstOrder.SourceInterp as SI
import           Packed.FirstOrder.TargetInterp (Val (..), execProg)
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.IO.Error (isDoesNotExistError)
import           System.Process
import           Text.PrettyPrint.GenericPretty

-- compiler passes
import           Packed.FirstOrder.Passes.Freshen
import           Packed.FirstOrder.Passes.Flatten

-- UNDER_CONSTRUCTION
-- import           Packed.FirstOrder.Passes.Codegen (codegenProg)
-- #ifdef LLVM_ENABLED
-- import qualified Packed.FirstOrder.Passes.LLVM.Codegen as LLVM
-- #endif
-- import           Packed.FirstOrder.Passes.Cursorize
-- import           Packed.FirstOrder.Passes.FindWitnesses (findWitnesses)
-- import           Packed.FirstOrder.Passes.HoistNewBuf
-- import           Packed.FirstOrder.Passes.InferEffects (inferEffects)
-- import           Packed.FirstOrder.Passes.InlinePacked
-- import           Packed.FirstOrder.Passes.CopyInsertion
-- import           Packed.FirstOrder.Passes.InlineTriv
-- import           Packed.FirstOrder.Passes.Lower
-- import           Packed.FirstOrder.Passes.RouteEnds (routeEnds)
-- import           Packed.FirstOrder.Passes.ShakeTree
-- import           Packed.FirstOrder.Passes.Typecheck
-- import           Packed.FirstOrder.Passes.Unariser

----------------------------------------
-- PASS STUBS
----------------------------------------
-- All of these need to be implemented, but are just the identity
-- function for now.  Move to Passes/ when implemented


-- | Find all local variables bound by case expressions which must be
-- traversed, but which are not by the current program.
findMissingTraversals :: L2.Prog -> SyM (Set Var)
findMissingTraversals _ = pure S.empty

-- | Add calls to an implicitly-defined, polymorphic "traverse"
-- function of type `p -> ()` for any packed type p.
addTraversals :: Set Var -> L2.Prog -> SyM L2.Prog
addTraversals _ p = pure p

-- | Generate code
lowerCopiesAndTraversals :: L2.Prog -> SyM L2.Prog
lowerCopiesAndTraversals p = pure p


-- Configuring and launching the compiler.
--------------------------------------------------------------------------------

-- | Overall configuration of the compiler, as determined by command
-- line arguments and possible environment variables.
data Config = Config
  { input     :: Input
  , mode      :: Mode -- ^ How to run, which backend.
  , benchInput :: Maybe FilePath -- ^ What packed, binary .gpkd file to use as input.
  , benchPrint :: Bool  -- ^ Should the benchamrked function have its output printed?
  , packed     :: Bool  -- ^ Use packed representation.
  , bumpAlloc  :: Bool  -- ^ Use bump-pointer allocation if using the non-packed backend.
  , verbosity  :: Int   -- ^ Debugging output, equivalent to DEBUG env var.
  , cc        :: String -- ^ C compiler to use
  , optc      :: String -- ^ Options to the C compiler
  , warnc     :: Bool
  , cfile     :: Maybe FilePath -- ^ Optional override to destination .c file.
  , exefile   :: Maybe FilePath -- ^ Optional override to destination binary file.
  , backend   :: Backend -- ^ Compilation backend used
  , stopAfter    :: String  -- ^ Stop the compilation pipeline after this pass
  }

-- | What input format to expect on disk.
data Input = Haskell
           | SExpr
           | Unspecified
  deriving (Show,Read,Eq,Ord,Enum,Bounded)

-- | How far to run the compiler/interpreter.
data Mode = ToParse  -- ^ Parse and then stop
          | ToC      -- ^ Compile to C
          | ToExe    -- ^ Compile to C then build a binary.
          | RunExe   -- ^ Compile to executable then run.
          | Interp2  -- ^ Interp late in the compiler pipeline.
          | Interp1  -- ^ Interp early.  Not implemented.

          | Bench Var -- ^ Benchmark a particular function applied to the packed data within an input file.

          | BenchInput FilePath -- ^ Hardcode the input file to the benchmark in the C code.
  deriving (Show,Read,Eq,Ord)

-- | Compilation backend used
data Backend = C | LLVM
  deriving (Show)

defaultConfig :: Config
defaultConfig =
  Config { input = Unspecified
         , mode  = ToExe
         , benchInput = Nothing
         , benchPrint = False
         , packed    = False
         , bumpAlloc = False
         , verbosity = 1
         , cc = "gcc"
         , optc = " -O3  "
         , warnc = False
         , cfile = Nothing
         , exefile = Nothing
         , backend = C
         , stopAfter = ""
         }

suppress_warnings :: String
suppress_warnings = " -Wno-incompatible-pointer-types -Wno-int-conversion "

configParser :: Parser Config
configParser = Config <$> inputParser
                      <*> modeParser
                      <*> ((Just <$> strOption (long "bench-input" <> metavar "FILE" <>
                                      help ("Hard-code the input file for --bench-fun, otherwise it"++
                                            " becomes a command-line argument of the resulting binary."++
                                            " Also we RUN the benchmark right away if this is provided.")))
                          <|> pure Nothing)
                      <*> (switch (long "bench-print" <>
                                  help "print the output of the benchmarked function, rather than #t"))
                      <*> (switch (short 'p' <> long "packed" <>
                                  help "enable packed tree representation in C backend")
                           <|> fmap not (switch (long "pointer" <>
                                         help "enable pointer-based trees in C backend (default)"))
                          )
                      <*> switch (long "bumpalloc" <>
                                  help "Use BUMPALLOC mode in generated C code.  Only affects --pointer")

                      <*> (option auto (short 'v' <> long "verbose" <>
                                       help "Set the debug output level, 1-5, mirrors DEBUG env var.")
                           <|> pure 1)
                      <*> ((strOption $ long "cc" <> help "set C compiler, default 'gcc'")
                            <|> pure (cc defaultConfig))
                      <*> ((strOption $ long "optc" <> help "set C compiler options, default '-std=gnu11 -O3'")
                           <|> pure (optc defaultConfig))
                      <*> switch (short 'w' <> long "warnc" <>
                                  help "Show warnings from C compiler, normally suppressed")
                      <*> ((fmap Just (strOption $ long "cfile" <> help "set the destination file for generated C code"))
                           <|> pure (cfile defaultConfig))
                      <*> ((fmap Just (strOption $ short 'o' <> long "exefile" <>
                                       help "set the destination file for the executable"))
                           <|> pure (exefile defaultConfig))
                      <*> backendParser
                      <*> ((strOption $ long "stop-after" <> help "Stop the compilation pipeline after this pass")
                            <|> pure (stopAfter defaultConfig))
 where
  inputParser :: Parser Input
                -- I'd like to display a separator and some more info.  How?
  inputParser = -- infoOption "foo" (help "bar") <*>
                flag' Haskell (long "hs")  <|>
                flag Unspecified SExpr (long "gib")

  modeParser = -- infoOption "foo" (help "bar") <*>
               flag' ToParse (long "parse" <> help "only parse, then print & stop") <|>
               flag' ToC     (long "toC" <> help "compile to a C file, named after the input") <|>
               flag' Interp1 (long "interp1" <> help "run through the interpreter early, right after parsing") <|>
               flag' Interp2 (short 'i' <> long "interp2" <>
                              help "run through the interpreter after cursor insertion") <|>
               flag' RunExe  (short 'r' <> long "run"     <> help "compile and then run executable") <|>
               (Bench <$> toVar <$> strOption (short 'b' <> long "bench-fun" <> metavar "FUN" <>
                                     help ("generate code to benchmark a 1-argument FUN against a input packed file."++
                                           "  If --bench-input is provided, then the benchmark is run as well.")))

  -- use C as the default backend
  backendParser :: Parser Backend
  backendParser = flag C LLVM (long "llvm" <> help "use the llvm backend for compilation")


-- | Parse configuration as well as file arguments.
configWithArgs :: Parser (Config,[FilePath])
configWithArgs = (,) <$> configParser
                     <*> some (argument str (metavar "FILES..."
                                             <> help "Files to compile."))

----------------------------------------

-- | Command line version of the compiler entrypoint.  Parses command
-- line arguments given as string inputs.  This also allows us to run
-- conveniently from within GHCI.  For example:
--
-- >  compileCmd $ words $ " -r -p -v5 examples/test11c_funrec.gib "
--
compileCmd :: [String] -> IO ()
compileCmd args = withArgs args $
    do (cfg,files) <- execParser opts
       case files of
         [f] -> compile cfg f
         _ -> do dbgPrintLn 1 $ "Compiling multiple files:  "++show files
                 mapM_ (compile cfg) files
  where
    opts = info (helper <*> configWithArgs)
      ( fullDesc
     <> progDesc "Compile FILES according to the below options."
     <> header "A compiler for a minature tree traversal language" )


sepline :: String
sepline = replicate 80 '='

lvl :: Int
lvl = 3

data CompileState =
     CompileState { cnt :: Int -- ^ Gensym counter
                  , result :: Maybe SI.Value -- ^ Result of evaluating output of prior pass, if available.
                  }

-- | Compiler entrypoint, given a full configuration and a list of
-- files to process, do the thing.
compile :: Config -> FilePath -> IO ()
compile config@Config{mode,input,verbosity,backend,cfile,packed} fp0 = do
  -- set the env var DEBUG, to verbosity, when > 1
  setDebugEnvVar verbosity

  -- parse the input file
  (parsed, fp) <- parseInput input fp0
  (l1, cnt0) <- parsed

  case mode of
    Interp1 -> runL1 l1
    ToParse -> dbgPrintLn 0 $ sdoc l1

    _ -> do
      dbgPrintLn lvl $ "Compiler pipeline starting, parsed program:\n"++sepline ++ "\n" ++ sdoc l1

      -- (Stage 1) Run the program through the interpreter
      initResult <- interpProg l1

      -- (Stage 2) C/LLVM codegen
      let outfile = getOutfile backend fp cfile

      -- run the initial program through the compiler pipeline
      stM <- return $ passes config l1
      inprog <- evalStateT stM (CompileState {cnt=cnt0, result=initResult})
      ------------------------------ TEMPORARY ------------------------------------
      -- UNDER_CONSTRUCTION
      hPutStrLn stderr "WARNING: UNDER_CONSTRUCTION.  Compiler mostly disabled atm."
      case inprog of
        L1 l1 -> runL1 l1
        L2 l2 -> runL2 l2
      ------------------------------ TEMPORARY ------------------------------------
        L3 l3 -> do
         if mode == Interp2
           then do
             l3res <- execProg l3
             mapM_ (\(IntVal v) -> liftIO $ print v) l3res
             exitSuccess
           else do error "UNDER_CONSTRUCTION"
{- -- UNDER_CONSTRUCTION
             str <- case backend of
               C    -> codegenProg packed l3
#ifdef LLVM_ENABLED
               LLVM -> LLVM.codegenProg packed l3
#endif
               LLVM -> error $ "Cannot execute through the LLVM backend. To build Gibbon with LLVM;"
                       ++ "stack build --flag gibbon:llvm_enabled"

             -- The C code is long, so put this at a higher verbosity level.
             dbgPrintLn minChatLvl $ "Final C codegen: " ++show (length str) ++" characters.\n" ++ sepline ++ "\n" ++ str

             clearFile outfile
             writeFile outfile str

             -- (Stage 3) Code written, now compile if warranted.
             when (mode == ToExe || mode == RunExe || isBench mode ) $ do
               compileAndRunExe config fp
-}

-- | The compiler's policy for running/printing L1 programs.
runL1 :: L1.Prog -> IO ()
runL1 l1 = do
    -- FIXME: no command line option atm.  Just env vars.
    runConf <- getRunConfig []
    dbgPrintLn 2 $ "Running the following through SourceInterp:\n "++sepline ++ "\n" ++ sdoc l1
    SI.execAndPrint runConf l1
    exitSuccess

-- | The compiler's policy for running/printing L2 programs.
runL2 :: L2.Prog -> IO ()
runL2 l2 = runL1 (L2.revertToL1 l2)

-- | Set the env var DEBUG, to verbosity, when > 1
-- TERRIBLE HACK!!
-- This verbosity value is global, "pure" and can be read anywhere
--
setDebugEnvVar :: Int -> IO ()
setDebugEnvVar verbosity =
  when (verbosity > 1) $ do
    setEnv "DEBUG" (show verbosity)
    l <- evaluate dbgLvl
    hPutStrLn stderr$ " ! We set DEBUG based on command-line verbose arg: "++show l


-- |
parseInput :: Input -> FilePath -> IO (IO (L1.Prog, Int), FilePath)
parseInput ip fp =
  case ip of
    Haskell -> return (HS.parseFile fp, fp)
    SExpr   -> return (SExp.parseFile fp, fp)
    Unspecified ->
      case takeExtension fp of
        ".hs" -> return (HS.parseFile fp, fp)
        ".sexp" -> return (SExp.parseFile fp, fp)
        ".rkt"  -> return (SExp.parseFile fp, fp)
        ".gib"  -> return (SExp.parseFile fp, fp)
        oth -> do
          -- A silly hack just out of sheer laziness vis-a-vis tab completion:
          let f1 = fp ++ ".gib"
              f2 = fp ++ "gib"
          f1' <- doesFileExist f1
          f2' <- doesFileExist f2
          if f1' && oth == ""
            then return (SExp.parseFile f1, f2)
            else if f2' && oth == "."
                   then return (SExp.parseFile f2, f2)
                   else error$ "compile: unrecognized file extension: "++
                        show oth++"  Please specify compile input format."


-- |
interpProg :: L1.Prog -> IO (Maybe SI.Value)
interpProg l1 =
  if dbgLvl >= interpDbgLevel
  then do
    -- FIXME: no command line option atm.  Just env vars.
    runConf <- getRunConfig []
    (val,_stdout) <- SI.interpProg runConf l1
    dbgPrintLn 2 $ " [eval] Init prog evaluated to: "++show val
    return $ Just val
  else
    return Nothing

-- | A compile job stopped somewhere in the middle.
data InProgress = L1 L1.Prog
                | L2 L2.Prog
                | L3 L3.Prog

-- |
passes :: Config -> L1.Prog -> StateT CompileState IO InProgress
passes config@Config{mode,packed} l1 = do
      l1 <- passE config "freshNames" freshNames l1
      -- If we are executing a benchmark, then we
      -- replace the main function with benchmark code:
      l1 <- pure $ case mode of
                     Bench fnname -> benchMainExp config l1 fnname
                     _ -> l1
      l1 <- passE  config "flatten"       flatten                                   l1
      return (L1 l1)
{- -- UNDER_CONSTRUCTION
      l1 <- passE  config "inlineTriv"    (return . inlineTriv)                     l1
      l1 <- pass True  config "addCopies"     addCopies                             l1
      l2 <- passE  config "inferEffects"  inferEffects                              l1
      l2 <- passE' config "typecheck"     (typecheckStrict (TCConfig False))        l2
      let mmainTyPre = fmap snd $ L2.mainExp l2
      l2  <-
          if packed
          then do
            ---------------- Stubs currently ------------------

            mt <- pass False config "findMissingTraversals" findMissingTraversals   l2
            l2 <- passE' config "addTraversals"             (addTraversals mt)      l2
            l2 <- passE' config "lowerCopiesAndTraversals" lowerCopiesAndTraversals l2

            ------------------- End Stubs ---------------------

            l2 <- pass True  config "routeEnds"     routeEnds                        l2
            -- l2  <- pass' mode  "typecheck"   (typecheckPermissive (TCConfig False)) l2
            l2 <- pass False config "flatten"       (flatten2 l1)                    l2
            l2 <- pass True  config "findWitnesses" findWitnesses                    l2

            -- QUESTION: Should programs typecheck and execute at this point?
            -- ANSWER: Not yet, PackedTy/CursorTy mismatches remain:
            -- l2 <- pass' mode "typecheck" (typecheckPermissive (TCConfig False)) l2

            l2 <- pass True config "inlinePacked"  inlinePacked                     l2

            -- l2  <- pass' mode "typecheck"   (typecheckPermissive (TCConfig False)) l2
            -- [2016.12.31] For now witness vars only work out after cursorDirect then findWitnesses:

            l2 <- passF config "cursorDirect"         cursorDirect                  l2

            -- This will issue some warnings, but is useful for debugging:
            -- l2  <- pass' mode  "typecheck" (typecheckPermissive (TCConfig True))   l2

            l2 <- pass False config "flatten"       (flatten2 l1)                   l2
            l2 <- pass True config "findWitnesses" findWitnesses                    l2
            l2 <- pass True config "shakeTree"   shakeTree                          l2

            -- After findwitnesses is when programs should once again typecheck:
            l2 <- passE' config "typecheck"  (typecheckStrict (TCConfig True))      l2
            l2 <- pass False config "flatten"    (flatten2 l1)                      l2
            l2 <- pass True config "inlineTriv"  (inline2 l1)                       l2
            l2 <- pass True config "shakeTree"   shakeTree                          l2
            l2 <- pass True config "hoistNewBuf" hoistNewBuf                        l2
            return l2
          else return l2

      l2  <- passE' config "typecheck" (typecheckStrict (TCConfig packed))          l2
      l2  <- pass True config "unariser" unariser                                   l2
      l2  <- passE' config "typecheck" (typecheckStrict (TCConfig packed))          l2
      l3  <- pass True config "lower"     (lower (packed,mmainTyPre))               l2
      return l3
-}

{-
-- | Repurposing L1 passes for L2:
--
flatten2 :: L1.Prog -> L2.Prog -> SyM L2.Prog
flatten2 l1 = L2.mapMExprs (flattenExp (L1.ddefs l1))

inline2 :: L1.Prog -> L2.Prog -> SyM L2.Prog
inline2 _ p = return (L2.mapExprs (\_ -> inlineTrivExp (L2.ddefs p)) p)
-}

-- | Replace the main function with benchmark code
--
benchMainExp :: Config -> L1.Prog -> Var -> L1.Prog
benchMainExp Config{benchInput,benchPrint} l1 fnname = do
  let tmp = "bnch"
      (arg@(L1.PackedTy tyc _),ret) = L1.getFunTy fnname l1
      -- At L1, we assume ReadPackedFile has a single return value:
      newExp = L1.LetE (toVar tmp, [],
                         arg,
                         L1.E1 $ L1.PrimAppE (L1.ReadPackedFile benchInput tyc arg) [])
                $
                L1.E1 $ L1.LetE (toVar "benchres", [],
                         ret,
                         L1.E1 $ L1.TimeIt (L1.E1 $ L1.AppE fnname [] (L1.E1 $ L1.VarE (toVar tmp))) ret True)
                $
                -- FIXME: should actually return the result,
                -- as soon as we are able to print it.
                (if benchPrint
                  then L1.E1 $ L1.VarE (toVar "benchres")
                  else L1.E1 $ L1.PrimAppE L1.MkTrue [])
  l1{ L1.mainExp = Just $ L1.E1 newExp }


type PassRunner a b = (Out b, NFData a, NFData b) =>
                      String -> (a -> SyM b) -> a -> StateT CompileState IO b


-- | Run a pass and return the result
--
pass :: Bool -> Config -> PassRunner a b
pass quiet Config{stopAfter} who fn x = do
  cs@CompileState{cnt} <- get
  if quiet
    then do
      _ <- lift $ evaluate $ force x
      lift$ dbgPrintLn lvl $ "\nPass output, " ++who++":\n"++sepline
    else
      lift$ dbgPrintLn lvl $ "Running pass: " ++who++":\n"++sepline
  let (y,cnt') = runSyM cnt (fn x)
  put cs{cnt=cnt'}
  _ <- lift $ evaluate $ force y
  if quiet
    then lift$ dbgPrintLn lvl $ sdoc y
     -- Still print if you crank it up.
    else lift$ dbgPrintLn 6 $ sdoc y
  when (stopAfter == who) $ do
    dbgTrace 0 ("Compilation stopped; --stop-after=" ++ who) (return ())
    liftIO exitSuccess
  return y


-- | Like pass, but also evaluates and checks the result.
--
passE :: Config -> Interp p2 => PassRunner p1 p2
passE config@Config{mode} = wrapInterp mode (pass True config)


-- | Version of 'passE' which does not print the output.
--
passE' :: Config -> Interp p2 => PassRunner p1 p2
passE' config@Config{mode} = wrapInterp mode (pass False config)


-- | An alternative version that allows FAILURE while running
-- the interpreter part.
-- FINISHME! For now not interpreting.
--
passF :: Config -> PassRunner p1 p2
passF config = pass True config


-- | Wrapper to enable running a pass AND interpreting the result.
--
wrapInterp :: (NFData p1, NFData p2, Interp p2, Out p2) =>
              Mode -> PassRunner p1 p2 -> String -> (p1 -> SyM p2) -> p1 ->
              StateT CompileState IO p2
wrapInterp mode pass who fn x =
  do CompileState{result} <- get
     p2 <- pass who fn x
     -- In benchmark mode we simply turn OFF the interpreter.
     -- This decision should be finer grained.
     when (dbgLvl >= interpDbgLevel && not (isBench mode)) $ lift $ do
       let Just res1 = result
       -- FIXME: no command line option atm.  Just env vars.
       runConf <- getRunConfig []
       let res2 = interpNoLogs runConf p2
       res2' <- catch (evaluate (force res2))
                (\exn -> error $ "Exception while running interpreter on pass result:\n"++sepline++"\n"
                         ++ show (exn::SomeException) ++ "\n"++sepline++"\nProgram was: "++abbrv 300 p2)
       unless (show res1 == res2') $
         error $ "After pass "++who++", evaluating the program yielded the wrong answer.\nReceived:  "
         ++show res2'++"\nExpected:  "++show res1
       dbgPrintLn 5 $ " [interp] answer was: "++sdoc res2'
     return p2


-- | Compile and run the generated code if appropriate
--
compileAndRunExe :: Config -> FilePath -> IO ()
compileAndRunExe cfg@Config{backend,benchInput,mode,cfile,exefile} fp = do
  clearFile exe

  -- (Stage 4) Codegen finished, generate a binary
  dbgPrintLn minChatLvl cmd
  cd <- system cmd
  case cd of
    ExitFailure n -> error$ (show backend) ++" compiler failed!  Code: "++show n
    ExitSuccess -> do
      -- (Stage 5) Binary compiled, run if appropriate
      let runExe extra = do
            exepath <- makeAbsolute exe
            c2 <- system (exepath++extra)
            case c2 of
              ExitSuccess -> return ()
              ExitFailure n -> error$ "Treelang program exited with error code "++ show n
      runConf <- getRunConfig [] -- FIXME: no command line option atm.  Just env vars.
      case benchInput of
        -- CONVENTION: In benchmark mode we expect the generated executable to take 2 extra params:
        Just _ | isBench mode   -> runExe $ " " ++show (rcSize runConf) ++ " " ++ show (rcIters runConf)
        _      | mode == RunExe -> runExe ""
        _                                -> return ()
  where outfile = getOutfile backend fp cfile
        exe = getExeFile backend fp exefile
        cmd = compilationCmd backend cfg ++ outfile ++ " -o " ++ exe


-- | Return the correct filename to store the generated code,
-- based on the backend used, and override options specified
--
getOutfile :: Backend -> FilePath -> Maybe FilePath -> FilePath
getOutfile _ _ (Just override) = override
getOutfile LLVM fp Nothing = replaceExtension fp ".ll"
getOutfile C fp Nothing  = replaceExtension fp ".c"


-- | Return the correct filename for the generated exe,
-- based on the backend used, and override options specified
--
getExeFile :: Backend -> FilePath -> Maybe FilePath -> FilePath
getExeFile _ _ (Just override) = override
getExeFile LLVM fp Nothing = replaceExtension (replaceFileName fp ((takeBaseName fp) ++ "_llvm")) ".exe"
getExeFile C fp Nothing = replaceExtension fp ".exe"


-- | Compilation command
--
compilationCmd :: Backend -> Config -> String
compilationCmd LLVM _   = "clang-3.9 lib.o "
compilationCmd C config = (cc config) ++" -std=gnu11 "
                          ++(if (bumpAlloc config) then "-DBUMPALLOC " else "")
                          ++(optc config)++"  "
                          ++(if (warnc config) then "" else suppress_warnings)

-- |
isBench :: Mode -> Bool
isBench (Bench _) = True
isBench _ = False

-- | The debug level at which we start to call the interpreter on the program during compilation.
interpDbgLevel :: Int
interpDbgLevel = 1

-- |
clearFile :: FilePath -> IO ()
clearFile fileName = removeFile fileName `catch` handleErr
  where
   handleErr e | isDoesNotExistError e = return ()
               | otherwise = throwIO e
