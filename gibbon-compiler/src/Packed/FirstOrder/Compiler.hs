{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE NamedFieldPuns        #-}
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
-- import qualified Packed.FirstOrder.L3_Target   as L3
import           Packed.FirstOrder.Passes.Codegen (codegenProg)
import           Packed.FirstOrder.Passes.Cursorize
import           Packed.FirstOrder.Passes.FindWitnesses (findWitnesses)
import           Packed.FirstOrder.Passes.Flatten
import           Packed.FirstOrder.Passes.Freshen
import           Packed.FirstOrder.Passes.HoistNewBuf
import           Packed.FirstOrder.Passes.InferEffects (inferEffects)
import           Packed.FirstOrder.Passes.InlinePacked
import           Packed.FirstOrder.Passes.InlineTriv
import           Packed.FirstOrder.Passes.Lower
import           Packed.FirstOrder.Passes.RouteEnds (routeEnds)
import           Packed.FirstOrder.Passes.ShakeTree 
import           Packed.FirstOrder.Passes.Typecheck
import           Packed.FirstOrder.Passes.Unariser
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

----------------------------------------
-- PASS STUBS
----------------------------------------
-- All of these need to be implemented, but are just the identity
-- function for now.  Move to Passes/*.hs when implemented.


-- | Find all local variables bound by case expressions which must be
-- traversed, but which are not by the current program.
findMissingTraversals :: L2.Prog -> SyM (Set Var)
findMissingTraversals _ = pure S.empty

-- | Add calls to an implicitly-defined, polymorphic "traverse"
-- function of type `p -> ()` for any packed type p.
addTraversals :: Set Var -> L2.Prog -> SyM L2.Prog
addTraversals _ p = pure p

-- | Add calls to an implicitly-defined, polymorphic "copy" function,
--   of type `p -> p` that works on all packed data `p`.  A copy is
--   added every time constraints conflict disallowing an argument of
--   a data constructor to be unified with the needed output location.
addCopies :: L2.Prog -> SyM L2.Prog
addCopies p = pure p

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
  , benchInput :: Maybe FilePath -- ^ What backed, binary file to use as input.
  , packed    :: Bool -- ^ Use packed representation.
  , verbosity :: Int  -- ^ Debugging output, equivalent to DEBUG env var.
  , cc        :: String -- ^ C compiler to use
  , optc      :: String -- ^ Options to the C compiler
  , warnc     :: Bool
  , cfile     :: Maybe FilePath -- ^ Optional override to destination .c file.
  , exefile   :: Maybe FilePath -- ^ Optional override to destination binary file.
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

defaultConfig :: Config
defaultConfig =
  Config { input = Unspecified
         , mode  = ToExe
         , benchInput = Nothing
         , packed = False
         , verbosity = 1
         , cc = "gcc"
         , optc = " -O3  "
         , warnc = False
         , cfile = Nothing
         , exefile = Nothing
         }

suppress_warnings :: String
suppress_warnings = " -Wno-incompatible-pointer-types -Wno-int-conversion "
  
configParser :: Parser Config
configParser = Config <$> inputParser
                      <*> modeParser
                      <*> ((Just <$> strOption (long "bench-input" <> metavar "FILE" <>
                                      help ("Hard-code the input file for --bench, otherwise it"++
                                            " becomes a command-line argument of the resulting binary.")))
                          <|> pure Nothing)
                      <*> (switch (short 'p' <> long "packed" <>
                                  help "enable packed tree representation in C backend")
                           <|> fmap not (switch (long "pointer" <>
                                         help "enable pointer-based trees in C backend (default)"))
                          )
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
 where
  inputParser :: Parser Input
                -- I'd like to display a separator and some more info.  How?
  inputParser = -- infoOption "foo" (help "bar") <*>
                flag' Haskell (long "hs")  <|>
                flag Unspecified SExpr (long "sexp")

  modeParser = -- infoOption "foo" (help "bar") <*>
               flag' ToParse (long "parse" <> help "only parse, then print & stop") <|>
               flag' ToC     (long "toC" <> help "compile to a C file, named after the input") <|>
               flag' Interp1 (long "interp1" <> help "run through the interpreter early, right after parsing") <|>
               flag' Interp2 (short 'i' <> long "interp2" <>
                              help "run through the interpreter after cursor insertion") <|>
               flag' RunExe  (short 'r' <> long "run"     <> help "compile and then run executable") <|>
               flag ToExe ToExe (long "exe"  <> help "compile through C to executable (default)") <|>
               (Bench <$> strOption (short 'b' <> long "bench" <> metavar "FUN" <>
                                     help ("generate code to benchmark a 1-argument FUN against a input packed file."++
                                           "  If --bench-input is provided, then the benchmark is run as well.")))
               

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
-- >  compileCmd $ words $ " -r -p -v5 examples/test11c_funrec.sexp "
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


-- type PassRunner = forall a b . (Out b, NFData a, NFData b) => String -> (a -> SyM b) -> a -> StateT CompileState IO b
type PassRunner a b = (Out b, NFData a, NFData b) => String -> (a -> SyM b) -> a -> StateT CompileState IO b
     
-- | Compiler entrypoint, given a full configuration and a list of
-- files to process.
compile :: Config -> FilePath -> IO ()
-- compileFile :: (FilePath -> IO (L1.Prog,Int)) -> FilePath -> IO ()
compile Config{input,mode,benchInput,packed,verbosity,cc,optc,warnc,cfile,exefile} fp0 = do
  -- TERRIBLE HACK!!  This value is global, "pure" and can be read anywhere
  when (verbosity > 1) $ do
    setEnv "DEBUG" (show verbosity)
    l <- evaluate dbgLvl
    hPutStrLn stderr$ " ! We set DEBUG based on command-line verbose arg: "++show l

  (parser,fp) <- case input of
                  Haskell -> return (HS.parseFile, fp0)
                  SExpr   -> return (SExp.parseFile, fp0)
                  Unspecified ->
                   case takeExtension fp0 of
                     ".hs"   -> return (HS.parseFile, fp0)
                     ".sexp" -> return (SExp.parseFile, fp0)
                     ".rkt"  -> return (SExp.parseFile, fp0)
                     oth -> do
                       -- Here's a silly hack just out of sheer laziness vis-a-vis tab completion:
                         f1 <- doesFileExist $ fp0++".sexp"
                         f2 <- doesFileExist $ fp0++"sexp"
                         if f1 && oth == ""
                          then return (SExp.parseFile, fp0++".sexp")
                          else if f2 && oth == "."
                          then return (SExp.parseFile, fp0++"sexp")
                          else error$ "compile: unrecognized file extension: "++
                                  show oth++"  Please specify compile input format."
  (l1,cnt0) <- parser fp

  let printParse l = dbgPrintLn l $ sdoc l1    
  when (mode == Interp1) $ do
      runConf <- getRunConfig [] -- FIXME: no command line option atm.  Just env vars.
      dbgPrintLn 2 $ "Running the following through SourceInterp:\n "++sepline
      printParse 2
                 
      SI.execAndPrint runConf l1
      exitSuccess

  if mode == ToParse
   then do -- dbgPrintLn lvl "Parsed program:"
           -- dbgPrintLn l sepline
           printParse 0
   else do
    dbgPrintLn lvl $ "Compiler pipeline starting, parsed program:\n"++sepline
    printParse lvl
    let pass :: PassRunner a b
        pass who fn x = do
          cs@CompileState{cnt} <- get
          _ <- lift $ evaluate $ force x
          lift$ dbgPrintLn lvl $ "\nPass output, " ++who++":\n"++sepline
          let (y,cnt') = runSyM cnt (fn x)
          put cs{cnt=cnt'}
          _ <- lift $ evaluate $ force y
          lift$ dbgPrintLn lvl $ sdoc y
          return y

        -- No reason to chatter from passes that are stubbed out anyway:
        pass' :: PassRunner a b
        pass' who fn x = do
          cs@CompileState{cnt} <- get
          lift$ dbgPrintLn lvl $ "Running pass: " ++who++":\n"++sepline
          let (y,cnt') = runSyM cnt (fn x)          
          put cs{cnt=cnt'}
          _ <- lift $ evaluate $ force y
          lift$ dbgPrintLn 6 $ sdoc y -- Still print if you crank it up.
          return y

        -- | Like pass, but also evaluates and checks the result.
        -- passE :: String -> (L1.Prog -> SyM L1.Prog) -> L1.Prog -> StateT CompileState IO L1.Prog

        wrapInterp :: (NFData p1, NFData p2, Interp p2, Out p2) =>
                      PassRunner p1 p2 -> String -> (p1 -> SyM p2) -> p1 -> StateT CompileState IO p2
        wrapInterp pass who fn x =
          do CompileState{result} <- get
             p2 <- pass who fn x
             -- In benchmark mode we simply turn OFF the interpreter.  This decision should be finer grained.
             when (dbgLvl >= interpDbgLevel && not (isBench mode)) $ lift $ do
               let Just res1 = result 
               runConf <- getRunConfig [] -- FIXME: no command line option atm.  Just env vars.
               let res2 = interpNoLogs runConf p2
               res2' <- catch (evaluate (force res2))                        
                         (\exn -> error $ "Exception while running interpreter on pass result:\n"++sepline++"\n"
                                  ++ show (exn::SomeException) ++ "\n"++sepline++"\nProgram was: "++abbrv 300 p2)
               unless (show res1 == res2') $ 
                 error $ "After pass "++who++", evaluating the program yielded the wrong answer.\nReceived:  "
                         ++show res2'++"\nExpected:  "++show res1
               dbgPrintLn 5 $ " [interp] answer was: "++sdoc res2'
             return p2

        -- | Wrapper to enable running a pass AND interpreting the result.
        passE :: Interp p2 => PassRunner p1 p2
        passE = wrapInterp pass

        -- | Version of 'passE' which does not print the output.
        passE' :: Interp p2 => PassRunner p1 p2
        passE' = wrapInterp pass'

        -- | An alternative version that allows FAILURE while running
        -- the interpreter part.
        passF = pass -- FINISHME! For now not interpreting.
                    
    let outfile = case cfile of
                    Nothing -> (replaceExtension fp ".c")
                    Just f -> f
        exe     = case exefile of
                    Nothing -> replaceExtension fp ".exe"
                    Just f -> f

    clearFile outfile
    clearFile exe

    -- Repurposing L1 passes for L2:
    let flatten2 :: L2.Prog -> SyM L2.Prog
        flatten2 = L2.mapMExprs (flattenExp (L1.ddefs l1))
        inline2 :: L2.Prog -> SyM L2.Prog
        inline2 p = return (L2.mapExprs (\_ -> inlineTrivExp (L2.ddefs p)) p)
    initResult <- if dbgLvl >= interpDbgLevel
                  then do runConf <- getRunConfig [] -- FIXME: no command line option atm.  Just env vars.
                          (val,_stdout) <- SI.interpProg runConf l1
                          dbgPrintLn 2 $ " [eval] Init prog evaluated to: "++show val
                          return $ Just val
                  else return Nothing
    str <- evalStateT
             (do l1 <-       passE "freshNames"               freshNames               l1

                 -- -- If we are executing a benchmark, then we
                 -- -- replace the main function with benchmark code:
                 l1 <- pure$ case mode of
                             Bench fnname ->
                                 let tmp = "bnch"
                                     (arg,ret) = L1.getFunTy fnname l1
                                 in
                                 l1{ L1.mainExp = Just $
                                      -- At L1, we assume ReadPackedFile has a single return value:
                                      L1.LetE (tmp, arg, L1.PrimAppE (L1.ReadPackedFile benchInput arg) []) $ 
                                        L1.LetE ("ignored", ret, L1.TimeIt (L1.AppE fnname (L1.VarE tmp)) ret True) $
                                          -- FIXME: should actually return the result, as soon as we are able to print it.
                                          (L1.LitE 0)
                                    }
                             _ -> l1

                 l1  <-       passE "flatten"                  flatten                  l1
                 l1  <-       passE "inlineTriv"               (return . inlineTriv)    l1
                 l2  <-       passE "inferEffects"             inferEffects             l1
                 l2  <-       passE' "typecheck"     (typecheckStrict (TCConfig False)) l2
                 l2  <-
                     if packed
                     then do
                       ---------------- Stubs currently ------------------
                       mt  <- pass'  "findMissingTraversals"    findMissingTraversals     l2
                       l2  <- passE' "addTraversals"            (addTraversals mt)        l2
                       l2  <- passE' "addCopies"                addCopies                 l2
                       l2  <- passE' "lowerCopiesAndTraversals" lowerCopiesAndTraversals  l2
                       ------------------- End Stubs ---------------------
                       l2  <- pass   "routeEnds"                routeEnds                l2
                       l2  <- pass'  "flatten"                  flatten2                 l2
                       l2  <- pass   "findWitnesses"            findWitnesses            l2
                       -- QUESTION: Should programs typecheck and execute at this point?
                       -- l2  <- passE' "typecheck"     (typecheckStrict (TCConfig True))   l2
                       l2  <- pass   "inlinePacked"             inlinePacked             l2
                       -- [2016.12.31] For now witness vars only work out after cursorDirect then findWitnesses:
                       l2  <- passF  "cursorDirect"             cursorDirect             l2
                       l2  <- pass'  "flatten"                  flatten2                 l2
                       l2  <- pass   "findWitnesses"            findWitnesses            l2
                       -- After findwitnesses is when programs should once again typecheck:
                       l2  <- passE' "typecheck"     (typecheckStrict (TCConfig True))   l2
                              
                       l2  <- pass' "flatten"                  flatten2                  l2
                       l2  <- pass  "inlineTriv"               inline2                   l2
                       l2  <- pass  "shakeTree"                shakeTree                 l2
                       l2  <- pass  "hoistNewBuf"              hoistNewBuf               l2
                       return l2
                     else return l2
                 l2  <-       passE' "typecheck"     (typecheckStrict (TCConfig packed)) l2
                 l2  <-       pass   "unariser"                 unariser                 l2
                 l2  <-       passE' "typecheck"     (typecheckStrict (TCConfig packed)) l2
                 l3  <-       pass   "lower"                    (lower packed)           l2

                 if mode == Interp2
                  then do l3res <- lift $ execProg l3
                          mapM_ (\(IntVal v) -> liftIO $ print v) l3res
                          liftIO $ exitSuccess
                  else do                   
                   str <- lift (codegenProg packed l3)

                   -- The C code is long, so put this at a higher verbosity level.
                   lift$ dbgPrintLn minChatLvl $ "Final C codegen: "++show (length str)++" characters."
                   lift$ dbgPrintLn 5 sepline
                   lift$ dbgPrintLn 5 str
                   return str)
              (CompileState { cnt=cnt0
                            , result= initResult })

    writeFile outfile str
    -- (Stage 2) Code written, now compile if warranted.
    when (mode == ToExe || mode == RunExe || isBench mode ) $ do
      let cmd = cc ++" -std=gnu11 "++optc++" "++" "
                   ++(if warnc then "" else suppress_warnings)
                   ++" "++outfile++" -o "++ exe
      dbgPrintLn minChatLvl cmd
      cd <- system cmd
      case cd of
       ExitFailure n -> error$ "C compiler failed!  Code: "++show n
       ExitSuccess -> do
         -- (Stage 3) Binary compiled, run if appropriate
         let runExe = do exepath <- makeAbsolute exe
                         c2 <- system exepath
                         case c2 of
                           ExitSuccess -> return ()
                           ExitFailure n -> error$ "Treelang program exited with error code "++ show n
         case benchInput of
           Just _ | isBench mode -> runExe
           _ | mode == RunExe    -> runExe
           _                     -> return ()


isBench :: Mode -> Bool
isBench (Bench _) = True
isBench _ = False
                             
-- | The debug level at which we start to call the interpreter on the program during compilation.
interpDbgLevel :: Int
interpDbgLevel = 1
                             
clearFile :: FilePath -> IO ()
clearFile fileName = removeFile fileName `catch` handleErr
  where
   handleErr e | isDoesNotExistError e = return ()
               | otherwise = throwIO e


