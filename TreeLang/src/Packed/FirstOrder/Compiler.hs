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

import Control.DeepSeq
import Control.Exception
import Control.Monad.State.Strict
import Options.Applicative
import Packed.FirstOrder.Common
import qualified Packed.FirstOrder.HaskellFrontend as HS
import Packed.FirstOrder.Interpreter (Val (..), execProg)
import qualified Packed.FirstOrder.L1_Source as L1
import qualified Packed.FirstOrder.LTraverse as L2

import Packed.FirstOrder.Passes.InferEffects (inferEffects)
import Packed.FirstOrder.Passes.FindWitnesses (findWitnesses)
import Packed.FirstOrder.Passes.RouteEnds (routeEnds)
import Packed.FirstOrder.Passes.Freshen
import Packed.FirstOrder.Passes.Cursorize
import Packed.FirstOrder.Passes.Flatten
import Packed.FirstOrder.Passes.InlineTriv
import Packed.FirstOrder.Passes.ShakeTree 
import Packed.FirstOrder.Passes.Lower
import Packed.FirstOrder.Passes.InlinePacked
import Packed.FirstOrder.Passes.Unariser
import Packed.FirstOrder.Passes.HoistNewBuf
import Packed.FirstOrder.Passes.Typecheck

import qualified Packed.FirstOrder.SExpFrontend as SExp
import Packed.FirstOrder.Target (codegenProg)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.Process
import Text.PrettyPrint.GenericPretty

------------------------------------------------------------

import Data.Set as S hiding (map)

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

data Config = Config
  { input     :: Input
  , mode      :: Mode -- ^ How to run, which backend.
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
  deriving (Show,Read,Eq,Ord,Enum,Bounded)

defaultConfig :: Config
defaultConfig =
  Config { input = Unspecified
         , mode  = ToExe
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
configParser = Config <$> inputParser <*> modeParser
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
  -- Most direct way, but I don't like it:
  _inputParser :: Parser Input
  _inputParser = option auto
   ( long "input"
        <> metavar "I"
        <> help ("Input file format, if unspecified based on file extension. " ++
               "Options are: "++show [minBound .. maxBound::Input]))
  _modeParser :: Parser Mode
  _modeParser = option auto
   ( long "mode"
        <> metavar "I"
        <> help ("Compilation mode. " ++
                "Options are: "++show [minBound .. maxBound::Mode]))

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
               flag ToExe ToExe (long "exe"  <> help "compile through C to executable (default)")

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

-- | Compiler entrypoint, given a full configuration and a list of
-- files to process.
compile :: Config -> FilePath -> IO ()
-- compileFile :: (FilePath -> IO (L1.Prog,Int)) -> FilePath -> IO ()
compile Config{input,mode,packed,verbosity,cc,optc,warnc,cfile,exefile} fp0 = do
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
  if mode == ToParse
   then do -- dbgPrintLn lvl "Parsed program:"
           -- dbgPrintLn l sepline
           printParse 0
   else do
    dbgPrintLn lvl $ "Compiler pipeline starting, parsed program:\n"++sepline
    printParse lvl
    let pass :: (Out b, NFData a, NFData b) => String -> (a -> SyM b) -> a -> StateT Int IO b
        pass who fn x = do
          cnt <- get
          _ <- lift $ evaluate $ force x
          let (y,cnt') = runSyM cnt (fn x)
          put cnt'
          lift$ dbgPrintLn lvl $ "\nPass output, " ++who++":\n"++sepline
          _ <- lift $ evaluate $ force y
          lift$ dbgPrintLn lvl $ sdoc y
          return y

        -- No reason to chatter from passes that are stubbed out anyway:
        pass' :: (Out b, NFData b) => String -> (a -> SyM b) -> a -> StateT Int IO b
        pass' who fn x = do
          cnt <- get
          let (y,cnt') = runSyM cnt (fn x)
          lift$ dbgPrintLn lvl $ "Running pass: " ++who++":\n"++sepline
          put cnt'
          _ <- lift $ evaluate $ force y
          lift$ dbgPrintLn 6 $ sdoc y -- Still print if you crank it up.
          return y

    when (mode == Interp1) $
      error "Early-phase interpreter not implemented yet!"

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
    str <- evalStateT
             (do l1b <-       pass "freshNames"               freshNames               l1
                 l1c <-       pass "flatten"                  flatten                  l1b
                 l1d <-       pass "inlineTriv"               (return . inlineTriv)    l1c
                 l2  <-       pass  "inferEffects"            inferEffects             l1d
                 l2' <-
                     if packed
                     then do
                       ---------------- Stubs currently ------------------
                       mt  <- pass' "findMissingTraversals"    findMissingTraversals    l2
                       l2b <- pass' "addTraversals"            (addTraversals mt)       l2
                       l2c <- pass' "addCopies"                addCopies                l2b
                       l2d <- pass' "lowerCopiesAndTraversals" lowerCopiesAndTraversals l2c
                       ------------------- End Stubs ---------------------
                       l2d' <- pass' "typecheck"                typecheck                 l2d
                       l2e <- pass  "routeEnds"                routeEnds                 l2d'
                       l2f <- pass' "flatten"                  flatten2                  l2e
                       l2g <- pass  "findWitnesses"            findWitnesses             l2f
                       l2h <- pass  "inlinePacked"             inlinePacked              l2g
                       l2i <- pass  "cursorDirect"             cursorDirect              l2h
                       l2i' <- pass' "typecheck"                typecheck                l2i
                       l2j <- pass' "flatten"                  flatten2                  l2i'
                       l2k <- pass  "findWitnesses"            findWitnesses             l2j
                              
                       l2l <- pass' "flatten"                  flatten2                  l2k
                       l2m <- pass  "inlineTriv"               inline2                   l2l
                       l2n <- pass  "shakeTree"                shakeTree                 l2m
                       l2o <- pass  "hoistNewBuf"              hoistNewBuf               l2n
                       return l2o
                     else return l2
                 l2'' <-       pass  "unariser"                 unariser                 l2'
                 l3   <-       pass  "lower"                    (lower packed)           l2''

                 if mode == Interp2
                  then do mapM_ (\(IntVal v) -> liftIO $ print v) (execProg l3)
                          liftIO $ exitSuccess
                  else do
                   str <- lift (codegenProg l3)
                   -- The C code is long, so put this at a higher level.
                   lift$ dbgPrintLn 1 $ "Final C codegen: "++show (length str)++" characters."
                   lift$ dbgPrintLn 5 sepline
                   lift$ dbgPrintLn 5 str
                   return str)
              cnt0

    writeFile outfile str
    when (mode == ToExe || mode == RunExe) $ do
      let cmd = cc ++" -std=gnu11 "++optc++" "++" "
                   ++(if warnc then "" else suppress_warnings)
                   ++" "++outfile++" -o "++ exe
      dbgPrintLn 1 cmd
      cd <- system cmd
      case cd of
       ExitFailure n -> error$ "C compiler failed!  Code: "++show n
       ExitSuccess -> do
         when (mode == RunExe)$ do
          exepath <- makeAbsolute exe
          c2 <- system exepath
          case c2 of
            ExitSuccess -> return ()
            ExitFailure n -> error$ "Treelang program exited with error code "++ show n


clearFile :: FilePath -> IO ()
clearFile fileName = removeFile fileName `catch` handleErr
  where
   handleErr e | isDoesNotExistError e = return ()
               | otherwise = throwIO e
