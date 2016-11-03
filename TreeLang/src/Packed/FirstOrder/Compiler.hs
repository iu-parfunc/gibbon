{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import Control.Monad.State
import Options.Applicative
import Packed.FirstOrder.Common
import qualified Packed.FirstOrder.HaskellFrontend as HS
import qualified Packed.FirstOrder.L1_Source as L1
import Packed.FirstOrder.LTraverse (inferEffects, Prog(..))
import qualified Packed.FirstOrder.LTraverse as L2
import Packed.FirstOrder.Passes.Cursorize (cursorize, lower)
import qualified Packed.FirstOrder.SExpFrontend as SExp
import Packed.FirstOrder.Target (codegenProg)
import System.FilePath
import System.Environment
import System.Process
import System.Directory
import System.Exit
import System.IO.Error (isDoesNotExistError)
import Text.PrettyPrint.GenericPretty
------------------------------------------------------------

import Data.Set as S hiding (map)
import qualified Data.Map as M
import Data.Maybe (fromJust)

----------------------------------------
-- PASS STUBS
----------------------------------------
-- All of these need to be implemented, but are just the identity
-- function for now.

-- | Rename all local variables
freshNames :: L1.Prog -> SyM L1.Prog
freshNames (L1.Prog defs funs main) =
    do main' <- case main of
                  Nothing -> return Nothing
                  Just m -> do m' <- freshExp [] m
                               return $ Just m'
       funs' <- freshFuns funs
       return $ L1.Prog defs funs' main'
    where freshFuns = mapM freshFun
          freshFun (FunDef nam (narg,targ) ty bod) =
              do narg' <- gensym narg
                 bod' <- freshExp [(narg,narg')] bod
                 return $ FunDef nam (narg',targ) ty bod'

          freshExp :: [(Var,Var)] -> L1.Exp -> SyM L1.Exp
          freshExp vs (L1.VarE v) =
              case lookup v vs of
                Nothing -> return $ L1.VarE v
                Just v' -> return $ L1.VarE v'
          freshExp _ (L1.LitE i) =
              return $ L1.LitE i
          freshExp vs (L1.AppE v e) =
              do e' <- freshExp vs e
                 return $ L1.AppE v e'
          freshExp vs (L1.PrimAppE p es) =
              do es' <- mapM (freshExp vs) es
                 return $ L1.PrimAppE p es'
          freshExp vs (L1.LetE (v,t,e1) e2) =
              do e1' <- freshExp vs e1
                 v' <- gensym v
                 e2' <- freshExp ((v,v'):vs) e2
                 return $ L1.LetE (v',t,e1') e2'
          freshExp vs (L1.IfE e1 e2 e3) =
              do e1' <- freshExp vs e1
                 e2' <- freshExp vs e2
                 e3' <- freshExp vs e3
                 return $ L1.IfE e1' e2' e3'
          freshExp vs (L1.ProjE i e) =
              do e' <- freshExp vs e
                 return $ L1.ProjE i e'
          freshExp vs (L1.MkProdE es) =
              do es' <- mapM (freshExp vs) es
                 return $ L1.MkProdE es'
          freshExp vs (L1.CaseE e mp) =
              do e' <- freshExp vs e
                 mp' <- mapM (\(args,ae) -> do
                                args' <- mapM gensym args
                                let vs' = (zip args args') ++ vs
                                ae' <- freshExp vs' ae
                                return (args',ae')) mp
                 return $ L1.CaseE e' mp'
          freshExp vs (L1.MkPackedE c es) =
              do es' <- mapM (freshExp vs) es
                 return $ L1.MkPackedE c es'
          freshExp vs (L1.TimeIt e) =
              do e' <- freshExp vs e
                 return $ L1.TimeIt e'
          freshExp vs (L1.MapE (v,t,b) e) =
              do b' <- freshExp vs b
                 e' <- freshExp vs e
                 return $ L1.MapE (v,t,b') e'
          freshExp vs (L1.FoldE (v1,t1,e1) (v2,t2,e2) e3) =
              do e1' <- freshExp vs e1
                 e2' <- freshExp vs e2
                 e3' <- freshExp vs e3
                 return $ L1.FoldE (v1,t1,e1') (v2,t2,e2') e3'

-- | Put the program in A-normal form where only varrefs and literals
-- are allowed in operand position.
flatten :: L1.Prog -> SyM L1.Prog
flatten (L1.Prog defs funs main) =
    do main' <- case main of
                  Nothing -> return Nothing
                  Just m -> do m' <- flattenExp [] m
                               return $ Just (inlineTrivExp [] m')
       funs' <- flattenFuns funs
       return $ L1.Prog defs funs' main'
    where flattenFuns = mapM flattenFun
          flattenFun (FunDef nam (narg,targ) ty bod) =
              do bod' <- flattenExp [(narg,targ)] bod
                 return $ FunDef nam (narg,targ) ty (inlineTrivExp [] bod')

          flattenExp :: [(Var,L1.Ty)] -> L1.Exp -> SyM L1.Exp
          flattenExp _env (L1.VarE v) = return $ L1.VarE v
          flattenExp _env (L1.LitE i) = return $ L1.LitE i
          flattenExp _env (L1.AppE v (L1.VarE v')) = return $ L1.AppE v (L1.VarE v')
          flattenExp env (L1.AppE v e) =
              do e' <- flattenExp env e
                 v' <- gensym "tmp_flat"
                 let ty = typeExp env e
                 return $ mkLetE (v',ty,e') (L1.AppE v (L1.VarE v'))
          flattenExp env (L1.PrimAppE p es) =
              do es' <- mapM (flattenExp env) es
                 nams <- mapM gensym $ replicate (length es) "tmp_flat"
                 let bind [] _t e = e
                     bind ((v,e'):xs) t e = mkLetE (v,t,e') $ bind xs t e                 
                     doprim ty = return $ bind (zip nams es') ty $
                                  L1.PrimAppE p $ map L1.VarE nams
                 case p of
                   L1.AddP -> doprim L1.IntTy
                   L1.SubP -> doprim L1.IntTy
                   L1.MulP -> doprim L1.IntTy
                   L1.EqSymP -> doprim L1.SymTy
                   L1.EqIntP -> doprim L1.IntTy
                   L1.MkTrue  -> return (L1.PrimAppE L1.MkTrue [])
                   L1.MkFalse -> return (L1.PrimAppE L1.MkFalse [])
                   L1.DictInsertP -> error "DictInsertP not handled in flatten yet"
                   L1.DictLookupP ->
                       do let dictty = typeExp env $ es !! 1
                          return $
                                 mkLetE (nams !! 0, L1.SymTy, es !! 0) $ -- NOTE: expected to be symbol!
                                 mkLetE (nams !! 1, dictty, es !! 1) $
                                 L1.PrimAppE L1.DictLookupP $ map L1.VarE nams
                   L1.ErrorP s t -> return $ L1.PrimAppE (L1.ErrorP s t) []
                   

          flattenExp env (L1.LetE (v,t,e') e) =
              do fe' <- flattenExp env e'
                 fe  <- flattenExp env e
                 return $ mkLetE (v,t,fe') fe
          flattenExp env (L1.IfE e1 e2 e3) =
              do fe1 <- flattenExp env e1
                 fe2 <- flattenExp env e2
                 fe3 <- flattenExp env e3
                 v1 <- gensym "tmp_flat"
                 return $ mkLetE (v1,L1.BoolTy,fe1) $ L1.IfE (L1.VarE v1) fe2 fe3
          flattenExp env (L1.ProjE i e) =
              do fe <- flattenExp env e
                 return $ L1.ProjE i fe
          flattenExp env (L1.MkProdE es) =
              do fes <- mapM (flattenExp env) es
                 nams <- mapM gensym $ replicate (length fes) "tmp_flat"
                 let tys = map (typeExp env) fes
                     bind [] e = e
                     bind ((v,t,e'):xs) e = mkLetE (v,t,e') $ bind xs e
                 return $ bind (zip3 nams tys fes) $ L1.MkProdE $ map L1.VarE nams
          flattenExp env (L1.CaseE e mp) =
              do fe <- flattenExp env e
                 v <- gensym "tmp_flat"
                 let als = M.assocs mp
                     ty  = typeExp env fe
                 fals <- forM als $ \(c,(args,ae)) -> do
                           let tys = lookupDataCon defs c
                           fae <- flattenExp ((zip args tys) ++ env) ae
                           return (c,(args,fae))
                 let fmp = M.fromList fals
                 return $ mkLetE (v,ty,fe) $ L1.CaseE (L1.VarE v) fmp
          flattenExp env (L1.MkPackedE c es) =
              do fes <- mapM (flattenExp env) es
                 nams <- mapM gensym $ replicate (length fes) "tmp_flat"
                 let tys = map (typeExp env) fes
                     bind [] e = e
                     bind ((v,t,e'):xs) e = mkLetE (v,t,e') $ bind xs e
                 return $ bind (zip3 nams tys fes) $ L1.MkPackedE c $ map L1.VarE nams
          flattenExp env (L1.TimeIt e) = (flattenExp env e) >>= (\fe -> return $ L1.TimeIt fe)
          flattenExp env (L1.MapE (v,t,e') e) =
              do fe' <- flattenExp env e'
                 fe <- flattenExp env e
                 return $ L1.MapE (v,t,fe') fe
          flattenExp env (L1.FoldE (v1,t1,e1) (v2,t2,e2) e3) =
              do fe1 <- flattenExp env e1
                 fe2 <- flattenExp env e2
                 fe3 <- flattenExp env e3
                 return $ L1.FoldE (v1,t1,fe1) (v2,t2,fe2) fe3

          -- | Helper function that lifts out Lets on the RHS of other Lets.
          --   Absolutely requires unique names.
          mkLetE (vr,ty, L1.LetE bnd e) bod = mkLetE bnd $ mkLetE (vr,ty,e) bod
          mkLetE bnd bod = L1.LetE bnd bod
                                              
          typeExp :: [(Var,L1.Ty)] -> L1.Exp -> L1.Ty
          typeExp env (L1.VarE v) = fromJust $ lookup v env
          typeExp _env (L1.LitE _i) = L1.IntTy
          typeExp _env (L1.AppE v _e) =
              case M.lookup v funs of
                Nothing -> error $ "Could not look up type of function: " ++ v ++ " (typeExp)"
                Just x -> funRetTy x
          typeExp _env (L1.PrimAppE p _es) =
              case p of
                L1.AddP -> L1.IntTy
                L1.SubP -> L1.IntTy
                L1.MulP -> L1.IntTy
                L1.EqSymP -> L1.BoolTy
                _ -> error $ "case " ++ (show p) ++ " not handled in typeExp yet"
          typeExp env (L1.LetE (v,t,_) e) = typeExp ((v,t):env) e
          typeExp env (L1.IfE _ e _) = typeExp env e
          typeExp env (L1.ProjE i e) =
              let (L1.ProdTy tys) = typeExp env e
              in tys !! i
          typeExp env (L1.MkProdE es) =
              L1.ProdTy $ map (typeExp env) es
          typeExp env (L1.CaseE _e mp) =
              let (c,(args,e)) = (M.assocs mp) !! 0
              in typeExp ((zip args (lookupDataCon defs c)) ++ env) e
          typeExp _env (L1.MkPackedE c _es) = L1.Packed c
          typeExp env (L1.TimeIt e) = typeExp env e
          typeExp env (L1.MapE _ e) = typeExp env e
          typeExp env (L1.FoldE _ _ e) = typeExp env e


                                         
-- | Inline trivial let bindings (binding a var to a var or int), mainly to clean up
--   the output of `flatten`.
inlineTrivExp :: [(Var,L1.Exp)] -> L1.Exp -> L1.Exp
inlineTrivExp env (L1.VarE v) =
    case lookup v env of
      Nothing -> L1.VarE v
      Just e -> e
inlineTrivExp _env (L1.LitE i) = L1.LitE i
inlineTrivExp env (L1.AppE v e) = L1.AppE v $ inlineTrivExp env e
inlineTrivExp env (L1.PrimAppE p es) = L1.PrimAppE p $ map (inlineTrivExp env) es
inlineTrivExp env (L1.LetE (v,t,e') e) =
    case e' of
      L1.VarE _v -> inlineTrivExp ((v,e'):env) e
      L1.LitE _i -> inlineTrivExp ((v,e'):env) e
      _ -> L1.LetE (v,t,e') e
inlineTrivExp env (L1.IfE e1 e2 e3) =
    L1.IfE (inlineTrivExp env e1) (inlineTrivExp env e2) (inlineTrivExp env e3)
inlineTrivExp env (L1.ProjE i e) = L1.ProjE i $ inlineTrivExp env e
inlineTrivExp env (L1.MkProdE es) = L1.MkProdE $ map (inlineTrivExp env) es
inlineTrivExp env (L1.CaseE e mp) =
    let e' = inlineTrivExp env e
        mp' = M.fromList $ map (\(c,(args,ae)) -> (c,(args,inlineTrivExp env ae))) $ M.assocs mp
    in L1.CaseE e' mp'
inlineTrivExp env (L1.MkPackedE c es) = L1.MkPackedE c $ map (inlineTrivExp env) es
inlineTrivExp env (L1.TimeIt e) = L1.TimeIt $ inlineTrivExp env e
inlineTrivExp env (L1.MapE (v,t,e') e) = L1.MapE (v,t,inlineTrivExp env e') (inlineTrivExp env e)
inlineTrivExp env (L1.FoldE (v1,t1,e1) (v2,t2,e2) e3) =
    L1.FoldE (v1,t1,inlineTrivExp env e1) (v2,t2,inlineTrivExp env e2) (inlineTrivExp env e3)

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
  , mode      :: Mode
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
         , mode  = ToExe }

configParser :: Parser Config
configParser = Config <$> inputParser <*> modeParser
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
               flag' Interp2 (long "interp2" <> help "run through the interpreter after cursor insertion") <|>
               flag' RunExe  (long "run"     <> help "compile and then run executable") <|>  
               flag ToExe ToExe (long "exe"  <> help "compile through C to executable (default)")

-- | Parse configuration as well as file arguments.
configWithArgs :: Parser (Config,[FilePath])
configWithArgs = (,) <$> configParser 
                     <*> some (argument str (metavar "FILES..."
                                             <> help "Files to compile."))

----------------------------------------

-- | Command line version of the compiler entrypoint.  Parses command
-- line arguments given as string inputs.
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
lvl = 2

-- | Compiler entrypoint, given a full configuration and a list of
-- files to process.
compile :: Config -> FilePath -> IO ()
-- compileFile :: (FilePath -> IO (L1.Prog,Int)) -> FilePath -> IO ()
compile Config{input,mode} fp = do 
  let parser = case input of
                 Haskell -> HS.parseFile
                 SExpr   -> SExp.parseFile
                 Unspecified -> 
                   case takeExtension fp of
                     ".hs"   -> HS.parseFile
                     ".sexp" -> SExp.parseFile
                     ".rkt"  -> SExp.parseFile
                     oth -> error$ "compile: unrecognized file extension: "++
                                   show oth++"  Please specify compile input format."
  (l1,cnt0) <- parser fp
  let printParse l = dbgPrintLn l $ sdoc l1
  if mode == ToParse
   then do -- dbgPrintLn lvl "Parsed program:"
           -- dbgPrintLn l sepline
           printParse 0
   else do 
    dbgPrintLn lvl "Compiler pipeline starting, parsed program:"
    dbgPrintLn lvl sepline
    printParse lvl
    let pass :: (Out b, NFData a, NFData b) => String -> (a -> SyM b) -> a -> StateT Int IO b
        pass who fn x = do
          cnt <- get
          _ <- lift $ evaluate $ force x
          let (y,cnt') = runSyM cnt (fn x)
          put cnt'
          lift$ dbgPrintLn lvl $ "\nPass output, " ++who++":"
          lift$ dbgPrintLn lvl sepline
          _ <- lift $ evaluate $ force y
          lift$ dbgPrintLn lvl $ sdoc y
          return y

        -- No reason to chatter from passes that are stubbed out anyway:
        pass' :: (Out b, NFData b) => String -> (a -> SyM b) -> a -> StateT Int IO b
        pass' _ fn x = do
          cnt <- get;
          let (y,cnt') = runSyM cnt (fn x);
          put cnt';
          _ <- lift $ evaluate $ force y;
          return y

    when (mode == Interp1) $ 
      error "Early-phase interpreter not implemented yet!"

    let outfile = (replaceExtension fp ".c")
        exe     = replaceExtension fp ".exe"
    
    clearFile outfile
    clearFile exe
    str <- evalStateT
             (do l1b <- pass "freshNames"               freshNames               l1
                 l1c <- pass "flatten"                  flatten                  l1b
                 l2  <- pass  "inferEffects"             inferEffects             l1c
                 mt  <- pass' "findMissingTraversals"    findMissingTraversals    l2
                 l2b <- pass' "addTraversals"            (addTraversals mt)       l2
                 l2c <- pass' "addCopies"                addCopies                l2b
                 l2d <- pass' "lowerCopiesAndTraversals" lowerCopiesAndTraversals l2c
                 l2e <- pass  "cursorize"                cursorize                l2d
                 l3  <- pass  "lower"                    lower                    l2e

                 if mode == Interp2
                  then error "FINISHME - call Target interpreter"
                  else do
                   str <- lift (codegenProg l3)
                   lift$ dbgPrintLn lvl $ "\nFinal C codegen:"
                   lift$ dbgPrintLn lvl sepline
                   lift$ dbgPrintLn lvl str
                   return str)
              cnt0
    
    writeFile outfile str
    when (mode == ToExe || mode == RunExe) $ do
      cd <- system $ "gcc -std=gnu11 -O3 "++outfile++" -o "++ exe
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
