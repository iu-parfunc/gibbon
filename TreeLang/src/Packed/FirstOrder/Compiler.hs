-- | The compiler pipeline, assembled from several passes.

module Packed.FirstOrder.Compiler
    ( compileSExpFile
    , compileHSFile )
    where

import Packed.FirstOrder.Common
import qualified Packed.FirstOrder.SExpFrontend as SExp
import qualified Packed.FirstOrder.HaskellFrontend as HS
import qualified Packed.FirstOrder.L1_Source as L1 
import Packed.FirstOrder.LTraverse as L2 (inferEffects, cursorize, Prog)
import Packed.FirstOrder.Target  as L3 (codegenProg,Prog)
import System.FilePath (replaceExtension)
import Text.PrettyPrint.GenericPretty
import Control.Monad.State
    
------------------------------------------------------------
        
import Data.Set as S

----------------------------------------   
-- PASS STUBS
----------------------------------------   
-- All of these need to be implemented, but are just the identity
-- function for now.

-- | Rename all local variables 
freshNames :: L1.Prog -> SyM L1.Prog
freshNames = return

-- | Put the program in A-normal form where only varrefs and literals
-- are allowed in operand position.
flatten :: L1.Prog -> SyM L1.Prog
flatten = return


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

-- | Convert into the target language.  This does not make much of a
-- change, but it checks the changes that have already occurred.
lower :: L2.Prog -> SyM L3.Prog
lower = error "FINISHME"


--------------------------------------------------------------------------------
                             
-- | Compile foo.sexp and write the output C code to the corresponding
-- foo.c file.
compileSExpFile :: FilePath -> IO ()
compileSExpFile = compileFile SExp.parseFile

-- | Same as compileSExpFile except starting with a ".hs".
compileHSFile :: FilePath -> IO ()
compileHSFile = compileFile HS.parseFile

sepline :: String
sepline = replicate 80 '='
                   

compileFile :: (FilePath -> IO (L1.Prog,Int)) -> FilePath -> IO ()
compileFile parser fp =
  do (l1,cnt0) <- parser fp

     let lvl = 2
     dbgPrintLn lvl "Compiler pipeline starting, parsed program:"
     dbgPrintLn lvl sepline
     dbgPrintLn lvl $ sdoc l1
     let pass :: Out b => String -> (a -> SyM b) -> a -> StateT Int IO b
         pass who fn x = do
           cnt <- get
           let (y,cnt') = runSyM cnt (fn x)
           put cnt'
           lift$ dbgPrintLn lvl $ "\nPass output, " ++who++":"
           lift$ dbgPrintLn lvl sepline
           lift$ dbgPrintLn lvl $ sdoc y
           return y
           
     str <- evalStateT
              (do l1b <- pass "freshNames"               freshNames               l1
                  l1c <- pass "flatten"                  flatten                  l1b
                  l2  <- pass "inferEffects"             inferEffects             l1c
                  mt  <- pass "findMissingTraversals"    findMissingTraversals    l2
                  l2b <- pass "addTraversals"            (addTraversals mt)       l2
                  l2c <- pass "addCopies"                addCopies                l2b
                  l2d <- pass "lowerCopiesAndTraversals" lowerCopiesAndTraversals l2c
                  l2e <- pass "cursorize"                cursorize                l2d
                  l3  <- pass "lower"                    lower                    l2e
                  return (codegenProg l3))
               cnt0
     writeFile (replaceExtension fp ".c") str
