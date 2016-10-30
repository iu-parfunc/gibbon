-- | The compiler pipeline, assembled from several passes.

module Packed.FirstOrder.Compiler
    ( compileSExpFile
    , compileHSFile )
    where

import Packed.FirstOrder.Common
import Packed.FirstOrder.SExpFrontend (parseFile)
-- import Packed.FirstOrder.HaskellFrontend (parseFile)
import Packed.FirstOrder.L1_Source hiding (Prog)
import qualified Packed.FirstOrder.L1_Source as L1 
import Packed.FirstOrder.LTraverse as L2 (inferEffects, cursorize, Prog)
import Packed.FirstOrder.Target  as L3 (codegenProg,Prog)
import System.FilePath (replaceExtension)

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

-- | Compile foo.sexp and write the output C code to the corresponding
-- foo.c file.
compileSExpFile :: FilePath -> IO ()
compileSExpFile fp =
  do (l1,cnt) <- parseFile fp
     let (str,_) = runSyM cnt $ do 
                     l1b <- freshNames l1
                     l1c <- flatten  l1b
                     l2  <- inferEffects l1c
                     mt  <- findMissingTraversals l2
                     l2b <- addTraversals mt l2
                     l2c <- addCopies l2b
                     l2d <- lowerCopiesAndTraversals l2c
                     l2e <- cursorize l2d
                     l3  <- lower l2e
                     return (codegenProg l3)
     writeFile (replaceExtension fp ".c") str

lower :: L2.Prog -> SyM L3.Prog
lower = error "FINISHME"

-- | Same as compileSExpFile except starting with a ".hs".
compileHSFile :: FilePath -> IO ()
compileHSFile = error "FINISHME"

