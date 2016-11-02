{-# LANGUAGE NamedFieldPuns #-}
-- | The compiler pipeline, assembled from several passes.

module Packed.FirstOrder.Compiler
    ( compileSExpFile
    , compileHSFile )
    where

import Packed.FirstOrder.Common
import qualified Packed.FirstOrder.SExpFrontend as SExp
import qualified Packed.FirstOrder.HaskellFrontend as HS
import qualified Packed.FirstOrder.L1_Source as L1 
import Packed.FirstOrder.LTraverse (inferEffects, Prog(..))
import Packed.FirstOrder.Passes.Cursorize (cursorize, lower)
import qualified Packed.FirstOrder.LTraverse as L2
import Packed.FirstOrder.Target (codegenProg,Prog)
import qualified Packed.FirstOrder.Target as L3 
import System.FilePath (replaceExtension)
import Text.PrettyPrint.GenericPretty
import Control.Monad.State
import Control.DeepSeq
import Control.Exception (evaluate)
------------------------------------------------------------
        
import Data.Set as S hiding (map)
import qualified Data.Set as S (map)

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
                         

-- | Put the program in A-normal form where only varrefs and literals
-- are allowed in operand position.
flatten :: L1.Prog -> SyM L1.Prog
flatten p = return p -- TEMP/FIXME
flatten (L1.Prog defs funs main) =
    do main' <- case main of
                  Nothing -> return Nothing
                  Just m -> do m' <- flattenExp m
                               return $ Just m'
       funs' <- flattenFuns funs
       return $ L1.Prog defs funs' main'
    where flattenFuns = undefined
          flattenExp (L1.VarE v) = return $ L1.VarE v
          flattenExp (L1.LitE i) = return $ L1.LitE i
          flattenExp (L1.AppE v (L1.VarE v')) = return $ L1.AppE v (L1.VarE v')
          flattenExp (L1.AppE v e) =
              do e' <- flattenExp e
                 v' <- gensym "tmp_flat"
                 let ty = undefined  -- TODO: get type of argument
                 return $ L1.LetE (v',ty,e') (L1.AppE v (L1.VarE v'))
          flattenExp (L1.PrimAppE p es) =
              do es' <- mapM flattenExp es
                 nams <- mapM gensym $ replicate (length es) "tmp_flat"
                 let bind [] t e = e
                     bind ((v,e'):xs) t e = L1.LetE (v,t,e') $ bind xs t e
                 case p of
                   L1.AddP -> return $ bind (zip nams es') L1.IntTy $
                              L1.PrimAppE L1.AddP $ map L1.VarE nams
                   L1.SubP -> return $ bind (zip nams es') L1.IntTy $
                              L1.PrimAppE L1.SubP $ map L1.VarE nams
                   L1.MulP -> return $ bind (zip nams es') L1.IntTy $
                              L1.PrimAppE L1.MulP $ map L1.VarE nams
                   L1.EqP -> undefined -- TODO: need to look up types
                   L1.DictInsertP -> undefined -- TODO
                   L1.DictLookupP -> undefined
                   L1.ErrorP s -> return $ L1.PrimAppE (L1.ErrorP s) []
          flattenExp (L1.LetE (v,t,e') e) =
              do e' <- flattenExp e'
                 e <- flattenExp e
                 return $ L1.LetE (v,t,e') e
          flattenExp (L1.IfE e1 e2 e3) =
              do e1 <- flattenExp e1
                 e2 <- flattenExp e2
                 e3 <- flattenExp e3
                 v1 <- gensym "tmp_flat"
                 return $ L1.LetE (v1,L1.BoolTy,e1) $ L1.IfE (L1.VarE v1) e2 e3
          flattenExp (L1.ProjE i e) =
              do e <- flattenExp e
                 return $ L1.ProjE i e
          flattenExp (L1.MkProdE es) =
              do es <- mapM flattenExp es
                 nams <- mapM gensym $ replicate (length es) "tmp_flat"
                 let tys = undefined -- TODO: get types
                     bind [] e = e
                     bind ((v,t,e'):xs) e = L1.LetE (v,t,e') $ bind xs e
                 return $ bind (zip3 nams tys es) $ L1.MkProdE $ map L1.VarE nams
          flattenExp (L1.CaseE e mp) =
              do e <- flattenExp e
                 let ty = undefined -- TODO: get type
                 mp <- mapM (\(args,ae) -> (flattenExp ae) >>= (\ae -> return (args,ae))) mp
                 return $ L1.CaseE e mp
          flattenExp (L1.MkPackedE c es) =
              do es <- mapM flattenExp es
                 nams <- mapM gensym $ replicate (length es) "tmp_flat"
                 let tys = undefined
                     bind [] e = e
                     bind ((v,t,e'):xs) e = L1.LetE (v,t,e') $ bind xs e
                 return $ bind (zip3 nams tys es) $ L1.MkPackedE c $ map L1.VarE nams
          flattenExp (L1.TimeIt e) = (flattenExp e) >>= (\e -> return $ L1.TimeIt e)

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
           
     str <- evalStateT
              (do l1b <- pass' "freshNames"               freshNames               l1
                  l1c <- pass' "flatten"                  flatten                  l1b
                  l2  <- pass  "inferEffects"             inferEffects             l1c
                  mt  <- pass' "findMissingTraversals"    findMissingTraversals    l2
                  l2b <- pass' "addTraversals"            (addTraversals mt)       l2
                  l2c <- pass' "addCopies"                addCopies                l2b
                  l2d <- pass' "lowerCopiesAndTraversals" lowerCopiesAndTraversals l2c
                  l2e <- pass  "cursorize"                cursorize                l2d
                  l3  <- pass  "lower"                    lower                    l2e
                  return (codegenProg l3))
               cnt0
     writeFile (replaceExtension fp ".c") str
