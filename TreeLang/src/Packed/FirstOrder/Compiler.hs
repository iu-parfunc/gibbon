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
import qualified Data.Map as M
import Data.Maybe (fromJust, catMaybes)

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


-- | Put the program in A-normal form where only varrefs and literals
-- are allowed in operand position.
flatten :: L1.Prog -> SyM L1.Prog
-- flatten p = return p -- TEMP/FIXME
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
                 let ty = funRetTy $ fromJust $ M.lookup v funs 
                 return $ L1.LetE (v',ty,e') (L1.AppE v (L1.VarE v'))
          flattenExp env (L1.PrimAppE p es) =
              do es' <- mapM (flattenExp env) es
                 nams <- mapM gensym $ replicate (length es) "tmp_flat"
                 let bind [] _t e = e
                     bind ((v,e'):xs) t e = L1.LetE (v,t,e') $ bind xs t e
                 case p of
                   L1.AddP -> return $ bind (zip nams es') L1.IntTy $
                              L1.PrimAppE L1.AddP $ map L1.VarE nams
                   L1.SubP -> return $ bind (zip nams es') L1.IntTy $
                              L1.PrimAppE L1.SubP $ map L1.VarE nams
                   L1.MulP -> return $ bind (zip nams es') L1.IntTy $
                              L1.PrimAppE L1.MulP $ map L1.VarE nams
                   L1.EqP -> return $ bind (zip nams es') L1.IntTy $ -- NOTE: only for ints!
                              L1.PrimAppE L1.EqP $ map L1.VarE nams
                   L1.DictInsertP -> undefined -- TODO/FIXME
                   L1.DictLookupP -> undefined -- TODO/FIXME
                   L1.ErrorP s t -> return $ L1.PrimAppE (L1.ErrorP s t) []
          flattenExp env (L1.LetE (v,t,e') e) =
              do fe' <- flattenExp env e'
                 fe <- flattenExp env e
                 return $ L1.LetE (v,t,fe') fe
          flattenExp env (L1.IfE e1 e2 e3) =
              do fe1 <- flattenExp env e1
                 fe2 <- flattenExp env e2
                 fe3 <- flattenExp env e3
                 v1 <- gensym "tmp_flat"
                 return $ L1.LetE (v1,L1.BoolTy,fe1) $ L1.IfE (L1.VarE v1) fe2 fe3
          flattenExp env (L1.ProjE i e) =
              do fe <- flattenExp env e
                 return $ L1.ProjE i fe
          flattenExp env (L1.MkProdE es) =
              do fes <- mapM (flattenExp env) es
                 nams <- mapM gensym $ replicate (length fes) "tmp_flat"
                 let tys = map (typeExp env) fes
                     bind [] e = e
                     bind ((v,t,e'):xs) e = L1.LetE (v,t,e') $ bind xs e
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
                 return $ L1.LetE (v,ty,fe) $ L1.CaseE (L1.VarE v) fmp
          flattenExp env (L1.MkPackedE c es) =
              do fes <- mapM (flattenExp env) es
                 nams <- mapM gensym $ replicate (length fes) "tmp_flat"
                 let tys = map (typeExp env) fes
                     bind [] e = e
                     bind ((v,t,e'):xs) e = L1.LetE (v,t,e') $ bind xs e
                 return $ bind (zip3 nams tys fes) $ L1.MkPackedE c $ map L1.VarE nams
          flattenExp env (L1.TimeIt e) = (flattenExp env e) >>= (\fe -> return $ L1.TimeIt fe)

          typeExp :: [(Var,L1.Ty)] -> L1.Exp -> L1.Ty
          typeExp env (L1.VarE v) = fromJust $ lookup v env
          typeExp _env (L1.LitE _i) = L1.IntTy
          typeExp _env (L1.AppE v _e) = funRetTy $ fromJust $ M.lookup v funs
          typeExp _env (L1.PrimAppE p _es) =
              case p of
                L1.AddP -> L1.IntTy
                L1.SubP -> L1.IntTy
                L1.MulP -> L1.IntTy
                L1.EqP -> L1.BoolTy
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

                  str <- lift (codegenProg l3)
                  lift$ dbgPrintLn lvl $ "\nFinal C codegen:"
                  lift$ dbgPrintLn lvl sepline
                  lift$ dbgPrintLn lvl str
                  return str)
               cnt0

     writeFile (replaceExtension fp ".c") str
