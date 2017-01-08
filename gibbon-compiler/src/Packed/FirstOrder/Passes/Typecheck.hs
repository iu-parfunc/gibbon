
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Packed.FirstOrder.Passes.Typecheck
    ( typecheckStrict
    , typecheckPermissive
    , typecheck
--    , typecheckExp
    ) where

import Packed.FirstOrder.Common
import Packed.FirstOrder.L2_Traverse as L2
import qualified Packed.FirstOrder.L1_Source as L1

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.ST
import Control.Monad
import Data.STRef

lvl :: Int
lvl = 1

data TCVar s = Concrete L1.Ty
             | Fun L1.Ty L1.Ty
             | Alias (STRef s (Either (TCVar s) ()))

instance Show (TCVar s) where
  show (Concrete t) = "Concrete "++show t
  show (Fun a b) = "Fun "++show a++" "++show b
  show (Alias _) = "Alias"
               
type TCEnv s = M.Map Var (TCVar s)

--------------------------------------------------------------------------------
    
-- | Entrypoint with the expected type for a pass in the top-level compiler pipeline.
typecheckStrict :: L2.Prog -> SyM L2.Prog
typecheckStrict prg = if typecheck prg
                      then return prg
                      else error "Compiler exiting due to above type errors."
-- WARNING: depending on DEBUG, "above type errors" may not actually show.
-- !!TODO!!: switch to a writer monad to accumulate error messages, rather than dbgTrace.

-- | In contrast with 'typecheckStrict', this issues type errors only as warnings.
typecheckPermissive :: L2.Prog -> SyM L2.Prog
typecheckPermissive prg = do let !_ = typecheck prg 
                             return prg
                          
                   
-- | Run typecheck and report errors using the trace mechanism.
-- Returns true if the program typechecks.
typecheck :: L2.Prog -> Bool
typecheck prg = runST $ do
                  rf <- newSTRef True
                  !_ <- typecheck' rf prg
                  readSTRef rf
 
typecheck' :: forall s . STRef s Bool -> L2.Prog -> ST s L2.Prog 
typecheck' success prg@(L2.Prog defs _funs _main) = L2.mapMExprs fn prg
 where
  fn Env2{vEnv,fEnv} exp0 =
      do let initTCE = M.map Concrete vEnv `M.union`
                       M.map (\(a,b) -> Fun a b) fEnv
         tv <- typecheckExp defs initTCE exp0
         mty <- extractTCVar tv
         case mty of
           Just ty -> dbgTrace 2 ("Typecheck program returned: " ++ (show ty)) $ 
                      return exp0
           Nothing -> return exp0

  typecheckExp :: DDefs L1.Ty -> TCEnv s -> Exp -> ST s (TCVar s)
  typecheckExp dd tcenv ex0 =
      case ex0 of
        VarE v -> lookupTCVar tcenv v
        LitE _i -> return $ Concrete IntTy
        AppE v e -> 
            do te <- typecheckExp dd tcenv e
               let fty = M.lookup v tcenv
               case fty of
                 Nothing -> failFresh $ "Couldn't look up type of function: " ++ (show v)
                 Just (Fun a b) -> do assertEqTCVar ex0 (Concrete a) te
                                      return (Concrete b)
                 Just oth -> failFresh $ "Function had non-arrow type: " ++ show oth

        PrimAppE p es ->
            do tes <- mapM (typecheckExp dd tcenv) es
               case p of
                 L1.AddP -> do mapM_ (assertEqTCVar ex0 (Concrete IntTy)) tes
                               return $ Concrete IntTy
                 L1.SubP -> do mapM_ (assertEqTCVar ex0 (Concrete IntTy)) tes
                               return $ Concrete IntTy
                 L1.MulP -> do mapM_ (assertEqTCVar ex0 (Concrete IntTy)) tes
                               return $ Concrete IntTy
                 L1.EqIntP -> do mapM_ (assertEqTCVar ex0 (Concrete IntTy)) tes
                                 return $ Concrete BoolTy
                 L1.EqSymP -> do mapM_ (assertEqTCVar ex0 (Concrete SymTy)) tes
                                 return $ Concrete BoolTy                                      
                 -- FIXME: finish cases (need to actually recur)
                 L1.ErrorP _s t -> return $ Concrete t
                 L1.DictEmptyP t -> return $ Concrete $ SymDictTy t
                 L1.DictLookupP t -> return $ Concrete t
                 L1.DictInsertP t -> return $ Concrete $ SymDictTy t

                 L1.SizeParam -> return $ Concrete IntTy 
                 L1.MkTrue    -> return $ Concrete BoolTy
                 L1.MkFalse   -> return $ Concrete BoolTy

                 L1.ReadPackedFile _ ty -> return $ Concrete ty
                                     
                 -- _ -> failFresh $ "Case not handled in typecheck: " ++ (show p)

        LetE (v,t,e') e ->
            do te' <- typecheckExp dd tcenv e'
               assertEqTCVar ex0 (Concrete t) te'
               typecheckExp dd (M.insert v (Concrete t) tcenv) e
        IfE e1 e2 e3 ->
            do te1 <- typecheckExp dd tcenv e1
               te2 <- typecheckExp dd tcenv e2
               te3 <- typecheckExp dd tcenv e3
               assertEqTCVar ex0 (Concrete BoolTy) te1
               assertEqTCVar ex0 te2 te3
               return te2
        ProjE i e ->
            do te <- typecheckExp dd tcenv e
               checkProd te i e
        MkProdE es ->
            do tes <- mapM (typecheckExp dd tcenv) es
               outty <- allConcrete tes es
               case outty of
                 Nothing -> failFresh "Giving up on typing MkProdE"
                 Just outty' ->
                     return $ Concrete outty'
        CaseE e cs ->
            do te <- typecheckExp dd tcenv e
               typecheckCases dd cs te ex0
        MkPackedE c es ->
            do tes <- mapM (typecheckExp dd tcenv) es
               typecheckPacked dd c tes
        TimeIt e t _b ->
            do te <- typecheckExp dd tcenv e
               assertEqTCVar ex0 (Concrete t) te
               return te
        _ -> failFresh $ "Case not handled in typecheckExp: " ++ (show ex0)
   where

    checkProd te i e =
      let fail = failFresh $ "In checking " ++ (show e) ++
                  ", I expected a product type, but found " ++ (show te) ++ "." in
      case te of
        Concrete (ProdTy ts) -> return $ Concrete $ ts !! i
        Alias r -> do r' <- readSTRef r
                      case r' of
                        Left tcv -> checkProd tcv i e
                        Right () -> failFresh $ "I ran into a ProjE of " ++ (show e) ++
                                            ", but I don't know the type of that thing."
        Concrete _ -> fail
        Fun _ _    -> fail


    allConcrete [] _ = return $ Just $ ProdTy [] -- Unit type.
    allConcrete ((Concrete t1):t2:ts) es = do pts <- allConcrete (t2:ts) es
                                              case pts of
                                                Nothing -> return Nothing
                                                Just (ProdTy ts') -> 
                                                    return $ Just $ ProdTy $ t1:ts'
                                                Just _ -> error "impossible."

    -- We don't allow singleton tuples in general, but here we plan to
    -- recursively build up the product types.
    allConcrete [(Concrete t)] _es = return $ Just $ ProdTy [t]
    allConcrete _ es = do reportErr $ "Tried to do a MkProdE of " ++ (show es) ++
                                      " but couldn't determine its type."
                          return Nothing

    typecheckCases dd1 cs _te1 ex1 = 
        do tcs <- forM cs $ \(c,args,e) ->
                  do let targs = map Concrete $ lookupDataCon dd c
                         ntcenv = M.fromList (zip args targs) `M.union` tcenv
                     typecheckExp dd1 ntcenv e
           foldM_ (\i a -> (assertEqTCVar ex1 i a) >> (return a)) (head tcs) (tail tcs)
           -- FIXME: need to assert that the types in tcs match what's expected from te
           return $ head tcs

    typecheckPacked dd1 c _tes =
        do let te = Concrete $ L1.Packed $ getTyOfDataCon dd1 c
           -- FIXME: need to assert that tes match te
           return te

  --  This is not top-level because it is under the scope of 'success':
  reportErr :: String -> ST s ()
  reportErr str = do
    writeSTRef success False 
    return $! dbgTrace lvl (" [typecheck] " ++ str) ()

  failFresh :: String -> ST s (TCVar s)
  failFresh str = do reportErr str
                     r <- newSTRef $ Right ()
                     return $ Alias r

  lookupTCVar :: TCEnv s -> Var -> ST s (TCVar s)
  lookupTCVar tcenv v =
      case M.lookup v tcenv of
        Nothing -> do reportErr $ "Failed to look up type of var " ++ (show v)
                      h <- newSTRef $ Right ()
                      return $ Alias h
        Just tcv -> return tcv

  assertEqTCVar :: Exp -> TCVar s -> TCVar s -> ST s ()
  assertEqTCVar e (Concrete t1) (Concrete t2) = 
      if t1 == t2
      then return ()
      else reportErr $ "Types not equal: " ++ (show t1) ++ ", " ++ (show t2)
                      ++ " (when checking expression " ++ take 80 (show e) ++ "...)"
  assertEqTCVar e (Concrete t) (Alias a) = makeEqTCVar t a e
  assertEqTCVar e (Alias a) (Concrete t) = makeEqTCVar t a e
  assertEqTCVar e (Alias a1) (Alias a2) = makeEqAlias a1 a2 e

  makeEqTCVar :: L1.Ty -> STRef s (Either (TCVar s) ()) -> Exp -> ST s ()
  makeEqTCVar t r e =
      do r' <- readSTRef r
         case r' of
           Left tcv -> assertEqTCVar e tcv (Concrete t)
           Right () -> writeSTRef r $ Left $ Concrete t

  makeEqAlias :: STRef s (Either (TCVar s) ()) -> STRef s (Either (TCVar s) ()) -> Exp -> ST s ()
  makeEqAlias r1 r2 e =
      do r1' <- readSTRef r1
         r2' <- readSTRef r2
         case (r1',r2') of
              (Right (), Right ()) ->
                  do r <- newSTRef $ Right ()
                     writeSTRef r1 $ Left $ Alias r
                     writeSTRef r2 $ Left $ Alias r
              (Left tcv, Right ()) -> writeSTRef r2 $ Left tcv
              (Right (), Left tcv) -> writeSTRef r1 $ Left tcv
              (Left tc1, Left tc2) -> assertEqTCVar e tc1 tc2

  extractTCVar :: (TCVar s) -> ST s (Maybe L1.Ty)
  extractTCVar (Concrete t) = return (Just t)
  extractTCVar (Alias r) =
      do r' <- readSTRef r
         case r' of
           Left tcv -> extractTCVar tcv
           Right () -> do reportErr "Expression didn't have a type that I could figure out"
                          return Nothing

