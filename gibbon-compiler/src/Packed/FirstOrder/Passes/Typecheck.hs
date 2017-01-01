
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
import Control.Monad.ST
import Control.Monad
import Data.STRef

lvl :: Int
lvl = 1

data TCVar s = Concrete L1.Ty
             | Alias (STRef s (Either (TCVar s) ()))

type TCEnv s = M.Map Var (TCVar s)

--------------------------------------------------------------------------------
    
-- | Entrypoint with the expected type for a pass in the top-level compiler pipeline.
typecheckStrict :: L2.Prog -> SyM L2.Prog
typecheckStrict prg = if typecheck prg
                      then return prg
                      else error "Exiting due to above type errors."


-- | In contrast with 'typecheckStrict', this issues type errors only as warnings.
typecheckPermissive :: L2.Prog -> SyM L2.Prog
typecheckPermissive prg = do let !_ = typecheck prg 
                             return prg
                           
-- !!TODO!!: switch to a writer monad to accumulate error messages, rather than dbgTrace.
                   
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
  fn env2 exp0 = do t <- typecheckExp defs env2 M.empty exp0
                    reportTCVar t
                    return exp0

  typecheckExp :: DDefs L1.Ty -> Env2 L1.Ty -> TCEnv s -> Exp -> ST s (TCVar s)
  typecheckExp dd env2 tcenv ex0 =
      case ex0 of
        VarE v -> lookupTCVar tcenv v
        LitE _i -> return $ Concrete IntTy
        AppE v e -> 
            do te <- typecheckExp dd env2 tcenv e
               let fty = M.lookup v (fEnv env2)
               case fty of
                 Nothing -> failFresh $ "Couldn't look up type of function: " ++ (show v)
                 Just t ->
                     do let tvout = Concrete $ snd t
                            tvin = Concrete $ fst t
                        assertEqTCVar ex0 tvin te
                        return tvout
        PrimAppE p es ->
            do tes <- mapM (typecheckExp dd env2 tcenv) es
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
                 _ -> failFresh $ "Case not handled in typecheck: " ++ (show p)
        LetE (v,t,e') e ->
            do te' <- typecheckExp dd env2 tcenv e'
               assertEqTCVar ex0 (Concrete t) te'
               typecheckExp dd env2 (M.insert v (Concrete t) tcenv) e
        IfE e1 e2 e3 ->
            do te1 <- typecheckExp dd env2 tcenv e1
               te2 <- typecheckExp dd env2 tcenv e2
               te3 <- typecheckExp dd env2 tcenv e3
               assertEqTCVar ex0 (Concrete BoolTy) te1
               assertEqTCVar ex0 te2 te3
               return te2
        ProjE i e ->
            do te <- typecheckExp dd env2 tcenv e
               checkProd te i e
        MkProdE es ->
            do tes <- mapM (typecheckExp dd env2 tcenv) es
               outty <- allConcrete tes es
               case outty of
                 Nothing -> failFresh "Giving up on typing MkProdE"
                 Just outty' ->
                     return $ Concrete outty'
        CaseE e cs ->
            do te <- typecheckExp dd env2 tcenv e
               typecheckCases dd cs te ex0
        MkPackedE c es ->
            do tes <- mapM (typecheckExp dd env2 tcenv) es
               typecheckPacked dd c tes
        TimeIt e t _b ->
            do te <- typecheckExp dd env2 tcenv e
               assertEqTCVar ex0 (Concrete t) te
               return te
        _ -> failFresh $ "Case not handled in typecheckExp: " ++ (show ex0)
   where
    checkProd te i e =
      case te of
        Concrete (ProdTy ts) -> return $ Concrete $ ts !! i
        Concrete t -> failFresh $ "In checking " ++ (show e) ++
                        ", I expected a product type, but found " ++ (show t) ++ "."
        Alias r -> do r' <- readSTRef r
                      case r' of
                        Left tcv -> checkProd tcv i e
                        Right () -> failFresh $ "I ran into a ProjE of " ++ (show e) ++
                                            ", but I don't know the type of that thing."

    allConcrete ((Concrete t):ts) es = do pts <- allConcrete ts es
                                          case pts of
                                            Nothing -> return Nothing
                                            Just (ProdTy ts') -> 
                                                return $ Just $ ProdTy $ t:ts'
    allConcrete [(Concrete t)] _es = return $ Just $ ProdTy [t]
    allConcrete _ es = do reportErr $ "Tried to do a MkProdE of " ++ (show es) ++
                                      " but couldn't determine its type."
                          return Nothing

    typecheckCases dd1 cs te1 ex1 = 
        do tcs <- forM cs $ \(c,args,e) ->
                  do let targs = map Concrete $ lookupDataCon dd c
                         ntcenv = M.fromList (zip args targs) `M.union` tcenv
                     typecheckExp dd1 env2 ntcenv e
           foldM_ (\i a -> (assertEqTCVar ex1 i a) >> (return a)) (head tcs) (tail tcs)
           -- FIXME: need to assert that the types in tcs match what's expected from te
           return $ head tcs

    typecheckPacked dd1 c tes =
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
              -- FIXME: we shouldn't give up if we run into two fresh type vars
              (Right (), Right ()) -> reportErr "Typecheck is giving up on a branch. Program must be very broken."
              (Left tcv, Right ()) -> writeSTRef r2 $ Left tcv
              (Right (), Left tcv) -> writeSTRef r1 $ Left tcv
              (Left tc1, Left tc2) -> assertEqTCVar e tc1 tc2

  reportTCVar :: (TCVar s) -> ST s ()
  reportTCVar (Concrete t) = reportErr $ "Typecheck returned: " ++ (show t)
  reportTCVar (Alias r) =
      do r' <- readSTRef r
         case r' of
           Left tcv -> reportTCVar tcv
           Right () -> reportErr "Expression didn't have a type that I could figure out"

