{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE InstanceSigs #-}

module Gibbon.L0.Typecheck where

import           Control.Monad.State ( MonadState )
import           Control.Monad.Except
#if !MIN_VERSION_base(4,13,0)
-- https://downloads.haskell.org/ghc/8.8.1/docs/html/users_guide/8.8.1-notes.html
import           Control.Monad.Fail(MonadFail(..))
#endif
import           Data.Foldable ( foldlM )
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Safe as Sf

import           Text.PrettyPrint hiding ( (<>) )
import           Text.PrettyPrint.GenericPretty
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup
#endif

import           Gibbon.L0.Syntax as L0
import           Gibbon.Common
import           Data.Function
import           Data.Bitraversable

--------------------------------------------------------------------------------

newtype TcM a = TcM (ExceptT Doc PassM a)
  deriving (Functor, Applicative, Monad, MonadError Doc, MonadState Int)

instance MonadFail TcM where
  fail = error

runTcM :: TcM a -> PassM (Either Doc a)
runTcM (TcM tc) = runExceptT tc

err :: Doc -> TcM a
err d = throwError ("L0.Typecheck: " $$ nest 4 d)

tcProg :: Prog0 -> PassM Prog0
tcProg prg@Prog{ddefs,fundefs,mainExp} = do
  let init_fenv = M.map funTy fundefs
  fundefs_tc <- mapM (tcFun ddefs init_fenv) fundefs
  -- generalize top level functions, e.g. `foo x = x :: $0 -> $0` to `foo x = x :: forall x. x -> x`
  fundefs' <-  mapM (\fndef -> do
                              fnty'' <- either (error . render) id <$> runTcM (snd <$> generalize M.empty emptySubst [] (tyFromScheme (funTy fndef)))
                              pure $ fndef {funTy = fnty''}
                           ) fundefs_tc
  let fenv = M.map funTy fundefs'
  -- Run the typechecker on the expression, and update it's type in the program
  -- (the parser initializes the main expression with the void type).
  mainExp' <- case mainExp of
                Nothing -> pure Nothing
                Just (e,gvn_main_ty) -> do
                  let tc = do
                              (s1, drvd_main_ty, e_tc) <-
                                tcExp ddefs emptySubst M.empty fenv [] True e
                              s2 <- unify e gvn_main_ty drvd_main_ty
                              -- let e_tc' = fixTyApps s1 e_tc
                              pure (s1 <> s2, zonkTy s2 drvd_main_ty, e_tc)
                  res <- runTcM tc
                  case res of
                    Left er -> error (render er)
                    Right (_s, ty, e_tc) -> pure $ Just (e_tc, ty)
  pure prg { fundefs = fundefs'
           , mainExp = mainExp' }

tcFun :: DDefs0 -> Gamma -> FunDef0 -> PassM FunDef0
tcFun ddefs fenv fn@FunDef{funArgs,funTy,funBody, funName} = do
  res <- runTcM $ do
    let (ForAll tyvars (ArrowTy gvn_arg_tys gvn_retty)) = funTy
        init_venv = M.fromList $ zip funArgs $ map (ForAll []) gvn_arg_tys
        init_s = emptySubst
    (s1, drvd_funBody_ty, funBody_tc) <-
      tcExp ddefs init_s init_venv fenv tyvars False funBody
    s2 <- unify funBody drvd_funBody_ty gvn_retty
    pure $ fn { funTy   = zonkTyScheme (s1 <> s2) funTy
              , funBody = zonkExp (s1 <> s2) funBody_tc }
  case res of
    Left er   -> error $ render er ++ " in " ++ show funName
    Right fn1 -> pure fn1

tcExps :: DDefs0 -> Subst -> Gamma -> Gamma -> [TyVar]
       -> [(Bool, Exp0)] -> TcM (Subst, [Ty0], [Exp0])
tcExps ddefs sbst venv fenv bound_tyvars ls = do
  (sbsts,tys,exps) <- unzip3 <$> mapM go ls
  pure (foldl (<>) sbst sbsts, tys, exps)
  where
    go (is_main, e) = tcExp ddefs sbst venv fenv bound_tyvars is_main e

--
tcExp :: DDefs0 -> Subst -> Gamma -> Gamma -> [TyVar]
      -> Bool -> Exp0 -> TcM (Subst, Ty0, Exp0)
tcExp ddefs sbst venv fenv bound_tyvars is_main ex = (\(a,b,c) -> (a,b,c)) <$>
  case ex of
    VarE x -> do
      (metas, ty) <-
        case (M.lookup x venv, M.lookup x fenv) of
          (Nothing, Nothing) -> err $ text "Unbound variable " <> doc x
          (Just t, _) -> instantiate t
          (_, Just t) -> instantiate t
      if isFunTy ty
      then pure (sbst, ty, Ext $ FunRefE metas x)
      else pure (sbst, ty, VarE x)

    LitE{}    -> pure (sbst, IntTy, ex)
    CharE{}   -> pure (sbst, CharTy, ex)
    FloatE{}  -> pure (sbst, FloatTy, ex)
    LitSymE{} -> pure (sbst, SymTy0, ex)

    AppE f _tyapps args -> do
      (sigma, (metas, fn_ty_inst)) <-
        case (M.lookup f venv, M.lookup f fenv) of
          (Just lam_ty, _) -> (lam_ty,) <$> instantiate lam_ty
          (_, Just fn_ty)  -> (fn_ty,) <$> instantiate fn_ty
          _ -> err $ text "Unknown function:" <+> doc f

      if isCallUnsaturated sigma args
      then do
        e' <- saturateCall sigma ex
        (s1, e_ty, e'') <- tcExp ddefs sbst venv fenv bound_tyvars is_main e'
        pure (s1, e_ty, e'')
      else do
        (s2, arg_tys, args_tc) <- tcExps ddefs sbst venv fenv bound_tyvars (zip (repeat is_main) args)
        -- let fn_ty_inst' = zonkTy s2 fn_ty_inst
        let fn_ty_inst' = fn_ty_inst
        s3 <- unifyl ex (arrIns' fn_ty_inst') arg_tys
        fresh <- newMetaTy
        s4 <- unify ex (ArrowTy arg_tys fresh) fn_ty_inst'
        -- Fill in type applications for specialization...
        --     id 10 ===> id [Int] 10
        let tyapps = map (zonkTy s3) metas
            s5 = s2 <> s3 <> s4
        pure (s5, zonkTy s5 fresh, AppE f tyapps (map (zonkExp s5) args_tc))

    PrimAppE pr args -> do
      (s1, arg_tys, args_tc) <- tcExps ddefs sbst venv fenv bound_tyvars (zip (repeat is_main) args)
      let arg_tys' = map (zonkTy s1) arg_tys
          checkLen :: Int -> TcM ()
          checkLen n =
            if length args == n
            then pure ()
            else err $ text "Wrong number of arguments to " <+> doc pr <+> text "."
                         $$ text "Expected " <+> doc n
                         <+> text ", received " <+> doc (length args)
                         $$ exp_doc
          len0 = checkLen 0
          len1 = checkLen 1
          len2 = checkLen 2
          len3 = checkLen 3
          len4 = checkLen 4

          mk_bools = do
            len0
            pure (s1, BoolTy, PrimAppE pr args_tc)

          bool_ops = do
            len2
            s2 <- unify (args !! 0) BoolTy (arg_tys' !! 0)
            s3 <- unify (args !! 1) BoolTy (arg_tys' !! 1)
            pure (s1 <> s2 <> s3, BoolTy, PrimAppE pr args_tc)

          int_ops = do
            len2
            s2 <- unify (args !! 0) IntTy (arg_tys' !! 0)
            s3 <- unify (args !! 1) IntTy (arg_tys' !! 1)
            pure (s1 <> s2 <> s3, IntTy, PrimAppE pr args_tc)

          int_cmps = do
            len2
            s2 <- unify (args !! 0) IntTy (arg_tys' !! 0)
            s3 <- unify (args !! 1) IntTy (arg_tys' !! 1)
            pure (s1 <> s2 <> s3, BoolTy, PrimAppE pr args_tc)

          float_ops = do
            len2
            s2 <- unify (args !! 0) FloatTy (arg_tys' !! 0)
            s3 <- unify (args !! 1) FloatTy (arg_tys' !! 1)
            pure (s1 <> s2 <> s3, FloatTy, PrimAppE pr args_tc)

          float_cmps = do
            len2
            s2 <- unify (args !! 0) FloatTy (arg_tys' !! 0)
            s3 <- unify (args !! 1) FloatTy (arg_tys' !! 1)
            pure (s1 <> s2 <> s3, BoolTy, PrimAppE pr args_tc)
          
          char_cmps = do
            len2
            s2 <- unify (args !! 0) CharTy (arg_tys' !! 0)
            s3 <- unify (args !! 1) CharTy (arg_tys' !! 1)
            pure (s1 <> s2 <> s3, BoolTy, PrimAppE pr args_tc)

      case pr of
        MkTrue  -> mk_bools
        MkFalse -> mk_bools
        AddP    -> int_ops
        SubP    -> int_ops
        MulP    -> int_ops
        DivP    -> int_ops
        ModP    -> int_ops
        ExpP    -> int_ops
        FAddP   -> float_ops
        FSubP   -> float_ops
        FMulP   -> float_ops
        FDivP   -> float_ops
        FExpP   -> float_ops
        EqIntP  -> int_cmps
        LtP     -> int_cmps
        GtP     -> int_cmps
        LtEqP   -> int_cmps
        GtEqP   -> int_cmps
        EqFloatP -> float_cmps
        EqCharP  -> char_cmps
        FLtP     -> float_cmps
        FGtP     -> float_cmps
        FLtEqP   -> float_cmps
        FGtEqP   -> float_cmps
        OrP     -> bool_ops
        AndP    -> bool_ops

        Gensym -> len0 >>= \_ -> pure (sbst, SymTy0, PrimAppE pr args_tc)

        EqSymP -> do
          len2
          s2 <- unify (args !! 0) SymTy0 (arg_tys' !! 0)
          s3 <- unify (args !! 1) SymTy0 (arg_tys' !! 1)
          pure (s1 <> s2 <> s3, BoolTy, PrimAppE pr args_tc)

        EqBenchProgP _ -> do
          len0
          pure (s1, BoolTy, PrimAppE pr args_tc)

        RandP -> pure (s1, IntTy, PrimAppE pr args_tc)
        FRandP-> pure (s1, FloatTy, PrimAppE pr args_tc)
        FSqrtP -> do
          len1
          s2 <- unify (args !! 0) FloatTy (arg_tys' !! 0)
          pure (s1 <> s2, FloatTy, PrimAppE pr args_tc)

        FTanP -> do
          len1
          s2 <- unify (args !! 0) FloatTy (arg_tys' !! 0)
          pure (s1 <> s2, FloatTy, PrimAppE pr args_tc)

        FloatToIntP -> do
          len1
          s2 <- unify (args !! 0) FloatTy (arg_tys' !! 0)
          pure (s1 <> s2, IntTy, PrimAppE pr args_tc)

        IntToFloatP -> do
          len1
          s2 <- unify (args !! 0) IntTy (arg_tys' !! 0)
          pure (s1 <> s2, FloatTy, PrimAppE pr args_tc)

        PrintInt -> do
          len1
          s2 <- unify (args !! 0) IntTy (arg_tys' !! 0)
          pure (s1 <> s2, ProdTy [], PrimAppE pr args_tc)

        PrintChar -> do
          len1
          s2 <- unify (args !! 0) CharTy (arg_tys' !! 0)
          pure (s1 <> s2, ProdTy [], PrimAppE pr args_tc)

        PrintFloat -> do
          len1
          s2 <- unify (args !! 0) FloatTy (arg_tys' !! 0)
          pure (s1 <> s2, ProdTy [], PrimAppE pr args_tc)

        PrintBool -> do
          len1
          s2 <- unify (args !! 0) BoolTy (arg_tys' !! 0)
          pure (s1 <> s2, ProdTy [], PrimAppE pr args_tc)

        PrintSym -> do
          len1
          s2 <- unify (args !! 0) SymTy0 (arg_tys' !! 0)
          pure (s1 <> s2, ProdTy [], PrimAppE pr args_tc)

        ReadInt -> do
          len0
          pure (s1, IntTy, PrimAppE pr args_tc)

        SymSetEmpty -> do
          len0
          pure (s1, SymSetTy, PrimAppE pr args_tc)

        SymSetInsert -> do
          len2
          s2 <- unify (args !! 0) SymSetTy (arg_tys' !! 0)
          s3 <- unify (args !! 1) SymTy0 (arg_tys' !! 1)
          pure (s1 <> s2 <> s3, SymSetTy, PrimAppE pr args_tc)

        SymSetContains -> do
          len2
          s2 <- unify (args !! 0) SymSetTy (arg_tys' !! 0)
          s3 <- unify (args !! 1) SymTy0 (arg_tys' !! 1)
          pure (s1 <> s2 <> s3, BoolTy, PrimAppE pr args_tc)

        SymHashEmpty -> do
          len0
          pure (s1, SymHashTy, PrimAppE pr args_tc)

        SymHashInsert -> do
          len3
          s2 <- unify (args !! 0) SymHashTy (arg_tys' !! 0)
          s3 <- unify (args !! 1) SymTy0 (arg_tys' !! 1)
          s4 <- unify (args !! 2) SymTy0 (arg_tys' !! 2)
          pure (s1 <> s2 <> s3 <> s4, SymHashTy, PrimAppE pr args_tc)

        SymHashLookup -> do
          len2
          s2 <- unify (args !! 0) SymHashTy (arg_tys' !! 0)
          s3 <- unify (args !! 1) SymTy0 (arg_tys' !! 1)
          pure (s1 <> s2 <> s3, SymTy0, PrimAppE pr args_tc)

        SymHashContains -> do
          len2
          s2 <- unify (args !! 0) SymHashTy (arg_tys' !! 0)
          s3 <- unify (args !! 1) SymTy0 (arg_tys' !! 1)
          pure (s1 <> s2 <> s3, BoolTy, PrimAppE pr args_tc)

        DictEmptyP ty -> do
          len1
          let [a] = arg_tys'
          s2 <- unify (args !! 0) ArenaTy a
          case args !! 0 of
            (VarE var) ->
                pure (s1 <> s2, SymDictTy (Just var) ty,
                         PrimAppE pr args_tc)
            Ext (L0.L _ (VarE var)) ->
                pure (s1 <> s2, SymDictTy (Just var) ty,
                         PrimAppE pr args_tc)
            _ -> err $ text "Expected arena variable argument in: " <+> exp_doc

        DictInsertP ty -> do
          len4
          let [a,d,k,v] = arg_tys'
          s2 <- unify (args !! 1) (SymDictTy Nothing ty) d
          s3 <- unify (args !! 2) SymTy0 k
          s4 <- unify (args !! 3) ty v
          s5 <- unify (args !! 0) ArenaTy a
          case args !! 0 of
            (VarE var) -> pure (s1 <> s2 <> s3 <> s4 <> s5,
                                       SymDictTy (Just var) ty,
                                       PrimAppE pr args_tc)
            Ext (L0.L _ (VarE var)) -> pure (s1 <> s2 <> s3 <> s4 <> s5,
                                       SymDictTy (Just var) ty,
                                       PrimAppE pr args_tc)
            _ -> err $ text "Expected arena variable argument in: " <+> exp_doc

        DictLookupP ty -> do
          len2
          let [d,k] = arg_tys'
          s2 <- unify (args !! 0) (SymDictTy Nothing ty) d
          s3 <- unify (args !! 1) SymTy0 k
          pure (s1 <> s2 <> s3, ty, PrimAppE pr args_tc)

        DictHasKeyP ty -> do
          len2
          let [d,k] = arg_tys'
          s2 <- unify (args !! 0) (SymDictTy Nothing ty) d
          s3 <- unify (args !! 1) SymTy0 k
          pure (s1 <> s2 <> s3, BoolTy, PrimAppE pr args_tc)

        IntHashEmpty -> do
          len0
          pure (s1, IntHashTy, PrimAppE pr args_tc)

        IntHashInsert -> do
          len3
          s2 <- unify (args !! 0) IntHashTy (arg_tys' !! 0)
          s3 <- unify (args !! 1) SymTy0 (arg_tys' !! 1)
          s4 <- unify (args !! 2) IntTy (arg_tys' !! 2)
          pure (s1 <> s2 <> s3 <> s4, IntHashTy, PrimAppE pr args_tc)

        IntHashLookup -> do
          len2
          s2 <- unify (args !! 0) IntHashTy (arg_tys' !! 0)
          s3 <- unify (args !! 1) SymTy0 (arg_tys' !! 1)
          pure (s1 <> s2 <> s3, IntTy, PrimAppE pr args_tc)

        VAllocP elty -> do
          len1
          let [i] = arg_tys'
          s2 <- unify (args !! 0) IntTy i
          pure (s1 <> s2, VectorTy elty, PrimAppE pr args_tc)

        VFreeP elty -> do
          len1
          let [i] = arg_tys'
          s2 <- unify (args !! 0) (VectorTy elty) i
          pure (s1 <> s2, ProdTy [], PrimAppE pr args_tc)

        VFree2P elty -> do
          len1
          let [i] = arg_tys'
          s2 <- unify (args !! 0) (VectorTy elty) i
          pure (s1 <> s2, ProdTy [], PrimAppE pr args_tc)

        VLengthP elty -> do
          len1
          let [ls] = arg_tys'
          s2 <- unify (args !! 0) (VectorTy elty) ls
          pure (s1 <> s2, IntTy, PrimAppE pr args_tc)

        VNthP elty -> do
          len2
          let [ls,i] = arg_tys'
          s2 <- unify (args !! 0) (VectorTy elty) ls
          s3 <- unify (args !! 1) IntTy i
          pure (s1 <> s2 <> s3, elty, PrimAppE pr args_tc)

        VSliceP elty -> do
          len3
          let [from,to,ls] = arg_tys'
          s2 <- unify (args !! 0) IntTy from
          s3 <- unify (args !! 1) IntTy to
          s4 <- unify (args !! 2) (VectorTy elty) ls
          pure (s1 <> s2 <> s3 <> s3 <> s4, VectorTy elty, PrimAppE pr args_tc)

        InplaceVUpdateP elty -> do
          len3
          let [ls,i,val] = arg_tys'
          s2 <- unify (args !! 0) (VectorTy elty) ls
          s3 <- unify (args !! 1) IntTy i
          s4 <- unify (args !! 2) elty val
          pure (s1 <> s2 <> s3 <> s4, VectorTy elty, PrimAppE pr args_tc)

        VConcatP elty -> do
          len1
          let [ls] = arg_tys'
          s2 <- unify (args !! 0) (VectorTy (VectorTy elty)) ls
          pure (s1 <> s2, VectorTy elty, PrimAppE pr args_tc)

        -- Given that the first argument is a list of type (VectorTy t),
        -- ensure that the 2nd argument is function reference of type:
        -- ty -> ty -> IntTy
        --
        -- TODO: cannot unify if the 2nd argument is a lambda.
        VSortP elty -> do
          len2
          let [ls,fp] = arg_tys'
          s2 <- unify (args !! 0) (VectorTy elty) ls
          s3 <- unify (args !! 1) (ArrowTy [elty, elty] IntTy) fp
          pure (s1 <> s2 <> s3, VectorTy elty, PrimAppE pr args_tc)

        InplaceVSortP elty -> do
          (s2, t, e) <- go (PrimAppE (VSortP elty) args)
          case e of
            PrimAppE (VSortP t2) args2 ->
              pure (s1 <> s2, t, PrimAppE (InplaceVSortP t2) args2)
            _ -> err $ text "InPlaceVSortP"

        VMergeP elty -> do
          len2
          let [ls1,ls2] = arg_tys'
          s2 <- unify (args !! 0) (VectorTy elty) ls1
          s3 <- unify (args !! 1) (VectorTy elty) ls2
          pure (s1 <> s2 <> s3, VectorTy elty, PrimAppE pr args_tc)

        PDictInsertP kty vty -> do
          len3
          let [key, val, dict] = arg_tys'
          s2 <- unify (args !! 0) key kty
          s3 <- unify (args !! 1) val vty
          s4 <- unify (args !! 2) dict (PDictTy kty vty)
          pure (s1 <> s2 <> s3 <> s4, PDictTy kty vty, PrimAppE pr args_tc)

        PDictLookupP kty vty -> do
          len2
          let [key, dict] = arg_tys'
          s2 <- unify (args !! 0) key kty
          s3 <- unify (args !! 1) dict (PDictTy kty vty)
          pure (s1 <> s2 <> s3, vty, PrimAppE pr args_tc)

        PDictAllocP kty vty -> do
          len0
          pure (s1, PDictTy kty vty, PrimAppE pr args_tc)

        PDictHasKeyP kty vty -> do
          len2
          let [key, dict] = arg_tys'
          s2 <- unify (args !! 0) key kty
          s3 <- unify (args !! 1) dict (PDictTy kty vty)
          pure (s1 <> s2 <> s3, BoolTy, PrimAppE pr args_tc)

        PDictForkP kty vty -> do
          len1
          let [dict] = arg_tys'
          s2 <- unify (args !! 0) dict (PDictTy kty vty)
          pure (s1 <> s2, ProdTy [PDictTy kty vty, PDictTy kty vty], PrimAppE pr args_tc)

        PDictJoinP kty vty -> do
          len2
          let [dict1, dict2] = arg_tys'
          s2 <- unify (args !! 0) dict1 (PDictTy kty vty)
          s3 <- unify (args !! 1) dict2 (PDictTy kty vty)
          pure (s1 <> s2 <> s3, PDictTy kty vty, PrimAppE pr args_tc)

        LLAllocP elty -> do
          len0
          pure (s1, ListTy elty, PrimAppE pr args_tc)

        LLIsEmptyP elty -> do
          len1
          let [ll] = arg_tys
          s2 <- unify (args !! 0) ll (ListTy elty)
          pure (s1 <> s2, BoolTy, PrimAppE pr args_tc)

        LLConsP elty -> do
          len2
          let [elt, ll] = arg_tys
          s2 <- unify (args !! 0) elt elty
          s3 <- unify (args !! 1) ll (ListTy elty)
          pure (s1 <> s2 <> s3, ListTy elty, PrimAppE pr args_tc)

        LLHeadP elty -> do
          len1
          let [ll] = arg_tys
          s2 <- unify (args !! 0) ll (ListTy elty)
          pure (s1 <> s2, elty, PrimAppE pr args_tc)

        LLTailP elty -> do
          len1
          let [ll] = arg_tys
          s2 <- unify (args !! 0) ll (ListTy elty)
          pure (s1 <> s2, ListTy elty, PrimAppE pr args_tc)

        LLFreeP elty -> do
          len1
          let [i] = arg_tys'
          s2 <- unify (args !! 0) (ListTy elty) i
          pure (s1 <> s2, ProdTy [], PrimAppE pr args_tc)

        LLFree2P elty -> do
          len1
          let [i] = arg_tys'
          s2 <- unify (args !! 0) (ListTy elty) i
          pure (s1 <> s2, ProdTy [], PrimAppE pr args_tc)

        LLCopyP elty -> do
          len1
          let [i] = arg_tys'
          s2 <- unify (args !! 0) (ListTy elty) i
          pure (s1 <> s2, ListTy elty, PrimAppE pr args_tc)

        GetNumProcessors -> do
          len0
          pure (s1, IntTy, PrimAppE pr args_tc)

        ErrorP _str ty -> do
          len0
          pure (s1, ty, PrimAppE pr args_tc)

        SizeParam -> do
          len0
          pure (s1, IntTy, PrimAppE pr args_tc)

        IsBig -> do
          len2
          let [ity, _ety] = arg_tys'
          -- s1 <- unify (args !! 0) (PackedTy)
          s2 <- unify (args !! 0) IntTy ity
          pure (s1 <> s2, BoolTy, PrimAppE pr args_tc)

        ReadPackedFile _fp _tycon _reg ty -> do
          len0
          pure (s1, ty, PrimAppE pr args_tc)

        ReadArrayFile _ ty -> do
          len0
          pure (s1, VectorTy ty, PrimAppE pr args_tc)

        WritePackedFile fp ty -> do
             len1
             let [packed_ty] = arg_tys'
             s2 <- unify (args !! 0) ty packed_ty
             pure (s1 <> s2, ProdTy [], PrimAppE (WritePackedFile fp (zonkTy s2 ty)) args_tc)

        Write3dPpmFile{} -> err $ text "Write3dPpmFile"
        RequestSizeOf-> err $ text "Unexpected RequestSizeOf in L0: " <+> exp_doc
        RequestEndOf -> err $ text "Unexpected RequestEndOf in L0: " <+> exp_doc


    LetE (v, [], gvn_rhs_ty, rhs) bod -> do
      (s1, drvd_rhs_ty, rhs_tc) <- go rhs
      s2 <- unify rhs gvn_rhs_ty drvd_rhs_ty
      let s3         = s1 <> s2
          venv'      = zonkTyEnv s3 venv
          -- Added b/c Nothing :: Just Int
          drvd_rhs_ty' = zonkTy s3 drvd_rhs_ty
      (s4, rhs_ty_gen) <- generalize venv' s3 bound_tyvars drvd_rhs_ty'
      let venv''     = M.insert v rhs_ty_gen venv'
      (s5, bod_ty, bod_tc) <- tcExp ddefs s4 venv'' fenv bound_tyvars is_main bod
      pure (s5, bod_ty,
            LetE (v, [], zonkTy s4 gvn_rhs_ty, zonkExp s4 rhs_tc) (zonkExp s5 bod_tc))

    LetE (_, (_:_), _, _) _ -> err $ text "Unexpected LetE: " <+> exp_doc

    IfE a b c -> do
      (s1, t1, a_tc) <- go a
      (s2, t2, b_tc) <- tcExp ddefs s1 venv fenv bound_tyvars is_main b
      (s3, t3, c_tc) <- tcExp ddefs s2 venv fenv bound_tyvars is_main c
      s4 <- unify a t1 BoolTy
      s5 <- unify ex t2 t3
      let s6 = s3 <> s4 <> s5
      pure (s6, zonkTy s6 t2,
            IfE (zonkExp s6 a_tc) (zonkExp s6 b_tc) (zonkExp s6 c_tc))

    MkProdE es -> do
      (s1, es_tys, es_tc) <- tcExps ddefs sbst venv fenv bound_tyvars (zip (repeat is_main) es)
      pure (s1, ProdTy es_tys, MkProdE es_tc)

    ProjE i a -> do
     (s1, a_ty, a_tc) <- go a
     case zonkTy s1 a_ty of
       ProdTy tys -> pure (s1, tys !! i, ProjE i a_tc)
       a_ty' -> err $ "tcExp: Coulnd't match expected type: ProdTy [...]"
                      <+> "with actual type: " <+> doc a_ty'
                      $$ exp_doc

    CaseE scrt brs -> do
      (s1, scrt_ty, scrt_tc) <- go scrt
      let scrt_ty' = zonkTy s1 scrt_ty
      case scrt_ty' of
        (PackedTy tycon drvd_tyargs) -> do
          let ddf = lookupDDef ddefs tycon
          ddf' <- substTyVarDDef ddf drvd_tyargs
          brs' <- L.nubBy ((==) `on` fst3) . concat <$> traverse (\x@(a,_,c) -> 
              if a == "_default" 
                then traverse (\(a', args) -> do
                        args' <- traverse (bitraverse (\_ -> gensym "wildcard") pure) args
                        pure (a', args', c) 
                      ) (dataCons ddf)
                else pure [x]
            ) brs :: TcM [(DataCon, [(Var, Ty0)], Exp0)]
          let tycons_brs = map (getTyOfDataCon ddefs . fst3) brs'
          case L.nub tycons_brs of
            [one] -> if one == tycon
                     then do
                       (s2,t2,brs_tc) <- tcCases ddefs s1 venv fenv bound_tyvars ddf' brs' is_main ex
                       pure (s2, t2, CaseE scrt_tc brs_tc)
                     else err $ text "Couldn't match" <+> doc one
                                <+> "with:" <+> doc scrt_ty'
                                $$ exp_doc
            oth -> err $ text "Case clause constructors have mismatched types:"
                          <+> doc oth
                          $$ exp_doc
        _ -> err $ text "Couldn't match" <+> doc scrt_ty'
                     <+> "with a Packed type."
                     $$ exp_doc

    DataConE _tyapps dcon args -> do
      (metas, arg_tys_inst, ret_ty_inst) <- instDataConTy ddefs dcon
      (s1, arg_tys, args_tc) <- tcExps ddefs sbst venv fenv bound_tyvars (zip (repeat is_main) args)
      s2 <- unifyl ex arg_tys_inst arg_tys
      let s3 = s1 <> s2
          tyapps = ProdTy (map (zonkTy s3) metas)
      pure (s3, zonkTy s3 ret_ty_inst,
            DataConE tyapps dcon (map (zonkExp s3) args_tc))

    Ext (LambdaE args bod) -> do
      (freshs, venv', s3) <- foldlM
                               (\(fs,env,s) (v,ty) -> do
                                 fresh <- newMetaTy
                                 let env' = M.insert v (ForAll [] fresh) env
                                 s1 <- unify (VarE v) ty fresh
                                 pure (fs ++ [fresh], env', s1 <> s))
                               ([],venv, sbst)
                               args
      (s4, bod_ty, bod_tc) <- tcExp ddefs s3 venv' fenv bound_tyvars is_main bod
      return (s4, zonkTy s4 (ArrowTy freshs bod_ty),
              Ext (LambdaE (map (\(v,ty) -> (v, zonkTy s4 ty)) args) (zonkExp s4 bod_tc)))

    Ext (PolyAppE op arg) -> do
      (s1, t1, op_tc) <- go op
      (s2, t2, arg_tc) <- tcExp ddefs s1 venv fenv bound_tyvars is_main arg
      fresh_out <- newMetaTy
      let fresh_arrow = ArrowTy [t2] fresh_out
      s3 <- unify op t1 fresh_arrow
      let s4 = s2 <> s3
      pure (s4, zonkTy s4 fresh_out, 
        Ext (PolyAppE (zonkExp s4 op_tc) (zonkExp s4 arg_tc)))

    Ext (FunRefE tyapps f) -> do
      (_metas, ty) <-
        case (M.lookup f venv, M.lookup f fenv) of
          (Nothing, Nothing) -> err $ text "Unbound function " <> doc f
          (Just t, _) -> instantiate t
          (_, Just t) -> instantiate t
      pure (sbst, ty, Ext $ FunRefE tyapps f)

    Ext (BenchE fn tyapps args b) ->
      if is_main
      then do
        (s1, ty, e') <- go (AppE fn tyapps args)
        case e' of
          AppE fn' tyapps' args' ->
            pure (s1, zonkTy s1 ty, Ext (BenchE fn' tyapps' args' b))
          _ -> err $ text "BenchE"
      else err $ text "'bench' can only be used as a tail of the main expression." <+> exp_doc

    Ext (ParE0 es)  -> do
      (s1, es_tys, es_tc) <- tcExps ddefs sbst venv fenv bound_tyvars (zip (repeat is_main) es)
      pure (s1, ProdTy es_tys, Ext $ ParE0 es_tc)

    Ext (PrintPacked ty arg) -> do
            (s1, ty', arg') <- go arg
            s2 <- unify arg ty ty'
            pure (s1 <> s2, ProdTy [], Ext (PrintPacked ty arg'))

    Ext (CopyPacked ty arg) -> do
            (s1, ty', arg') <- go arg
            s2 <- unify arg ty ty'
            pure (s1 <> s2, ty', Ext (CopyPacked ty arg'))

    Ext (TravPacked ty arg) -> do
            (s1, ty', arg') <- go arg
            s2 <- unify arg ty ty'
            pure (s1 <> s2, ProdTy [], Ext (TravPacked ty arg'))

    Ext (L0.L _ e) -> go e

    Ext (LinearExt{}) -> err $ text "a linear types extension wasn't desugared: " <+> exp_doc

    TimeIt a ty b -> do
      (s1, ty', a') <- go a
      s2 <- unify ex ty ty'
      let s3 = s1 <> s2
      pure (s3, zonkTy s3 ty', TimeIt (zonkExp s3 a') (zonkTy s3 ty') b)

    WithArenaE v e1 -> do
      let venv' = M.insert v (ForAll [] ArenaTy) venv
      (s1, ty', e1') <- tcExp ddefs sbst venv' fenv bound_tyvars is_main e1
      pure (s1, ty', WithArenaE v e1')

    SpawnE fn tyapps args -> do
      (s1, ty, e') <- tcExp ddefs sbst venv fenv bound_tyvars is_main (AppE fn tyapps args)
      case e' of
        AppE fn' tyapps' args' -> pure (s1, ty, SpawnE fn' tyapps' args')
        _ -> err $ text "SpawnE: not a saturated function"

    SyncE   -> pure (sbst, ProdTy [], SyncE)

    MapE{}  -> err $ text "TODO" <+> exp_doc
    FoldE{} -> err $ text "TODO" <+> exp_doc
  where
    go = tcExp ddefs sbst venv fenv bound_tyvars is_main
    exp_doc = "In the expression: " <+> doc ex


tcCases :: DDefs0 -> Subst -> Gamma -> Gamma -> [TyVar]
        -> DDef0 -> [(DataCon, [(Var, Ty0)], Exp0)] -> Bool -> Exp0
        -> TcM (Subst, Ty0, [(DataCon, [(Var, Ty0)], Exp0)])
tcCases ddefs sbst venv fenv bound_tyvars ddf brs is_main ex = do
  (s1,tys,exps) <-
    foldlM
      (\(s,acc,ex_acc) (con,vtys,rhs) -> do
        let vars = map fst vtys
            tys  = lookupDataCon' ddf con
            tys_gen = map (ForAll (tyArgs ddf L.\\ bound_tyvars)) tys
            venv' = venv <> (M.fromList $ zip vars tys_gen)
            vtys' = zip vars tys
        (s', rhs_ty, rhs_tc) <- tcExp ddefs s venv' fenv bound_tyvars is_main rhs
        let rhs_ty' = zonkTy s' rhs_ty
            rhs_tc' = zonkExp s' rhs_tc
        pure (s', acc ++ [rhs_ty'], ex_acc ++ [(con,vtys',rhs_tc')]))
      (sbst, [], [])
      brs
  let (as,bs) = unzip (pairs tys)
  s2 <- unifyl ex as bs
  let s3 = s1 <> s2
      tys' = map (zonkTy s3) tys
  mapM_ (\(a,b) -> ensureEqualTy ex a b) (pairs tys')
  pure (s3, Sf.headErr tys',exps)
  where
    -- pairs [1,2,3,4,5] = [(1,2), (2,3) (4,5)]
    pairs :: [a] -> [(a,a)]
    pairs []  = []
    pairs [_] = []
    pairs (x:y:xs) = (x,y) : pairs (y:xs)

-- | Instantiate the topmost for-alls of the argument type with meta
-- type variables.
instantiate :: TyScheme -> TcM ([Ty0], Ty0)
instantiate (ForAll tvs ty) = do
  tvs' <- mapM (const newMetaTy) tvs
  let ty' = substTyVar (M.fromList $ zip tvs tvs') ty
  pure (tvs', ty')

generalize :: Gamma -> Subst -> [TyVar] -> Ty0 -> TcM (Subst, TyScheme)
generalize env s bound_tyvars ty = do
  new_bndrs <- mapM
                 (\(Meta i) -> do
                       v' <- varAppend <$> genLetter <*> pure (toVar $ "_" ++ show i)
                       pure $ BoundTv v')
                 meta_tvs
  let s2  = Subst $ M.fromList (zip meta_tvs (map TyVar new_bndrs))
      ty' = zonkTy (s <> s2) ty

      -- Generalize over BoundTv's too.
      free_tvs = (tyVarsInTy ty) L.\\ bound_tyvars
  pure (s <> s2, ForAll (new_bndrs ++ free_tvs) ty')
  where
    env_tvs = metaTvsInTySchemes (M.elems env)
    res_tvs = metaTvsInTy ty

    meta_tvs :: [MetaTv]
    meta_tvs = res_tvs L.\\ env_tvs

--
instDataConTy :: DDefs0 -> DataCon -> TcM ([Ty0], [Ty0], Ty0)
instDataConTy ddefs dcon = do
  let tycon = getTyOfDataCon ddefs dcon
      ddf   = lookupDDef ddefs tycon
      arg_tys = lookupDataCon ddefs dcon
      tyvars  = tyVarsInTys arg_tys
  -- Given a datatype;
  --
  --     data Either a b = Left a | Right b
  --
  -- and some constructor, build a substituion;
  --
  --     Left  [c] => [ a -> c    , b -> fresh ]
  --     Right [c] => [ a -> fresh, b -> c     ]
  --
  tyArgs' <- mapM (\tyarg ->
                if tyarg `elem` tyvars
                then pure (TyVar tyarg)
                else newMetaTy)
             (tyArgs ddf)
  (env1, tyArgs'') <- tyVarToMetaTyl tyArgs'
  (_env2, arg_tys_inst) <- tyVarToMetaTyl (map (substTyVar env1) arg_tys)
  let ret_ty_inst = PackedTy tycon tyArgs''
  pure (tyArgs'', arg_tys_inst, ret_ty_inst)


--------------------------------------------------------------------------------
-- Type environment
--------------------------------------------------------------------------------

-- We can't directly use Env2 because of the way it's tied together with
-- PreExp and the Expression class. We want to annotate L0 expressions
-- with 'Ty0' but Gamma should store 'TyScheme's.
type Gamma = TyEnv Var TyScheme

instance FreeVars a => FreeVars (TyEnv Var a) where
  gFreeVars env = foldr (S.union . gFreeVars) S.empty (M.elems env)

--------------------------------------------------------------------------------
-- Substitution
--------------------------------------------------------------------------------

newtype Subst = Subst (M.Map MetaTv Ty0)
  deriving (Ord, Eq, Read, Show, Generic, Out)

instance Semigroup Subst where
  -- (Subst s1) <> (Subst s2) =
  --   let mp = M.map (zonkTy (Subst s1)) s2 `M.union` s1 in Subst mp
  (<>) :: Subst -> Subst -> Subst
  (Subst s1) <> (Subst s2) =
    let s2' = M.map (zonkTy (Subst s1)) s2
        mp =  M.unionWith combine s2' s1
    in Subst mp

-- | Combine substitutions. In case of substitutions with intersecting keys,
-- we will take the narrower type of the two. e.g. combine [($1, $2)] [($1, IntTy)]
-- should be [($1, IntTy)]. Map.union does a left biased union so it will result in [($1, $2)]
combine :: Ty0 -> Ty0 -> Ty0
combine v1 v2 | v1 == v2 = v1
              | otherwise = case (v1, v2) of
                (MetaTv _, _) -> v2
                (_, MetaTv _) -> v1
                (ArrowTy xs y, ArrowTy xs' y') -> ArrowTy (zipWith combine xs xs') (combine y y')
                (VectorTy v1', VectorTy v2') -> VectorTy $ combine v1' v2'
                (ProdTy v1s, ProdTy v2s) -> ProdTy (zipWith combine v1s v2s)
                (PackedTy a1 v1s, PackedTy a2 v2s) ->
                  if a1 == a2 then PackedTy a1 (zipWith combine v1s v2s)
                  else error $ "PackedTy doesn't match "++ sdoc v1 ++ " with " ++ sdoc v2
                _ -> error $ "Failed to combine = " ++ sdoc v1 ++ " with " ++ sdoc v2


emptySubst :: Subst
emptySubst = Subst (M.empty)

-- | Perform substitutions in the type.
zonkTy :: Subst -> Ty0 -> Ty0
zonkTy s@(Subst mp) ty =
  case ty of
    IntTy   -> ty
    CharTy  -> ty
    FloatTy -> ty
    SymTy0  -> ty
    BoolTy  -> ty
    TyVar{} -> ty
    MetaTv v -> case M.lookup v mp of
                  Nothing -> MetaTv v
                  Just t@(MetaTv w) -> if v == w then MetaTv v else zonkTy s t
                  Just t -> zonkTy s t
    ProdTy tys  -> ProdTy (map go tys)
    SymDictTy v t -> SymDictTy v (go t)
    PDictTy k v -> PDictTy (go k) (go v)
    ArrowTy tys b  -> ArrowTy (map go tys) (go b)
    PackedTy t tys -> PackedTy t (map go tys)
    VectorTy t -> VectorTy (go t)
    ListTy t -> ListTy (go t)
    ArenaTy  -> ty
    SymSetTy -> ty
    SymHashTy -> ty
    IntHashTy -> ty
  where
    go = zonkTy s

zonkTyScheme :: Subst -> TyScheme -> TyScheme
zonkTyScheme s (ForAll tvs ty) = ForAll tvs (zonkTy s ty)

zonkTyEnv :: Subst -> Gamma -> Gamma
zonkTyEnv s env = M.map (zonkTyScheme s) env

-- Apply a substitution to an expression i.e substitue all types in it.
zonkExp :: Subst -> Exp0 -> Exp0
zonkExp s ex =
  case ex of
    VarE{}    -> ex
    LitE{}    -> ex
    CharE{}   -> ex
    FloatE{}  -> ex
    LitSymE{} -> ex
    AppE f tyapps args -> let tyapps1 = map (zonkTy s) tyapps
                          in AppE f tyapps1 (map go args)
    PrimAppE pr args  ->
      let pr' = case pr of
                  VAllocP  ty -> VAllocP  (zonkTy s ty)
                  VFreeP  ty  -> VFreeP  (zonkTy s ty)
                  VFree2P  ty -> VFree2P  (zonkTy s ty)
                  VLengthP ty -> VLengthP (zonkTy s ty)
                  VNthP    ty -> VNthP    (zonkTy s ty)
                  VSliceP  ty -> VSliceP  (zonkTy s ty)
                  VConcatP ty -> VConcatP (zonkTy s ty)
                  InplaceVUpdateP ty -> InplaceVUpdateP (zonkTy s ty)
                  VSortP   ty -> VSortP   (zonkTy s ty)
                  VMergeP ty -> VMergeP (zonkTy s ty)
                  PDictAllocP k v -> PDictAllocP (zonkTy s k) (zonkTy s v)
                  PDictInsertP k v -> PDictInsertP (zonkTy s k) (zonkTy s v)
                  PDictLookupP k v -> PDictLookupP (zonkTy s k) (zonkTy s v)
                  PDictHasKeyP k v -> PDictHasKeyP (zonkTy s k) (zonkTy s v)
                  PDictForkP k v -> PDictForkP (zonkTy s k) (zonkTy s v)
                  PDictJoinP k v -> PDictJoinP (zonkTy s k) (zonkTy s v)
                  LLAllocP ty -> LLAllocP (zonkTy s ty)
                  LLIsEmptyP ty -> LLIsEmptyP (zonkTy s ty)
                  LLConsP ty -> LLConsP (zonkTy s ty)
                  LLHeadP ty -> LLHeadP (zonkTy s ty)
                  LLTailP ty -> LLTailP (zonkTy s ty)
                  LLFreeP ty -> LLFreeP (zonkTy s ty)
                  LLFree2P ty -> LLFree2P (zonkTy s ty)
                  LLCopyP ty -> LLCopyP (zonkTy s ty)
                  InplaceVSortP ty -> InplaceVSortP (zonkTy s ty)
                  ReadArrayFile fp ty -> ReadArrayFile fp (zonkTy s ty)
                  _ -> pr
      in PrimAppE pr' (map go args)
    -- Let doesn't store any tyapps.
    LetE (v,tyapps,ty,rhs) bod -> LetE (v, tyapps, zonkTy s ty, go rhs) (go bod)
    IfE a b c  -> IfE (go a) (go b) (go c)
    MkProdE ls -> MkProdE (map go ls)
    ProjE i e  -> ProjE i (go e)
    CaseE scrt brs ->
      CaseE (go scrt) (map
                        (\(dcon,vtys,rhs) -> let (vars,tys) = unzip vtys
                                                 vtys' = zip vars $ map (zonkTy s) tys
                                             in (dcon, vtys', go rhs))
                        brs)
    DataConE (ProdTy tyapps) dcon args ->
      DataConE (ProdTy (map (zonkTy s) tyapps)) dcon (map go args)
    DataConE{} -> error $ "zonkExp: Expected (ProdTy tyapps), got: " ++ sdoc ex
    TimeIt e ty b -> TimeIt (go e) (zonkTy s ty) b
    WithArenaE v e -> WithArenaE v (go e)
    Ext (LambdaE args bod) -> Ext (LambdaE (map (\(v,ty) -> (v, zonkTy s ty)) args) (go bod))
    Ext (PolyAppE rator rand) -> Ext (PolyAppE (go rator) (go rand))
    Ext (FunRefE tyapps f)    -> let tyapps1 = map (zonkTy s) tyapps
                                 in Ext (FunRefE tyapps1 f)
    Ext (BenchE fn tyapps args b) -> let tyapps1 = map (zonkTy s) tyapps
                                     in Ext (BenchE fn tyapps1 (map go args) b)
    Ext (ParE0 ls) -> Ext $ ParE0 (map go ls)
    Ext (L0.L p e)    -> Ext $ L0.L p (go e)
    Ext (PrintPacked ty arg) -> Ext $ PrintPacked (zonkTy s ty) (go arg)
    Ext (CopyPacked ty arg) -> Ext $ CopyPacked (zonkTy s ty) (go arg)
    Ext (TravPacked ty arg) -> Ext $ TravPacked (zonkTy s ty) (go arg)
    Ext (LinearExt{}) -> error $ "zonkExp: a linear types extension wasn't desugared: " ++ sdoc ex
    SpawnE fn tyapps args -> let tyapps1 = map (zonkTy s) tyapps
                             in SpawnE fn tyapps1 (map go args)
    SyncE    -> SyncE
    MapE{}   -> error $ "zonkExp: TODO, " ++ sdoc ex
    FoldE{}  -> error $ "zonkExp: TODO, " ++ sdoc ex
  where
    go = zonkExp s

-- Substitute tyvars with types in a ddef.
substTyVarDDef :: DDef0 -> [Ty0] -> TcM DDef0
substTyVarDDef d@DDef{tyArgs,dataCons} tys =
  if length tyArgs /= length tys
  then err $ text "substTyVarDDef: tyArgs don't match the tyapps, in "
               <+> doc tyArgs <+> text ", " <+> doc tys
  else do
    let mp = M.fromList (zip tyArgs tys)
        dcons' = map
                   (\(dcon,btys) ->
                      let (boxity, tys1) = unzip btys
                          tys1' = map (substTyVar mp) tys1
                      in (dcon, zip boxity tys1'))
                   dataCons
        free_tyvars =  concatMap tyVarsInTy (M.elems mp)
    pure d { tyArgs   = free_tyvars
           , dataCons = dcons' }

-- Substitue all tyvars in an expression.
substTyVarExp :: M.Map TyVar Ty0 -> Exp0 -> Exp0
substTyVarExp s ex =
  case ex of
    VarE{}    -> ex
    LitE{}    -> ex
    CharE{}   -> ex
    FloatE{}  -> ex
    LitSymE{} -> ex
    AppE f tyapps arg -> let tyapps1 = map (substTyVar s) tyapps
                         in AppE f tyapps1 (map go arg)
    PrimAppE pr args  -> PrimAppE (substTyVarPrim s pr) (map go args)
    -- Let doesn't store any tyapps.
    LetE (v,tyapps,ty,rhs) bod -> LetE (v, tyapps, substTyVar s ty, go rhs) (go bod)
    IfE a b c  -> IfE (go a) (go b) (go c)
    MkProdE ls -> MkProdE (map go ls)
    ProjE i e  -> ProjE i (go e)
    CaseE scrt brs ->
      CaseE (go scrt) (map
                        (\(dcon,vtys,rhs) -> let (vars,tys) = unzip vtys
                                                 vtys' = zip vars $ map (substTyVar s) tys
                                             in (dcon, vtys', go rhs))
                        brs)
    DataConE (ProdTy tyapps) dcon args ->
      DataConE (ProdTy (map (substTyVar s) tyapps)) dcon (map go args)
    DataConE{} -> error $ "substTyVarExp: Expected (ProdTy tyapps), got: " ++ sdoc ex
    TimeIt e ty b -> TimeIt (go e) (substTyVar s ty) b
    Ext (LambdaE args bod) -> Ext (LambdaE (map (\(v,ty) -> (v, substTyVar s ty)) args) (go bod))
    Ext (PolyAppE rator rand) -> Ext (PolyAppE (go rator) (go rand))
    Ext (FunRefE tyapps f) -> let tyapps1 = map (substTyVar s) tyapps
                              in Ext $ FunRefE tyapps1 f
    Ext (BenchE fn tyapps args b) -> let tyapps1 = map (substTyVar s) tyapps
                                     in Ext (BenchE fn tyapps1 (map go args) b)
    Ext (ParE0 ls) -> Ext $ ParE0 (map go ls)
    Ext (PrintPacked ty arg) -> Ext $ PrintPacked (substTyVar s ty) (go arg)
    Ext (CopyPacked ty arg) -> Ext $ CopyPacked (substTyVar s ty) (go arg)
    Ext (TravPacked ty arg) -> Ext $ TravPacked (substTyVar s ty) (go arg)
    Ext (L0.L p e)    -> Ext $ L0.L p (go e)
    Ext (LinearExt{}) -> error $ "substTyVarExp: a linear types extension wasn't desugared: " ++ sdoc ex
    WithArenaE{} -> error "substTyVarExp: WithArenaE not handled."
    SpawnE f tyapps arg -> let tyapps1 = map (substTyVar s) tyapps
                           in SpawnE f tyapps1 (map go arg)
    SyncE    -> SyncE
    MapE{}   -> error $ "substTyVarExp: TODO, " ++ sdoc ex
    FoldE{}  -> error $ "substTyVarExp: TODO, " ++ sdoc ex
  where
    go = substTyVarExp s

substTyVarPrim :: M.Map TyVar Ty0 -> Prim Ty0 -> Prim Ty0
substTyVarPrim mp pr =
    case pr of
        VAllocP elty -> VAllocP (substTyVar mp elty)
        VFreeP  elty  -> VFreeP  (substTyVar mp elty)
        VFree2P  elty -> VFree2P  (substTyVar mp elty)
        VLengthP elty -> VLengthP (substTyVar mp elty)
        VNthP elty -> VNthP (substTyVar mp elty)
        VSliceP elty -> VSliceP (substTyVar mp elty)
        InplaceVUpdateP elty -> InplaceVUpdateP (substTyVar mp elty)
        VConcatP elty -> VConcatP (substTyVar mp elty)
        VSortP elty -> VSortP (substTyVar mp elty)
        InplaceVSortP elty -> InplaceVSortP (substTyVar mp elty)
        VMergeP elty -> VMergeP (substTyVar mp elty)
        PDictInsertP kty vty -> PDictInsertP (substTyVar mp kty) (substTyVar mp vty)
        PDictLookupP kty vty -> PDictLookupP (substTyVar mp kty) (substTyVar mp vty)
        PDictAllocP kty vty -> PDictAllocP (substTyVar mp kty) (substTyVar mp vty)
        PDictHasKeyP kty vty -> PDictHasKeyP (substTyVar mp kty) (substTyVar mp vty)
        PDictForkP kty vty -> PDictForkP (substTyVar mp kty) (substTyVar mp vty)
        PDictJoinP kty vty -> PDictJoinP (substTyVar mp kty) (substTyVar mp vty)
        LLAllocP elty -> LLAllocP (substTyVar mp elty)
        LLIsEmptyP elty -> LLIsEmptyP (substTyVar mp elty)
        LLConsP elty -> LLConsP (substTyVar mp elty)
        LLHeadP elty -> LLHeadP (substTyVar mp elty)
        LLTailP elty -> LLTailP (substTyVar mp elty)
        LLFreeP elty -> LLFreeP (substTyVar mp elty)
        LLFree2P elty -> LLFree2P (substTyVar mp elty)
        LLCopyP elty -> LLCopyP (substTyVar mp elty)
        _ -> pr


tyVarToMetaTyl :: [Ty0] -> TcM (M.Map TyVar Ty0, [Ty0])
tyVarToMetaTyl tys =
  foldlM
    (\(env', acc) ty -> do
            (env'', ty') <- tyVarToMetaTy ty
            pure (env' <> env'', acc ++ [ty']))
    (M.empty, [])
    tys

-- | Replace the specified quantified type variables by
-- given meta type variables.
tyVarToMetaTy :: Ty0 -> TcM (M.Map TyVar Ty0, Ty0)
tyVarToMetaTy = go M.empty
  where
    go :: M.Map TyVar Ty0 -> Ty0 -> TcM (M.Map TyVar Ty0, Ty0)
    go env ty =
     case ty of
       IntTy    -> pure (env, ty)
       CharTy   -> pure (env, ty)
       FloatTy  -> pure (env, ty)
       SymTy0   -> pure (env, ty)
       BoolTy   -> pure (env, ty)
       TyVar v  -> do mty <- newMetaTy
                      pure (M.insert v mty env, mty)
       MetaTv{} -> pure (env, ty)
       ProdTy tys -> do (env', tys') <- gol env tys
                        pure (env', ProdTy tys')
       SymDictTy v t -> do (env', t') <- go env t
                           pure (env', SymDictTy v t')
       PDictTy k v -> do (env', k') <- go env k
                         (env'', v') <- go env' v
                         pure (env'', PDictTy k' v')
       ArrowTy as b -> do (env', as') <- gol env as
                          (env'', b') <- go env' b
                          pure (env'', ArrowTy as' b')
       PackedTy t tys -> do (env', tys') <- gol env tys
                            pure (env', PackedTy t tys')
       VectorTy el_t -> do (env', el_t') <- go env el_t
                           pure (env', VectorTy el_t')
       ListTy el_t -> do (env', el_t') <- go env el_t
                         pure (env', ListTy el_t')
       ArenaTy  -> pure (env, ty)
       SymSetTy -> pure (env, ty)
       SymHashTy-> pure (env, ty)
       IntHashTy-> pure (env, ty)

    gol :: M.Map TyVar Ty0 -> [Ty0] -> TcM (M.Map TyVar Ty0, [Ty0])
    gol env tys = foldlM
                     (\(env', acc) ty -> do
                         (env'', ty') <- go env' ty
                         pure (env'', acc ++ [ty']))
                     (env, [])
                     tys

--------------------------------------------------------------------------------
-- Unification
--------------------------------------------------------------------------------

unify :: Exp0 -> Ty0 -> Ty0 -> TcM Subst
unify ex ty1 ty2
  | ty1 == ty2 = --dbgTraceIt (sdoc ty1 ++ "/" ++ sdoc ty2) $
                 pure emptySubst
  | otherwise  = -- dbgTraceIt (sdoc ty1 ++ "/" ++ sdoc ty2) $
      case (ty1,ty2) of
        (IntTy, IntTy)     -> pure emptySubst
        (FloatTy,FloatTy)  -> pure emptySubst
        (BoolTy, BoolTy)   -> pure emptySubst
        (TyVar _, TyVar _) -> fail_
        -- -- CHECKME
        -- (MetaTv a, MetaTv b) -> unifyVar ex a (MetaTv b)
        (MetaTv a, _) -> unifyVar ex a ty2
        (_, MetaTv b) -> unifyVar ex b ty1
        (ProdTy as, ProdTy bs) -> unifyl ex as bs
        (ArrowTy as b, ArrowTy cs d) -> do
          s1 <- unifyl ex as cs
          s2 <- unify ex (zonkTy s1 b) (zonkTy s1 d)
          pure (s2 <> s1)
        (PackedTy tc1 tys1, PackedTy tc2 tys2) ->
          if tc1 == tc2
          then unifyl ex tys1 tys2
          else fail_
        (SymDictTy _ t1, SymDictTy _ t2) -> unify ex t1 t2
        (VectorTy t1, VectorTy t2) -> unify ex t1 t2
        (ListTy t1, ListTy t2) -> unify ex t1 t2
        (PDictTy k1 v1, PDictTy k2 v2) -> do
          s1 <- unify ex k1 k2
          s2 <- unify ex v1 v2
          pure (s1 <> s2)
        _ -> dbgTrace 1 ("unify: Catch-all _; failed to unify " ++ sdoc ty1 ++ " with " ++ sdoc ty2) fail_
  where fail_ = err $  text "Couldn't match type" <+> quotes (doc ty2)
                    <+> text "with" <+> quotes (doc ty1)
                    $$ text "Expected type:" <+> doc ty1
                    $$ text "Actual type:" <+> doc ty2
                    $$ text "In the expression: "
                    $$ nest 2 (doc ex)


unifyl :: Exp0 -> [Ty0] -> [Ty0] -> TcM Subst
unifyl _ [] [] = pure emptySubst
unifyl e (a:as) (b:bs) = do
    -- N.B. We must apply s1 over the rest of the list before unifying it, i.e.
    --
    --     (<>) <$> unify e a b <*> unifyl e as bs
    --
    -- doesn't work!
    s1 <- unify e a b
    s2 <- unifyl e (map (zonkTy s1) as) (map (zonkTy s1) bs)
    pure (s1 <> s2)
unifyl e as bs = err $ text "Couldn't unify:" <+> doc as <+> text "and" <+> doc bs
                         $$ text "In the expression: "
                         $$ nest 2 (doc e)

unifyVar :: Exp0 -> MetaTv -> Ty0 -> TcM Subst
unifyVar ex a t
  | occursCheck a t = err $ text "Occurs check: cannot construct the inifinite type: "
                              $$ nest 2 (doc a <+> text " ~ " <+> doc t)
                              $$ text "In the expression: "
                              $$ nest 2 (doc ex)
  | otherwise       = pure $ Subst (M.singleton a t)


occursCheck :: MetaTv -> Ty0 -> Bool
occursCheck a t = a `elem` metaTvsInTy t

--------------------------------------------------------------------------------
-- Other helpers
--------------------------------------------------------------------------------

ensureEqualTy :: Exp0 -> Ty0 -> Ty0 -> TcM ()
ensureEqualTy ex ty1 ty2
  | ty1 == ty2 = pure ()
  | otherwise  = err $ text "Couldn't match expected type:" <+> doc ty1
                         $$ text "with actual type: " <+> doc ty2
                         $$ text "In the expression: "
                         $$ nest 2 (doc ex)
