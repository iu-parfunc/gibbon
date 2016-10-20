{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module Packed.FirstOrder.HaskellFrontend
  ( desugarModule
  , desugarExp
  , desugarTopType
  , desugarType
  ) where

--------------------------------------------------------------------------------

import Control.Monad (forM)
import Data.Either (partitionEithers)
import Data.Foldable (foldrM)
import qualified Data.Map as M
import Data.Maybe (catMaybes)

import Language.Haskell.Exts.Syntax as H
import Packed.FirstOrder.L1_Source as L1

--------------------------------------------------------------------------------

type Ds a = Either String a

err :: String -> Ds a
err = Left

--------------------------------------------------------------------------------

desugarModule :: H.Module -> Ds P1
desugarModule (H.Module _ _ _ _ _ _ decls) = do
    -- since top-level function types and their types can't be declared in
    -- single top-level declaration we first collect types and then collect
    -- definition
    fun_tys <- (M.fromList . catMaybes) <$> mapM collectTopFunTy decls

    (data_decls, fun_decls) <-
      (partitionEithers . catMaybes) <$> mapM (collectTopLevel fun_tys) decls

    let
      data_map = M.fromList (map (\def -> (tyName def, def))  data_decls)
      fun_map  = M.fromList (map (\def -> (funName def, def)) fun_decls)

      (main_fn, fun_map_no_main) =
        -- ugh, no alterF in this 'containers' version
        ( funBody <$> M.lookup "main" fun_map
        , M.delete "main" fun_map
        )

    return (P1 data_map fun_map_no_main main_fn)

collectTopFunTy :: H.Decl -> Ds (Maybe (Var, TopTy))
collectTopFunTy (TypeSig _ [n] ty) = Just <$> (name_to_str n,) <$> desugarTopType ty
collectTopFunTy ty@TypeSig{} = err ("Unsupported top-level type declaration: " ++ show ty)
collectTopFunTy FunBind{} = return Nothing
collectTopFunTy unsupported = err ("Unsupported top-level thing: " ++ show unsupported)

collectTopLevel :: M.Map Var TopTy -> H.Decl -> Ds (Maybe (Either (DDef T1) (FunDef T1 L1)))

collectTopLevel _ TypeSig{} = return Nothing

collectTopLevel fun_tys (FunBind [Match _ fname args Nothing (UnGuardedRhs rhs) Nothing]) = do
    let fname' = name_to_str fname
        fun_ty = M.findWithDefault (error ("Can't find function in type env: " ++ fname'))
                                   fname' fun_tys
    args'   <- mapM collectArg args
    arg_tys <- mapM (getArgTy fun_ty) [ 1 .. length args' ]
    rhs'    <- desugarExp rhs
    return (Just (Right (FunDef fname' (getRetTy fun_ty) (zip args' arg_tys) rhs')))
  where
    collectArg :: Pat -> Ds Var
    collectArg (PVar n) = return (name_to_str n)
    collectArg arg      = err ("Unsupported function arg: " ++ show arg)

    getArgTy :: TopTy -> Int -> Ds T1
    getArgTy (Arrow ts) n = return (ts !! n)
    getArgTy ty         _ = err ("getArgTy: " ++ show ty)

    getRetTy :: TopTy -> T1
    getRetTy (Arrow ts) = last ts
    getRetTy (T1 t)     = t

collectTopLevel _ unsupported = err ("Unsupported top-level thing: " ++ show unsupported)

--------------------------------------------------------------------------------

-- TODO: Primops

desugarExp :: Exp -> Ds L1
desugarExp e =
    case e of

      Var qname -> Varref <$> qname_to_str qname

      Con qname -> MkPacked <$> qname_to_str qname <*> pure []

      H.Lit l   -> L1.Lit <$> lit_to_int l

      H.App e1 e2 ->
        desugarExp e1 >>= \case
          Varref "fst" ->
            L1.Fst <$> desugarExp e2
          Varref "snd" ->
            L1.Snd <$> desugarExp e2
          Varref f ->
            L1.App f <$> desugarExp e2
          MkPacked c as -> do
            e2' <- desugarExp e2
            return (L1.MkPacked c (as ++ [e2']))
          f ->
            err ("Only variables allowed in operator position in function applications. (found: " ++ show f ++ ")")

      H.Tuple Unboxed _ ->
        err "Only boxed tuples are allowed."
      H.Tuple Boxed [e1, e2] ->
        MkProd <$> desugarExp e1 <*> desugarExp e2
      H.Tuple _ es ->
        err ("Tuples can only be pairs. (" ++ show es ++ ")")

      H.Let (BDecls decls) rhs -> do
        rhs' <- desugarExp rhs
        foldrM generateBind rhs' decls

      H.Case scrt alts -> do
        scrt' <- desugarExp scrt
        CasePacked scrt' . M.fromList <$> mapM desugarAlt alts

--------------------------------------------------------------------------------

generateBind :: H.Decl -> L1 -> Ds L1

generateBind (PatBind _ _ _ Just{}) _ =
    err "where clauses not allowed"

generateBind (PatBind _ _ GuardedRhss{} _) _ =
    err "Guarded right hand side not supported."

generateBind (PatBind _ (PVar v) (UnGuardedRhs rhs) Nothing) e = do
    rhs' <- desugarExp rhs
    return (Letrec (name_to_str v, undefined, rhs') e)

generateBind (PatBind _ not_var _ _) _ =
    err ("Only variable bindings are allowed in let. (found: " ++ show not_var ++ ")")

generateBind not_pat_bind _ =
    err ("Only variable bindings are allowed in let. (found: " ++ show not_pat_bind ++ ")")

--------------------------------------------------------------------------------

desugarAlt :: H.Alt -> Ds (Constr, ([Var], L1))

desugarAlt (H.Alt _ (PApp qname ps) (UnGuardedRhs rhs) Nothing) = do
    con_name <- qname_to_str qname
    ps' <- forM ps $ \case PVar v -> return (name_to_str v)
                           _      -> err "Non-variable pattern in case."
    rhs' <- desugarExp rhs
    return (con_name, (ps', rhs'))

desugarAlt (H.Alt _ _ GuardedRhss{} _) =
    err "Guarded RHS not supported in case."

desugarAlt (H.Alt _ _ _ Just{}) =
    err "Where clauses not allowed in case."

desugarAlt (H.Alt _ pat _ _) =
    err ("Unsupported pattern in case: " ++ show pat)

-------------------------------------------------------------------------------

-- | Top-level function definitions can have arrow in the types. Others can't.
data TopTy
  = Arrow [T1]
  | T1 T1
  deriving (Show)

desugarTopType :: H.Type -> Ds TopTy
desugarTopType (TyInfix ty1 (UnQual (Symbol "->")) ty2) = do
    ty1' <- desugarType ty1
    ty2' <- desugarTopType ty2
    return . Arrow $ case ty2' of
                       Arrow ts -> ty1' : ts
                       T1 t1    -> [ty1', t1]
desugarTopType ty =
    err ("Toplevel types should be arrows. Found: " ++ show ty)

desugarType :: H.Type -> Ds T1
desugarType (TyCon (UnQual (Ident "Int"))) = return TInt
desugarType (TyCon (UnQual (Ident con))) = return (Packed con [])
desugarType (TyTuple Boxed [ty1, ty2]) = Prod <$> desugarType ty1 <*> desugarType ty2
desugarType (TyApp (TyCon (UnQual (Ident "Dict"))) ty) = TDict <$> desugarType ty
desugarType ty@(TyApp ty1 ty2) =
    desugarType ty1 >>= \case
      Packed con args -> do
        ty2' <- desugarType ty2
        return (Packed con (args ++ [ty2']))
      _ -> err ("Unsupported type: " ++ show ty)
desugarType ty = err ("Unsupported type: " ++ show ty)

--------------------------------------------------------------------------------

qname_to_str :: QName -> Ds String
qname_to_str (Qual mname n) = return (mname_str mname ++ "." ++ name_to_str n)
qname_to_str (UnQual n)     = return (name_to_str n)
qname_to_str s@Special{}    = err ("Special identifiers not supported: " ++ show s)

mname_str :: ModuleName -> String
mname_str (ModuleName s) = s

name_to_str :: Name -> String
name_to_str (Ident s)  = s
name_to_str (Symbol s) = s

lit_to_int :: Literal -> Ds Int
lit_to_int (H.Int i) = return (fromIntegral i) -- lossy conversion here
lit_to_int l         = err ("Literal not supported: " ++ show l)
