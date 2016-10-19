{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module Packed.FirstOrder.HaskellFrontend where

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

    (data_decls, fun_decls) <- (partitionEithers . catMaybes) <$> mapM (collectTopLevel fun_tys) decls

    let
      data_map = M.fromList (map (\def -> (tyName def, def))  data_decls)
      fun_map  = M.fromList (map (\def -> (funname def, def)) fun_decls)

      (main_fn, fun_map_no_main) =
        -- ugh, no alterF in this 'containers' version
        ( funbody <$> M.lookup "main" fun_map
        , M.delete "main" fun_map
        )

    return (P1 data_map fun_map_no_main main_fn)

collectTopFunTy :: H.Decl -> Ds (Maybe (Var, T1))
collectTopFunTy (TypeSig _ [n] ty) = Just <$> (name_to_str n,) <$> desugarType ty
collectTopFunTy ty@TypeSig{} = err ("Unsupported top-level type declaration: " ++ show ty)
collectTopFunTy FunBind{} = return Nothing
collectTopFunTy unsupported = err ("Unsupported top-level thing: " ++ show unsupported)

collectTopLevel :: M.Map Var T1 -> H.Decl -> Ds (Maybe (Either (DDef T1) (FunDef T1 L1)))

collectTopLevel _ TypeSig{} = return Nothing

collectTopLevel fun_tys (FunBind [Match _ fname args Nothing (UnGuardedRhs rhs) Nothing]) = do
    let fname' = name_to_str fname
        fun_ty = M.findWithDefault (error ("Can't find function in type env: " ++ fname'))
                                   fname' fun_tys
    args'   <- mapM collectArg args
    arg_tys <- mapM (getArgTy fun_ty) [ 1 .. length args' ]
    rhs'    <- desugarExp rhs
    return (Just (Right (FunDef fname' fun_ty (zip args' arg_tys) rhs')))
  where
    collectArg :: Pat -> Ds Var
    collectArg (PVar n) = return (name_to_str n)
    collectArg arg      = err ("Unsupported function arg: " ++ show arg)

    getArgTy :: T1 -> Int -> Ds T1
    getArgTy ty          0 = return ty
    getArgTy (Arrow _ t) n = getArgTy t (n - 1)
    getArgTy ty          _ = err ("getArgTy: " ++ show ty)

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


{-
  TopLevel
  (TyEnv [("Tree", SumTy [("Leaf",[IntTy]),("Node",[VarTy "Tree", VarTy "Tree"])])])
  [(FunDecl "add1" ["t"]
     (CaseE "t" [
         ("Leaf", "x",
          (LetE [("v1",(IntE 1)),("v2",(ProjE "Leaf" "x" 0))]
            (PrimOpE PlusP ["v1","v2"]))),
         ("Node", "x",
          (LetE [("x1",(ProjE "Node" "x" 0)),("x2",(ProjE "Node" "x" 1))]
            (LetE [("y1",(AppE "add1" "x1")),("y2",(AppE "add1" "x2"))]
              (ConstrE "Node" ["y1","y2"]))))
         ]))
  ]
  (IntE 0)
  -}
