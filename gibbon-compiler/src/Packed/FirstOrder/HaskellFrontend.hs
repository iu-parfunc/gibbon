{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module Packed.FirstOrder.HaskellFrontend
  ( parseFile
  -- * Everething else could remain internal:
  , desugarModule
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

import Language.Haskell.Exts.Parser -- (parse)
import Language.Haskell.Exts.Syntax as H
import Packed.FirstOrder.L1_Source as L1
import Packed.FirstOrder.Common
    
--------------------------------------------------------------------------------

type Ds a = Either String a

err :: String -> Ds a
err = Left

--------------------------------------------------------------------------------

desugarModule :: H.Module -> Ds Prog
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

    return (Prog data_map fun_map_no_main main_fn)

collectTopFunTy :: H.Decl -> Ds (Maybe (Var, TopTy))
collectTopFunTy (TypeSig _ [n] ty) = Just <$> (name_to_str n,) <$> desugarTopType ty
collectTopFunTy ty@TypeSig{} = err ("Unsupported top-level type declaration: " ++ show ty)
collectTopFunTy FunBind{} = return Nothing
collectTopFunTy DataDecl{} = return Nothing
collectTopFunTy unsupported = err ("collectTopFunTy: Unsupported top-level thing: " ++ show unsupported)

collectTopLevel :: M.Map Var TopTy -> H.Decl -> Ds (Maybe (Either (DDef Ty) (FunDef Ty L1.Exp)))

collectTopLevel _ TypeSig{} = return Nothing

collectTopLevel fun_tys (FunBind [Match _ fname args Nothing (UnGuardedRhs rhs) Nothing]) = do
    let fname' = name_to_str fname
        fun_ty = M.findWithDefault (error ("Can't find function in type env: " ++ fname'))
                                   fname' fun_tys
    [arg']   <- mapM collectArg args
    -- Limiting to one argument for now:
    [arg_ty] <- mapM (getArgTy fun_ty) [ 1 .. length [arg'] ]
    rhs'    <- desugarExp rhs
    return (Just (Right (FunDef fname' (arg',arg_ty) (getRetTy fun_ty) rhs')))
  where
    collectArg :: Pat -> Ds Var
    collectArg (PVar n) = return (name_to_str n)
    collectArg arg      = err ("Unsupported function arg: " ++ show arg)

    getArgTy :: TopTy -> Int -> Ds Ty
    getArgTy (Arrow ts) n = return (ts !! n)
    getArgTy ty         _ = err ("getArgTy: " ++ show ty)

    getRetTy :: TopTy -> Ty
    getRetTy (Arrow ts) = last ts
    getRetTy (T1 t)     = t

collectTopLevel _ (DataDecl _ DataType [] ty_name [] cons []) = do
    let ty_name' = name_to_str ty_name
    constrs <- mapM collectConstr cons
    return (Just (Left (DDef ty_name' constrs)))
  where
    collectConstr (QualConDecl _ [] [] (ConDecl con_name arg_tys)) =
      ( name_to_str con_name, ) <$> mapM desugarType arg_tys
    collectConstr unsupported =
      err ("Unsupported data constructor: " ++ show unsupported)

collectTopLevel _ unsupported = err ("collectTopLevel: Unsupported top-level thing: " ++ show unsupported)

--------------------------------------------------------------------------------

desugarExp :: H.Exp -> Ds L1.Exp
desugarExp e =
    case e of

      Var qname -> VarE <$> qname_to_str qname

      Con qname -> MkPackedE <$> qname_to_str qname <*> pure []

      H.Lit l   -> L1.LitE <$> lit_to_int l

      H.App e1 e2 ->
        desugarExp e1 >>= \case
          VarE "fst" ->
            L1.ProjE 0 <$> desugarExp e2
          VarE "snd" ->
            L1.ProjE 1 <$> desugarExp e2
          VarE f ->
            L1.AppE f <$> desugarExp e2
          MkPackedE c as -> do
            e2' <- desugarExp e2
            return (L1.MkPackedE c (as ++ [e2']))
          L1.AppE f l -> do
            e2' <- desugarExp e2
            return (L1.AppE f (MkProdE [l,e2']))
          f ->
            err ("Only variables allowed in operator position in function applications. (found: " ++ show f ++ ")")

      H.Tuple Unboxed _ ->
        err "Only boxed tuples are allowed."
      H.Tuple Boxed [e1, e2] ->
        (\a b -> MkProdE [a,b]) <$> desugarExp e1 <*> desugarExp e2
      H.Tuple _ es ->
        err ("Tuples can only be pairs. (" ++ show es ++ ")")

      H.Let (BDecls decls) rhs -> do
        rhs' <- desugarExp rhs
        foldrM generateBind rhs' decls

      H.Case scrt alts -> do
        scrt' <- desugarExp scrt
        CaseE scrt' <$> mapM desugarAlt alts

      H.Paren e' -> desugarExp e'

      H.InfixApp e1 op e2 -> do
        e1' <- desugarExp e1
        e2' <- desugarExp e2
        op' <- desugarOp  op
        return (PrimAppE op' [e1', e2'])

      _ -> err ("desugarExp: Unsupported expression: " ++ show e)

-------------------------------------------------------------------------------

desugarOp :: QOp -> Ds Prim
desugarOp (QVarOp (UnQual (Symbol "+"))) = return AddP
desugarOp (QVarOp (UnQual (Symbol "-"))) = return SubP
desugarOp (QVarOp (UnQual (Symbol "*"))) = return MulP
desugarOp op                             = err ("Unsupported binary op: " ++ show op)

--------------------------------------------------------------------------------

generateBind :: H.Decl -> L1.Exp -> Ds L1.Exp

generateBind (PatBind _ _ _ Just{}) _ =
    err "where clauses not allowed"

generateBind (PatBind _ _ GuardedRhss{} _) _ =
    err "Guarded right hand side not supported."

generateBind (PatBind _ (PVar v) (UnGuardedRhs rhs) Nothing) e = do
    rhs' <- desugarExp rhs
    return (LetE (name_to_str v, __, rhs') e)

generateBind (PatBind _ not_var _ _) _ =
    err ("Only variable bindings are allowed in let. (found: " ++ show not_var ++ ")")

generateBind not_pat_bind _ =
    err ("Only variable bindings are allowed in let. (found: " ++ show not_pat_bind ++ ")")

--------------------------------------------------------------------------------

desugarAlt :: H.Alt -> Ds (DataCon, [Var], L1.Exp)

desugarAlt (H.Alt _ (PApp qname ps) (UnGuardedRhs rhs) Nothing) = do
    con_name <- qname_to_str qname
    ps' <- forM ps $ \case PVar v -> return (name_to_str v)
                           _      -> err "Non-variable pattern in case."
    rhs' <- desugarExp rhs
    return (con_name, ps', rhs')

desugarAlt (H.Alt _ _ GuardedRhss{} _) =
    err "Guarded RHS not supported in case."

desugarAlt (H.Alt _ _ _ Just{}) =
    err "Where clauses not allowed in case."

desugarAlt (H.Alt _ pat _ _) =
    err ("Unsupported pattern in case: " ++ show pat)

-------------------------------------------------------------------------------

-- | Top-level function definitions can have arrow in the types. Others can't.
data TopTy
  = Arrow [Ty]
  | T1 Ty
  deriving (Show)

desugarTopType :: H.Type -> Ds TopTy

desugarTopType (TyFun t1 t2) = do
    t1' <- desugarType t1
    t2' <- desugarTopType t2
    return . Arrow $ case t2' of
                       Arrow ts -> t1' : ts
                       T1 t     -> [t1', t]

desugarTopType ty =
    T1 <$> desugarType ty

desugarType :: H.Type -> Ds Ty

desugarType (TyCon (UnQual (Ident "Int"))) = return IntTy

desugarType (TyCon (UnQual (Ident con))) = return (Packed con)

desugarType (TyTuple Boxed [ty1, ty2]) = (\a b-> ProdTy [a,b]) <$> desugarType ty1 <*> desugarType ty2

desugarType (TyApp (TyCon (UnQual (Ident "Dict"))) ty) = SymDictTy  <$> desugarType ty

desugarType ty@(TyApp ty1 _ty2) =
    desugarType ty1 >>= \case
      Packed con -> do
        -- ty2' <- desugarType ty2
        return (Packed con)
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

----------------------------------------

parseFile :: FilePath -> IO (L1.Prog, Int)
parseFile path = do 
    fmap parse (readFile path) >>= \case
      ParseOk hs -> do
        putStrLn "haskell-src-exts parsed OK. Desugaring..."
        case desugarModule hs of
          Right ast -> do
            putStrLn "Desugared AST:"
            print ast
            return (ast,0)
          Left er -> do
            error ("Desugaring failed: " ++ er)
      ParseFailed _ er -> do
        error ("haskell-src-exts failed: " ++ er)
