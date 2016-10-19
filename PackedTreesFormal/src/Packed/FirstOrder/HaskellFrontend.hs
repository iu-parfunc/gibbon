{-# LANGUAGE LambdaCase #-}

module Packed.FirstOrder.HaskellFrontend where

--------------------------------------------------------------------------------

import Control.Monad (forM)
import Data.Foldable (foldrM)
import qualified Data.Map as M

import Language.Haskell.Exts.Syntax as H
import Packed.FirstOrder.L1_Source as L1

--------------------------------------------------------------------------------

type Ds a = Either String a

err :: String -> Ds a
err = Left

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
lit_to_int (Int i) = return (fromIntegral i) -- lossy conversion here
lit_to_int l       = err ("Literal not supported: " ++ show l)
