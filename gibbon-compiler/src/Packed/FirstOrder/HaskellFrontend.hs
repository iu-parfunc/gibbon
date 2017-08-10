{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TupleSections        #-}

module Packed.FirstOrder.HaskellFrontend
  ( parseFile
  -- * Everything else could remain internal:
  , desugarModule
  , desugarExp
  , desugarTopType
  , desugarType
  ) where

--------------------------------------------------------------------------------

import Control.Monad (forM)
import Data.Either (partitionEithers)
import Data.Foldable (foldrM)
import Data.Loc
import Data.Maybe (catMaybes)
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax as H
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L

import Packed.FirstOrder.L1.Syntax as L1
import Packed.FirstOrder.Common as C

--------------------------------------------------------------------------------

type Ds a = Either String a

err :: String -> Ds a
err = Left

--------------------------------------------------------------------------------

desugarModule :: H.Module -> Ds Prog1
desugarModule (H.Module _ _ _ _ _ _ decls) = do
  -- since top-level function types and their types can't be declared in
  -- single top-level declaration we first collect types and then collect
  -- definition
  funTys <- (M.fromList . catMaybes) <$> mapM collectTopFunTy decls

  (dataDecls, fun_decls) <-
    (partitionEithers . catMaybes) <$> mapM (collectTopLevel funTys) decls

  let
    dataMap = M.fromList (map (\def -> (tyName def, def))  dataDecls)
    funMap  = M.fromList (map (\def -> (funName def, def)) fun_decls)

    (mainFn, funMapNoMain) =
      -- ugh, no alterF in this 'containers' version
      ( funBody <$> M.lookup (toVar "main") funMap
      , M.delete (toVar "main") funMap
      )
    -- TODO(cskksc): See SExpFrontend.hs#L208
    mainExp = fmap (\ex -> (voidTy,ex)) mainFn

  return (Prog dataMap funMapNoMain mainExp)

collectTopFunTy :: H.Decl -> Ds (Maybe (Var, TopTy))
collectTopFunTy decl =
  case decl of
    TypeSig _ [n] ty -> Just <$> ((toVar . nameToStr) n ,) <$> desugarTopType ty
    ty@TypeSig{}     -> err ("Unsupported top-level type declaration: " ++ show ty)
    FunBind{}        -> return Nothing
    DataDecl{}       -> return Nothing
    unsupported      -> err ("collectTopFunTy: Unsupported top-level thing: " ++ show unsupported)


collectTopLevel :: M.Map Var TopTy -> H.Decl -> Ds (Maybe (Either (DDef Ty1) (FunDef Ty1 (L L1.Exp1))))

collectTopLevel _ TypeSig{} = return Nothing

collectTopLevel funTys (FunBind [Match _ fname args Nothing (UnGuardedRhs rhs) Nothing]) = do
    let fname'      = (toVar . nameToStr) fname
        fun_ty      = M.findWithDefault (error ("Can't find function in type env: " ++ (fromVar fname')))
                      fname' funTys
    [arg']   <- mapM collectArg args
    -- Limiting to one argument for now:
    [arg_ty] <- mapM (getArgTy fun_ty) [ 1 .. length [arg'] ]
    rhs'    <- desugarExp rhs
    return (Just (Right (FunDef { funName = fname'
                                , funArg  = arg'
                                , funTy   = ArrowTy{ arrIn = arg_ty
                                                   , arrOut = getRetTy fun_ty
                                                   , locVars = []
                                                   , arrEffs = S.empty
                                                   , locRets = []}
                                , funBody = rhs'
                                })))
  where
    collectArg :: Pat -> Ds Var
    collectArg (PVar n) = return $ (toVar . nameToStr) n
    collectArg arg      = err ("Unsupported function arg: " ++ show arg)

    getArgTy :: TopTy -> Int -> Ds Ty1
    getArgTy (Arrow ts) n = return (ts !! n)
    getArgTy ty         _ = err ("getArgTy: " ++ show ty)

    getRetTy :: TopTy -> Ty1
    getRetTy (Arrow ts) = last ts
    getRetTy (T1 t)     = t

collectTopLevel _ (DataDecl _ DataType [] ty_name [] cons []) = do
    let ty_name' = nameToStr ty_name
    constrs <- mapM collectConstr cons
    return (Just (Left (DDef (toVar ty_name') constrs)))
  where
    collectConstr (QualConDecl _ [] [] (ConDecl conName arg_tys)) =
      ( nameToStr conName, ) <$>
      L.map ( False, ) <$>
      mapM desugarType arg_tys
    collectConstr unsupported =
      err ("Unsupported data constructor: " ++ show unsupported)

collectTopLevel _ unsupported = err ("collectTopLevel: Unsupported top-level thing: " ++ show unsupported)

--------------------------------------------------------------------------------

pattern FstVar :: forall t t1 (t2 :: * -> * -> *). L (PreExp t2 t1 t)
pattern FstVar <- L NoLoc (VarE (C.Var "fst"))
  where FstVar = L NoLoc (VarE (toVar "fst"))

pattern SndVar :: forall t t1 (t2 :: * -> * -> *). L (PreExp t2 t1 t)
pattern SndVar <- L NoLoc (VarE (C.Var "snd"))
  where SndVar = L NoLoc (VarE (toVar "snd"))

-- | Convert Haskell src-exts syntax to our syntax.  Handle infix operators, etc.
-- Disambiguate things that look like applications.
desugarExp :: H.Exp -> Ds (L L1.Exp1)
desugarExp e = L NoLoc <$>
    case e of
      H.Var qname -> VarE <$> toVar <$> qnameToStr qname

      Con qname -> DataConE () <$> qnameToStr qname <*> pure []

      H.Lit l   -> L1.LitE <$> litToInt l

      H.App e1 e2 ->
        desugarExp e1 >>= \case
          FstVar ->
            L1.ProjE 0 <$> desugarExp e2
          SndVar ->
            L1.ProjE 1 <$> desugarExp e2
          L _ (VarE f) ->
            L1.AppE f [] <$> desugarExp e2
          L _ (DataConE () c as) -> do
            e2' <- desugarExp e2
            return (L1.DataConE () c (as ++ [e2']))
          L _ (L1.AppE f [] l) -> do
            e2' <- desugarExp e2
            return (L1.AppE f [] (L NoLoc $ MkProdE [l,e2']))
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
        xs <- foldrM generateBind rhs' decls
        Right $ unLoc xs

      H.Case scrt alts -> do
        scrt' <- desugarExp scrt
        CaseE scrt' <$> mapM (desugarAlt ()) alts

      H.Paren e0 -> do
        e' <- desugarExp e0
        Right $ unLoc e'

      H.InfixApp e1 op e2 -> do
        e1' <- desugarExp e1
        e2' <- desugarExp e2
        op' <- desugarOp  op
        return (PrimAppE op' [e1', e2'])

      _ -> err ("desugarExp: Unsupported expression: " ++ show e)

-------------------------------------------------------------------------------

desugarOp :: QOp -> Ds Prim
desugarOp (QVarOp (UnQual (Symbol op))) =
  case op of
    "+" -> return AddP
    "-" -> return SubP
    "*" -> return MulP
    _   -> err $ "Unsupported binary op: " ++ show op

desugarOp op = err $ "Unsupported op: " ++ show op

--------------------------------------------------------------------------------

generateBind :: H.Decl -> L (L1.Exp1) -> Ds (L (L1.Exp1))
generateBind decl exp =
  case decl of
    PatBind _ _ _ Just{}        -> err "where clauses not allowed"
    PatBind _ _ GuardedRhss{} _ -> err "Guarded right hand side not supported."

    PatBind _ (PVar v) (UnGuardedRhs rhs) Nothing -> do
      rhs' <- desugarExp rhs
      return $ L NoLoc $ LetE ((toVar . nameToStr) v, [],
                              error "Haskell front end doesn't know type.  Must infer",
                              rhs')
                         exp

    PatBind _ not_var _ _ -> err $ "Only variable bindings are allowed in let."
                                   ++ "(found: "++ show not_var ++ ")"

    oth -> err ("Only variable bindings are allowed in let. (found: " ++ show oth ++ ")")


--------------------------------------------------------------------------------

desugarAlt :: l -> H.Alt -> Ds (DataCon, [(Var,l)], L (L1.Exp1))
desugarAlt dummyL alt =
  case alt of
    H.Alt _ (PApp qname ps) (UnGuardedRhs rhs) Nothing -> do
      conName <- qnameToStr qname
      ps' <- forM ps $ \case PVar v -> return $ (toVar . nameToStr) v
                             _      -> err "Non-variable pattern in case."
      rhs' <- desugarExp rhs
      return (conName, [(v,dummyL) | v <- ps'], rhs')

    H.Alt _ _ GuardedRhss{} _ -> err "Guarded RHS not supported in case."
    H.Alt _ _ _ Just{}        -> err "Where clauses not allowed in case."
    H.Alt _ pat _ _           -> err $ "Unsupported pattern in case: " ++ show pat

-------------------------------------------------------------------------------

-- | Top-level function definitions can have arrow in the types. Others can't.
data TopTy = Arrow [Ty1]
           | T1 Ty1
  deriving (Show)

desugarTopType :: H.Type -> Ds TopTy
desugarTopType ty =
  case ty of
    TyFun t1 t2 -> do
      t1' <- desugarType t1
      t2' <- desugarTopType t2
      return $ Arrow $
        case t2' of
          Arrow ts -> t1' : ts
          T1 t     -> [t1', t]

    _ -> T1 <$> desugarType ty


desugarType :: H.Type -> Ds Ty1
desugarType ty =
  case ty of
    TyCon (UnQual (Ident "Int")) -> return IntTy
    TyCon (UnQual (Ident con))   -> return (Packed con)
    TyTuple Boxed [ty1, ty2]     -> (\a b-> ProdTy [a,b])
                                    <$> desugarType ty1
                                    <*> desugarType ty2

    TyApp (TyCon (UnQual (Ident "Dict"))) ty' -> SymDictTy <$> desugarType ty'

    TyApp ty1 _ty2 ->
      desugarType ty1 >>= \case
      Packed con -> return (Packed con)
      _ -> err ("Unsupported type: " ++ show ty)

    _ -> err $ "Unsupported type: " ++ show ty

--------------------------------------------------------------------------------

qnameToStr :: QName -> Ds String
qnameToStr qname =
  case qname of
    Qual mname n -> return (mnameToStr mname ++ "." ++ nameToStr n)
    UnQual n     -> return (nameToStr n)
    Special{}    -> err $ "Special identifiers not supported: " ++ show qname

mnameToStr :: ModuleName -> String
mnameToStr (ModuleName s) = s

nameToStr :: Name -> String
nameToStr (Ident s)  = s
nameToStr (Symbol s) = s

litToInt :: Literal -> Ds Int
litToInt (H.Int i) = return (fromIntegral i) -- lossy conversion here
litToInt l         = err ("Literal not supported: " ++ show l)

----------------------------------------

parseFile :: FilePath -> IO (L1.Prog1, Int)
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
