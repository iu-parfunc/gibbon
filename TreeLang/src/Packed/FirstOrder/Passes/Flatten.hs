-- | Put the program in A-normal form where only varrefs and literals are
-- allowed in operand position.

module Packed.FirstOrder.Passes.Flatten
  ( flatten
  , flattenExp
  , typeExp
  ) where

-------------------------------------------------------------------------------

import Control.Monad.State
import Packed.FirstOrder.Common
import qualified Packed.FirstOrder.L1_Source as L1
import Packed.FirstOrder.L1_Source (Exp(..), Prim(..))

import qualified Data.Map as M

import Prelude hiding (exp)

-------------------------------------------------------------------------------

flatten :: L1.Prog -> SyM L1.Prog
flatten prg@(L1.Prog defs funs main) = do
    main' <- mapM (flattenExp defs env20) main
    funs' <- flattenFuns funs
    return $ L1.Prog defs funs' main'
  where
    flattenFuns = mapM flattenFun
    flattenFun (FunDef nam (narg,targ) ty bod) = do
      let env2 = Env2 (M.singleton narg targ) (fEnv env20)
      bod' <- flattenExp defs env2 bod
      return $ FunDef nam (narg,targ) ty bod'

    env20 = L1.progToEnv prg

-- NOTE: / FIXME
-- If we would just include arrow types in the grammar from the start,
-- the the typeenv could contain function types too.  Data constructors could
-- go in there too.  Everything would be simpler.  We would simply have to use other means
-- to remember that L1 programs are first order.

            
flattenExp :: DDefs L1.Ty -> Env2 L1.Ty -> L1.Exp -> SyM L1.Exp
flattenExp defs env2 = fExp (vEnv env2)
  where
    fExp :: M.Map Var L1.Ty -> L1.Exp -> SyM L1.Exp
    fExp _env (L1.VarE v) = return $ L1.VarE v
    fExp _env (L1.LitE i) = return $ L1.LitE i
    fExp _env (L1.AppE v (L1.VarE v')) = return $ L1.AppE v (L1.VarE v')
    fExp env (L1.AppE v e) =
        do e' <- fExp env e
           v' <- gensym "tmp_flat"
           let ty = typeExp (defs,env2) env e
           return $ mkLetE (v',ty,e') (L1.AppE v (L1.VarE v'))
    fExp env (L1.PrimAppE p es) =
        do es' <- mapM (fExp env) es
           nams <- mapM gensym $ replicate (length es) "tmp_flat"
           let bind [] e = e
               bind ((v,e'):xs) e = mkLetE (v,(typeExp (defs,env2) env e'),e') $ bind xs e
           let exp = bind (zip nams es') $ L1.PrimAppE p $ map L1.VarE nams
           return exp
    fExp env (L1.LetE (v,t,e') e) =
        do fe' <- fExp env e'
           fe  <- fExp (M.insert v t env) e
           let exp = mkLetE (v,t,fe') fe
           return exp
    fExp env (L1.IfE e1 e2 e3) =
        do fe1 <- fExp env e1
           fe2 <- fExp env e2
           fe3 <- fExp env e3
           v1 <- gensym "tmp_flat"
           return $ mkLetE (v1,L1.BoolTy,fe1) $ L1.IfE (L1.VarE v1) fe2 fe3
    fExp env (L1.ProjE i e) =
        do fe <- fExp env e
           let ty = typeExp (defs,env2) env e
           v1 <- gensym "tmp_flat"
           return $ mkLetE (v1,ty,fe) $ L1.ProjE i (L1.VarE v1)
    fExp env (L1.MkProdE es) =
        do fes <- mapM (fExp env) es
           nams <- mapM gensym $ replicate (length fes) "tmp_flat"
           let tys = map (typeExp (defs,env2) env) fes
               bind [] e            = e
               bind ((v,t,e'):xs) e = mkLetE (v,t,e') $ bind xs e
           return $ bind (zip3 nams tys fes) $ L1.MkProdE $ map L1.VarE nams
    fExp env (L1.CaseE e mp) =
        do fe <- fExp env e
           v <- gensym "tmp_flat"
           let ty  = typeExp (defs,env2) env fe
           fals <- forM mp $ \(c,args,ae) -> do
                     let tys = lookupDataCon defs c
                     fae <- fExp (M.fromList (zip args tys) `M.union` env) ae
                     return (c,args,fae)
           return $ mkLetE (v,ty,fe) $ L1.CaseE (L1.VarE v) fals
    fExp env (L1.MkPackedE c es) =
        do fes <- mapM (fExp env) es
           nams <- mapM gensym $ replicate (length fes) "tmp_flat"
           let tys = map (typeExp (defs,env2) env) fes
               bind [] e            = e
               bind ((v,t,e'):xs) e = mkLetE (v,t,e') $ bind xs e
           return $ bind (zip3 nams tys fes) $ L1.MkPackedE c $ map L1.VarE nams
    -- very important to NOT "flatten" the time form:
    fExp env (L1.TimeIt e _) =
        do fe <- fExp env e
           let ty = typeExp (defs,env2) env e
           return $ L1.TimeIt fe ty
    fExp env (L1.MapE (v,t,e') e) =
        do fe' <- fExp env e'
           fe <- fExp env e
           return $ L1.MapE (v,t,fe') fe
    fExp env (L1.FoldE (v1,t1,e1) (v2,t2,e2) e3) =
        do fe1 <- fExp env e1
           fe2 <- fExp env e2
           fe3 <- fExp env e3
           return $ L1.FoldE (v1,t1,fe1) (v2,t2,fe2) fe3

    -- | Helper function that lifts out Lets on the RHS of other Lets.
    --   Absolutely requires unique names.
    mkLetE (vr,ty, L1.LetE bnd e) bod = mkLetE bnd $ mkLetE (vr,ty,e) bod
    mkLetE bnd bod = L1.LetE bnd bod

type TyEnv = M.Map Var L1.Ty
                     
typeExp :: (DDefs L1.Ty,Env2 L1.Ty) -> TyEnv -> L1.Exp -> L1.Ty
typeExp (dd,env2) env (L1.VarE v) =
  M.findWithDefault (error ("Cannot find type of variable " ++ show v)) v env

typeExp (dd,env2) _env (L1.LitE _i) = L1.IntTy
typeExp (dd,env2) _env (L1.AppE v _e) = snd $ fEnv env2 # v

typeExp (dd,env2) _env (L1.PrimAppE p _es) =
    case p of
      L1.AddP -> L1.IntTy
      L1.SubP -> L1.IntTy
      L1.MulP -> L1.IntTy
      L1.EqIntP -> L1.BoolTy
      L1.EqSymP -> L1.BoolTy
      L1.MkTrue -> L1.BoolTy
      L1.MkFalse -> L1.BoolTy
      L1.DictInsertP ty -> L1.SymDictTy ty
      L1.DictLookupP ty -> ty
      L1.DictEmptyP ty -> L1.SymDictTy ty
      _ -> error $ "case " ++ (show p) ++ " not handled in typeExp yet"
typeExp (dd,env2) env (L1.LetE (v,t,_) e) = typeExp (dd,env2) (M.insert v t env) e
typeExp (dd,env2) env (L1.IfE _ e _) = typeExp (dd,env2) env e
typeExp (dd,env2) env e0@(L1.ProjE i e) =
    case typeExp (dd,env2) env e of 
     (L1.ProdTy tys) -> tys !! i 
     oth -> error $ "typeExp: Cannot project fields from this type: "++show oth
                  ++"\nExpression:\n  "++sdoc e0
typeExp (dd,env2) env (L1.MkProdE es) =
    L1.ProdTy $ map (typeExp (dd,env2) env) es
typeExp (dd,env2) env (L1.CaseE _e mp) =
    let (c,args,e) = head mp
    in typeExp (dd,env2) (M.fromList (zip args (lookupDataCon dd c)) `M.union` env) e
typeExp (dd,env2) _env (L1.MkPackedE c _es) = L1.Packed c
typeExp (dd,env2) env (L1.TimeIt e _) = typeExp (dd,env2) env e
typeExp (dd,env2) env (L1.MapE _ e) = typeExp (dd,env2) env e
typeExp (dd,env2) env (L1.FoldE _ _ e) = typeExp (dd,env2) env e
