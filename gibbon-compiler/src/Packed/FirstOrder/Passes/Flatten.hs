
-- | Put the program in A-normal form where only varrefs and literals are
-- allowed in operand position.
--
--- GRAMMAR: takes an L1 program and returns an L1 program in
--- restricted form.

module Packed.FirstOrder.Passes.Flatten
  ( flatten
  , flattenExp
  , typeExp, TEnv
  ) where

-------------------------------------------------------------------------------

import Control.Monad.State
import Packed.FirstOrder.Common
import Packed.FirstOrder.L1.Syntax as L1
import qualified Packed.FirstOrder.L2.Syntax as L2
import Text.PrettyPrint.GenericPretty (Out)
    
-- import Packed.FirstOrder.L2.Syntax (isCursorTy)

import qualified Data.Map as M

import Prelude hiding (exp)

-------------------------------------------------------------------------------

-- | Flatten ensures that function operands are "trivial".
--
--   In the process, it also lifts lets out of case scrutinees, if
--   conditions, and tuple operands.
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


type Binds l e = (Var,[l],UrTy l, PreExp l e (UrTy l))

flattenExp :: forall l e . (Out l, Out e, Show l, Show e) =>
              DDefs (UrTy l) -> Env2 (UrTy l) -> PreExp l e (UrTy l) -> SyM (PreExp l e (UrTy l))
flattenExp ddefs env2 ex0 = do (b,e') <- exp (vEnv env2) ex0
                               return $ flatLets b e'
 where
   typeIt :: TEnv l -> PreExp l e (UrTy l) -> (UrTy l)
   typeIt = typeExp (ddefs,env2)

   exp :: TEnv l -> PreExp l e (UrTy l) -> SyM ([Binds l e],PreExp l e (UrTy l))
   exp tenv e0 =
     let triv :: String -> PreExp l e (UrTy l) -> SyM ([Binds l e], PreExp l e (UrTy l))
         triv m e = -- Force something to be trivial
           if isTriv e
           then return ([],e)
           else do tmp <- gensym $ toVar $ "flt" ++ m
                   let ty = typeIt tenv e
                   (bnds,e') <- exp tenv e
                   return ( bnds++[(tmp,[],ty,e')]
                          , VarE tmp)
         go = exp tenv
         gols f ls m = do (bndss,ls') <- unzip <$> mapM (triv m) ls
                          return (concat bndss, f ls')
     in
     case e0 of
       (Ext finishme)   -> error "FINISHME" -- return ([],e0)
       (VarE _)         -> return ([],e0)
       (LitE _)         -> return ([],e0)
       (LitSymE _)      -> return ([],e0)

       -- This pass is run at multiple points in the compiler pipeline.
       -- We COULD just let these patterns be treated as arbitrary AppE forms,
       -- but it is safer to handle them explicitly.
       L2.AddCursor _ _ -> return ([],e0) -- Already flat.

       L2.NewBuffer     -> return ([],e0) -- Already flat.
       L2.ScopedBuffer  -> return ([],e0) -- Already flat.
       L2.ReadInt _     -> return ([],e0) -- Already flat.
       -- Mimics the AppE case:
       L2.WriteInt v e  -> do (b1,e') <- triv "WI" e; return (b1, L2.WriteInt v e')
       -- A fail-safe:
       _ | L2.isExtendedPattern e0 -> error$ "Unhandled extended L2 pattern: "++ndoc e0

       (AppE f lvs arg)     -> do (b1,arg') <- triv "Ap" arg
                                  return (b1, AppE f lvs arg')
       (PrimAppE p ls)  -> gols (PrimAppE p)  ls "Prm"
       (MkProdE ls)     -> gols  MkProdE      ls "Prd"
       (DataConE loc k ls) -> gols (DataConE loc k) ls "Pkd"

       (LetE (v1,lv1,t1, (LetE (v2,lv2,t2,rhs2) rhs1)) bod) ->
         go $ LetE (v2,lv2,t2,rhs2) $ LetE (v1,lv1,t1,rhs1) bod

       (LetE (v,_,t,rhs) bod) -> do (bnd1,rhs') <- go rhs
                                    (bnd2,bod') <- exp (M.insert v t tenv) bod
                                    return (bnd1++[(v,[],t,rhs')]++bnd2, bod')
       (IfE a b c) -> do (b1,a') <- triv "If" a
                         (b2,b') <- go b
                         (b3,c') <- go c
                         return (b1, IfE a' (flatLets b2 b') (flatLets b3 c'))
       -- This can happen anywhere, but doing it here prevents
       -- unneccessary bloat where we can ill afford it:
       (ProjE ix l@(MkProdE ls)) ->
           dbgTrace 5 (" [flatten] Reducing project-of-tuple, index "++show ix++" expr:  "++take 80 (show l)++"...") $
           go (ls !! ix)
       (ProjE ix e) -> do (b,e') <- triv "Prj" e
                          return (b, ProjE ix e')
       (CaseE e ls) -> do (b,e') <- triv "Cse" e
                          ls' <- forM ls $ \ (k,vrs,rhs) -> do
                                   let tys = lookupDataCon ddefs k
                                       vrs' = map fst vrs
                                       tenv' = M.union (M.fromList (zip vrs' tys)) tenv
                                   (b2,rhs') <- exp tenv' rhs
                                   return (k,vrs, flatLets b2 rhs')
                          return (b, CaseE e' ls')
       -- TimeIt is treated like a conditional.  Don't lift out of it:
       (TimeIt e _t b) -> do (bnd,e') <- go e
                             return ([], TimeIt (flatLets bnd e') (typeIt tenv e) b)
       (MapE _ _)    -> error "FINISHLISTS"
       (FoldE _ _ _) -> error "FINISHLISTS"


-- | Helper function that lifts out Lets on the RHS of other Lets.
--   Absolutely requires unique names.
mkLetE :: (Var, [l], d, PreExp l e d) -> PreExp l e d -> PreExp l e d
mkLetE (vr,lvs,ty,(L1.LetE bnd e)) bod = mkLetE bnd $ mkLetE (vr,lvs,ty,e) bod
mkLetE bnd bod = L1.LetE bnd bod

-- | Alternative version of L1.mkLets that also flattens
flatLets :: [(Var,[l],d,PreExp l e d)] -> PreExp l e d -> PreExp l e d
flatLets [] bod = bod
flatLets (b:bs) bod = mkLetE b (flatLets bs bod)


type TEnv l = M.Map Var (UrTy l)

-- FIXME: Why is this not unified with Typecheck.hs?

typeExp :: (Show l, Show e, Out l, Out e) =>
           (DDefs (UrTy l), Env2 (UrTy l)) -> TEnv l -> PreExp l e (UrTy l) -> (UrTy l)
typeExp (_dd,_env2) env (L1.VarE v) =
--    M.findWithDefault (L1.Packed "CURSOR_TY") v env
   M.findWithDefault (error ("Cannot find type of variable " ++ show v)) v env

typeExp (_dd,_env2) _env (L1.LitE _i) = L1.IntTy
typeExp _ _ (L1.LitSymE _)          = L1.SymTy
typeExp (_dd,env2) _env (L1.AppE v _lvs _e) = snd $ fEnv env2 # v

typeExp (_,_) _env (L1.PrimAppE p _es) =
    case p of
      L1.AddP -> L1.IntTy
      L1.SubP -> L1.IntTy
      L1.MulP -> L1.IntTy
      L1.EqIntP -> L1.BoolTy
      L1.EqSymP -> L1.BoolTy
      L1.MkTrue -> L1.BoolTy
      L1.MkFalse -> L1.BoolTy
      L1.Gensym -> L1.SymTy
      L1.DictInsertP ty -> L1.SymDictTy (noLocsHere ty)
      L1.DictLookupP ty -> noLocsHere ty
      L1.DictEmptyP ty -> L1.SymDictTy (noLocsHere ty)
      L1.DictHasKeyP ty -> L1.SymDictTy (noLocsHere ty)
      L1.SizeParam -> L1.IntTy
      L1.ReadPackedFile _ _ ty -> (noLocsHere ty)
      _ -> error $ "case " ++ (show p) ++ " not handled in typeExp yet"

typeExp (dd,env2) env (L1.LetE (v,_,t,_) e) = typeExp (dd,env2) (M.insert v t env) e
typeExp (dd,env2) env (L1.IfE _ e _) = typeExp (dd,env2) env e
typeExp (dd,env2) env e0@(L1.ProjE i e) =
    case typeExp (dd,env2) env e of
     (L1.ProdTy tys) -> tys !! i
     oth -> error $ "typeExp: Cannot project fields from this type: "++show oth
                  ++"\nExpression:\n  "++sdoc e0
                  ++"\nEnvironment:\n  "++sdoc env
typeExp (dd,env2) env (L1.MkProdE es) =
    L1.ProdTy $ map (typeExp (dd,env2) env) es
typeExp (dd,env2) env (L1.CaseE _e mp) =
    let (c,args,e) = head mp
        args' = map fst args
    in typeExp (dd,env2) (M.fromList (zip args' (lookupDataCon dd c)) `M.union` env) e

typeExp (dd,_) _env (L1.DataConE l c _es) = L1.PackedTy (getTyOfDataCon dd c) l

typeExp (dd,env2) env (L1.TimeIt e _ _) = typeExp (dd,env2) env e
typeExp (dd,env2) env (L1.MapE _ e)     = typeExp (dd,env2) env e
typeExp (dd,env2) env (L1.FoldE _ _ e)  = typeExp (dd,env2) env e
typeExp (_,_) _ exp = error $ "typeExp: " ++ show exp ++ " not implemented"


-- This crops up in the types for primitive operations.  Can we remove the need for this?
noLocsHere :: Show a => UrTy a -> UrTy b
noLocsHere t = fmap (\_ -> error $ "This type should not contain a location: "++show t) t
