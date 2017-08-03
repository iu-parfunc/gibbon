
-- | Compiler pass to inline trivials.
module Packed.FirstOrder.Passes.InlineTriv (inlineTriv, inlineTrivExp) where


import           GHC.Stack (errorWithStackTrace)
import           Packed.FirstOrder.Common
import           Packed.FirstOrder.GenericOps (NoExt)
import           Packed.FirstOrder.L1.Syntax as L1 hiding (mkProj)
import qualified Packed.FirstOrder.L2.Syntax as L2
import           Prelude hiding (exp)
import           Text.PrettyPrint.GenericPretty

-- import Packed.FirstOrder.Passes.Flatten (typeExp, TEnv)


-- | Inline trivial let bindings (binding a var to a var or int), mainly to clean up
--   the output of `flatten`.
inlineTriv :: Prog -> Prog
inlineTriv (Prog ddefs funs main) =
    Prog ddefs (fmap inlineTrivFun funs) (fmap (inlineTrivExp ddefs) main)
  where
    inlineTrivFun (FunDef nam (narg,targ) ty bod) =
      FunDef nam (narg,targ) ty (inlineTrivExp ddefs bod)

type MyExp l = PreExp NoExt l (UrTy l)
type Env l = [(Var, (UrTy l, MyExp l))]

inlineTrivExp :: forall l a . (Out l, Show l)
              => DDefs a -> MyExp l -> MyExp l
inlineTrivExp _ddefs = go []
  where

  -- Just a hook for debugging:
  go :: Env l -> MyExp l -> MyExp l
  go env e  =
      -- dbgTrace 7 ("Inline, processing with env:\n "++sdoc env++"\n exp: "++sdoc e) $
      exp env e

  -- | Hree we go to some lengths to maintain the syntactic invariants
  -- for the extended L2 forms. The idea is that we can only reference
  -- variables within these forms, but we still must apply the
  -- environment because the old bindings have been removed.
  --
  -- An alternative would be to let the extended forms disappear at
  -- this point, and handle them at the level of "AppE" in Lower.hs.
  withVar :: Env l -> Var -> (Var -> MyExp l) -> MyExp l
  withVar env v fn =
    case lookup v env of
      Nothing        -> fn v
      Just (_, (VarE v2)) -> fn v2
      -- fixme, need gensym:
      Just (ty,oth)  -> LetE (v,[],ty,oth) $ fn v

  exp :: Env l -> MyExp l -> PreExp NoExt l (UrTy l)
  exp env e0 =
    case e0 of
      Ext _  -> e0
      VarE v -> case lookup v env of
                    Nothing -> VarE v
                    Just (_,e) -> e
      LitE i -> LitE i
      LitSymE v -> LitSymE v

      AppE v lvs e -> AppE v lvs $ go env e
      PrimAppE p es -> PrimAppE p $ map (go env) es

      LetE (v,lvs,t,e') e ->
       case e' of
         VarE v' -> case lookup v' env of
                      Nothing -> go ((v,(t,e')):env) e
                      Just pr -> go ((v,pr):env) e
         et | L1.isTriv et ->
                -- Apply existing renames:
                let et' = go env et in
                go ((v,(t,et')):env) e
         _ -> LetE (v,lvs,t,go env e') (go env e)

      IfE e1 e2 e3 -> IfE (go env e1) (go env e2) (go env e3)

      -- TODO: Type check here:
      ProjE i e -> mkProj i $ go env e

      MkProdE es -> MkProdE $ map (go env) es
      CaseE e mp ->
       let e' = go env e
           mp' = map (\(c,args,ae) -> (c,args,go env ae)) mp
       in CaseE e' mp'

      DataConE loc c es -> DataConE loc c $ map (go env) es
      TimeIt e t b -> TimeIt (go env e) t b
      MapE (v,t,e') e -> MapE (v,t,go env e') (go env e)
      FoldE (v1,t1,e1) (v2,t2,e2) e3 ->
       FoldE (v1,t1,go env e1) (v2,t2,go env e2) (go env e3)

-- FIXME: Remove:
      L2.NewBuffer -> L2.NewBuffer
      L2.ReadInt v     -> withVar env v $ \v2 -> L2.ReadInt v2
      L2.WriteInt v e  -> withVar env v $ \v2 -> L2.WriteInt v2 (go env e)
      L2.AddCursor v i -> withVar env v $ \v2 -> L2.AddCursor v2 i

      p | L2.isExtendedPattern p ->
          errorWithStackTrace $ "InlineTriv: failed to handle extended L2 form: "
          ++ndoc p++", env: "++ndoc env


-- Helpers which do opportunistic reduction:

mkProj :: Int -> MyExp l -> MyExp l
mkProj ix (MkProdE ls) = ls !! ix
mkProj ix e = ProjE ix e
