module Packed.FirstOrder.L0.Mono
  (specialize, L0Fun, L0Call)
  where

import Data.Map as M
import Data.List as L
import Data.Loc

import Packed.FirstOrder.Common as C

import Packed.FirstOrder.L0.Syntax as L0
import Packed.FirstOrder.L1.Syntax as L1

type L0Fun  = VarDef Ty0 (L Exp0)
type L0Call = (L Exp0)
type VarMap = Map Var (L Exp0)


specialize :: L0Fun -> L0Call -> (L0Fun, (L0Call -> L0Call))
specialize f@VarDef {varTy=ty, varBody} call = (newFunc, callFunc)
  where args     = reverse $ getArgs call
        varMap   = varsToArgs (getVars varBody) args
        newTy    = updateTy ty args
        newFunc  = specializeFunc f newTy varMap
        callFunc = \ x -> updateCall x $ varName newFunc
    
    
varsToArgs :: [Var] -> [(L Exp0)] -> VarMap
varsToArgs vs as = M.fromList $ L.filter f $ L.zip vs as
  where f = \ p -> not $ isVarE $ snd p 

updateTy :: Ty0 -> [(L Exp0)] -> Ty0
updateTy oldTy [] = oldTy
updateTy (ArrowTy t0 t1) (a:as) = if isVarE a
                                  then ArrowTy t0 $ updateTy t1 as
                                  else updateTy t1 as
updateTy err _ = error $ "Not an arrow type " ++ show err
                                        
-- now actually specialize the function to the values in the call
specializeFunc :: L0Fun -> Ty0 -> VarMap -> L0Fun
specializeFunc VarDef {varName, varBody} ty varMap =
  VarDef {varName=newName, varTy=ty, varBody=newFB}
    where
      newName = fst $ runSyM 1 $ gensym varName
      newFB   = specializeFB varMap varBody

      -- specialize the function body
      specializeFB :: VarMap -> (L Exp0) -> (L Exp0)
      specializeFB vM expr@(L loc ex) =
        case ex of
          VarE x -> case M.lookup x vM of
                      Just v  -> v
                      Nothing -> if x == varName then (L loc $ VarE newName) else expr
          -- remove the specialized variable(s) from recursive calls
          Ext (PolyAppE f d@(L _ (VarE x))) | isSelfCall expr varName ->
            case M.lookup x vM of
              Just _  -> specializeFB vM f
              Nothing -> L loc $ Ext $ PolyAppE (specializeFB vM f) (specializeFB vM d)
          -- application with a var (i.e. a lambda)
          Ext (PolyAppE r@(L _ (VarE x)) rd) ->
            case M.lookup x vM of
              Just v  -> replaceLam v $ specializeFB vM rd
              Nothing -> L loc $ Ext $ PolyAppE r $ specializeFB vM rd
          -- remove the specialized variables from lambdas
          Ext (LambdaE x bd) ->
            case M.lookup x vM of
              Just _  -> specializeFB vM bd
              Nothing -> L loc $ Ext $ LambdaE x $ specializeFB vM bd
          LitE _    -> expr
          LitSymE _ -> expr
          PrimAppE p ls -> L loc $ PrimAppE p $ L.map (specializeFB vM) ls
          MkProdE ls    -> L loc $ MkProdE $ L.map (specializeFB vM) ls
          ProjE i x     -> L loc $ ProjE i $ specializeFB vM x
          -- application, change name if necessary
          AppE a ls d   -> if (a == varName)
                           then L loc $ AppE newName ls $ specializeFB vM d
                           else L loc $ AppE a ls $ specializeFB vM d
          IfE p t f     -> L loc $ IfE (specializeFB vM p)
                           (specializeFB vM t)
                           (specializeFB vM f)
          DataConE ls k as   -> L loc $ DataConE ls k $ L.map (specializeFB vM) as
          CaseE k ls         -> L loc $ CaseE k $ L.map (\ (ds,vs,es) -> (ds,vs,specializeFB vM es)) ls
          LetE (x,ls,d,a) bd -> L loc $ LetE (x,ls,d, specializeFB vM a) $ specializeFB vM bd
          TimeIt x d p       -> L loc $ TimeIt (specializeFB vM x) d p
          Ext (PolyAppE a d) -> L loc $ Ext $ PolyAppE (specializeFB vM a) (specializeFB vM d)
          MapE _ _     -> error $ "not implemented"
          FoldE _ _ _  -> error $ "not implemented"

      -- replace each instance of the variable in the lambda
      replaceLam :: (L Exp0) -> (L Exp0) -> (L Exp0)
      replaceLam (L _ (Ext (LambdaE v b))) e = replace v e b
      replaceLam e _ = error $ "Not a lambda : " ++ show e

      -- replace in the body of the lambda
      -- these are useful functions that should probably exist outside this scope
      replace :: Var -> (L Exp0) -> (L Exp0) -> (L Exp0)
      replace v e expr@(L loc b) =
        case b of
          VarE v2 | v == v2 -> e
          VarE _    -> error $ "unbound variable in lambda"
          LitE _    -> expr
          LitSymE _ -> expr
          PrimAppE p ls -> L loc $ PrimAppE p $ L.map (replace v e) ls
          MkProdE ls    -> L loc $ MkProdE $ L.map (replace v e) ls
          ProjE i x     -> L loc $ ProjE i $ replace v e x
          AppE a ls d   -> L loc $ AppE a ls $ replace v e d
          IfE p t f     -> L loc $ IfE (replace v e p)
                           (replace v e t)
                           (replace v e f)
          DataConE ls k as   -> L loc $ DataConE ls k $ L.map (replace v e) as
          CaseE k ls         -> L loc $ CaseE k $ L.map (\ (ds,vs,es) -> (ds,vs,replace v e es)) ls
          LetE (x,ls,d,a) bd -> L loc $ LetE (x,ls,d, replace v e a) $ replace v e bd
          TimeIt x d p       -> L loc $ TimeIt (replace v e x) d p
          Ext (LambdaE x bd) -> L loc $ Ext $ LambdaE x $ replace v e bd
          Ext (PolyAppE a d) -> L loc $ Ext $ PolyAppE (replace v e a) (replace v e d)
          MapE _ _     -> error $ "not implemented"
          FoldE _ _ _  -> error $ "not implemented"
                                                       

-- used to update the callsite(s), filters out anything
-- out of the product that is not a variable
-- TODO again this issue of single variable
updateCall :: L0Call -> Var -> L0Call
updateCall (L rloc ex) newName =
  case ex of
    Ext (PolyAppE (L vl (VarE _)) arg) -> if isVarE arg
                                         then L rloc $ Ext $ PolyAppE (L vl $ VarE newName) arg
                                         else L vl $ VarE newName
    Ext (PolyAppE f arg) -> if isVarE arg
                            then L rloc $ Ext $ PolyAppE (updateCall f newName) arg 
                            else (updateCall f newName)
    err                  -> error $ "Not a valid call to update " ++ show err

-- Helpers

isVarE :: (L Exp0) -> Bool
isVarE (L _ e) = case e of
                   VarE _ -> True
                   _      -> False

getArgs :: L0Call -> [(L Exp0)]
getArgs (L _ call) =
  case call of
    Ext (PolyAppE f a) -> a : getArgs f
    _                  -> []

getVars :: (L Exp0) -> [Var]
getVars (L _ ex) =
  case ex of
    Ext (LambdaE v b) -> v : getVars b
    _                 -> []

isSelfCall :: (L Exp0) -> Var -> Bool
isSelfCall (L _ ex) name =
  case ex of
    AppE v _ _ -> v == name
    Ext (PolyAppE (L _ (VarE v)) _) -> v == name
    Ext (PolyAppE f _) -> isSelfCall f name
    err                -> error $ "Not an application " ++ show err

