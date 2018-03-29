module Packed.FirstOrder.L0.Mono
  (specialize, CurFun, CCall, FCall, L0Fun)
  where

import Data.Map as M
import Data.List as L
import Data.Loc

import Packed.FirstOrder.Common as C

import Packed.FirstOrder.L0.Syntax as L0
import Packed.FirstOrder.L1.Syntax as L1

-- some type defns to make things look cleaner
type Exp = (L Exp0)

-- we now have curried functions and curried calls
-- curried functions are these variable defns
-- but curried calls vs function calls are PolyAppE vs AppE
type CurFun  = VarDef Ty0 Exp
type CCall = Exp

type L0Fun = FunDef Ty0 Exp
type FCall = Exp

type VarMap = Map Var Exp

specialize :: CurFun -> CCall -> (L0Fun, (CCall -> FCall))
specialize f@VarDef {varTy=ty, varBody} call = (newFunc, callFunc)
  where args     = reverse $ getArgs call
        varMap   = varsToArgs (getVars varBody) args
        newTy    = updateTy ty [] args
        newFunc  = specializeFunc f newTy varMap
        callFunc = \ x -> updateCall x $ funName newFunc
        
varsToArgs :: [Var] -> [Exp] -> VarMap
varsToArgs vs as = M.fromList $ L.filter f $ L.zip vs as
  where f = \ p -> not $ isVarE $ snd p 

updateTy :: Ty0 -> [Ty0] -> [Exp] -> Ty0
updateTy oldTy _ [] = oldTy
updateTy (ArrowTy t0 t1) ts (a:[]) = if isVarE a
                                     then ArrowTy (L0.ProdTy $ ts++[t0]) t1
                                     else ArrowTy (L0.ProdTy ts) t1
updateTy (ArrowTy t0 t1) ts (a:as) = if isVarE a
                                     then updateTy t1 (t0:ts) as
                                     else updateTy t1 ts as
updateTy err _ _ = error $ "Not an arrow type " ++ show err
                                        
-- now actually specialize the function to the values in the call
specializeFunc :: CurFun -> Ty0 -> VarMap -> L0Fun
specializeFunc VarDef {varName, varBody} (ArrowTy t0 t1) varMap =
  FunDef {funName=newName, funArg=(newVar,t0), funRetTy=t1, funBody=newFB}
    where
      newName = fst $ runSyM 1 $ gensym varName
      newVar  = toVar "x" -- just a variable name, may cause issues later
      newVM   = varsToTuple 0 $ getVars varBody
      newFB   = specializeFB newVM varBody

      -- map the variables from the lambda to a tuple access
      -- if the variable is already mapped, keep the old value in the map
      varsToTuple :: Int -> [Var] -> VarMap
      varsToTuple _ [] = varMap
      varsToTuple i (v:vs) =
        case M.lookup v varMap of
          Just _  -> varsToTuple i vs
          Nothing -> M.insert v (l$ ProjE i $ l$ VarE newVar) $ varsToTuple (succ i) vs

      -- specialize the function body
      specializeFB :: VarMap -> Exp -> Exp
      specializeFB vM expr@(L loc ex) =
        case ex of
          VarE x -> case M.lookup x vM of
                      Just v  -> v
                      Nothing -> if x == varName then L loc $ VarE newName else expr
          -- remove the specialized argument(s) from recursive calls
          -- change from PolyApp to App
          Ext (PolyAppE _ _) | isSelfCall expr varName ->
            L loc $ AppE newName [] $ L loc $ MkProdE $ filterArgs expr
             -- this removes any arguments that exist within the varmap
             where filterArgs = L.filter f . collectArgs
                   f = \ x -> case x of
                                (L _ (VarE y)) -> not $ member y vM
                                _              -> False
          -- application with a var (i.e. a lambda)
          Ext (PolyAppE r@(L _ (VarE x)) rd) ->
            case M.lookup x vM of
              Just v  -> replaceLam v $ specializeFB vM rd
              Nothing -> L loc $ Ext $ PolyAppE r $ specializeFB vM rd
          -- remove any lambdas with specialized variables
          -- this includes removing the "top level" lambda(s),
          -- since the top level variables are assigned to tuple access
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
          CaseE k ls         -> L loc $ CaseE (specializeFB vM k) $ L.map f ls
                                 where f = (\ (ds,vs,es) -> (ds,vs,specializeFB vM es))
          LetE (x,ls,d,a) bd -> L loc $ LetE (x,ls,d, specializeFB vM a) $ specializeFB vM bd
          TimeIt x d p       -> L loc $ TimeIt (specializeFB vM x) d p
          Ext (PolyAppE a d) -> L loc $ Ext $ PolyAppE (specializeFB vM a) (specializeFB vM d)
          MapE _ _     -> error $ "not implemented"
          FoldE _ _ _  -> error $ "not implemented"
specializeFunc _ t _ = error $ "Not an Arrow type: " ++ show t       

-- replace the each instance of the variable in the lambda with the given expression
replaceLam :: Exp -> Exp -> Exp
replaceLam (L _ (Ext (LambdaE var body))) ex = replace var ex body
  where
      -- replace in the body of the lambda
      replace :: Var -> Exp -> Exp -> Exp
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
replaceLam e _ = error $ "Not a lambda : " ++ show e                                                       

-- used to update the callsite(s)
-- changes curried functions to function calls with tuples
updateCall :: CCall -> Var -> FCall
updateCall e@(L rloc ex) newName =
  case ex of
    Ext (PolyAppE _ _) -> L rloc $ AppE newName [] $ L rloc $ MkProdE $ collectArgs e
    err                -> error $ "Not a valid call to update " ++ show err

-- Helpers

isVarE :: Exp -> Bool
isVarE (L _ e) = case e of
                   VarE _ -> True
                   _      -> False

getArgs :: CCall -> [Exp]
getArgs (L _ call) =
  case call of
    Ext (PolyAppE f a) -> a : getArgs f
    _                  -> []

collectArgs :: CCall -> [Exp]
collectArgs = L.filter isVarE . reverse . getArgs

getVars :: Exp -> [Var]
getVars (L _ ex) =
  case ex of
    Ext (LambdaE v b) -> v : getVars b
    _                 -> []

isSelfCall :: Exp -> Var -> Bool
isSelfCall (L _ ex) name =
  case ex of
    AppE v _ _ -> v == name
    Ext (PolyAppE (L _ (VarE v)) _) -> v == name
    Ext (PolyAppE f _) -> isSelfCall f name
    err                -> error $ "Not an application " ++ show err
