{-# LANGUAGE BlockArguments #-}

module Gibbon.Passes.RegionsInwards (regionsInwards) where

import GHC.Generics (Generic)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L 
import Data.Foldable as F
import Text.PrettyPrint.GenericPretty

import Gibbon.Common
import Gibbon.L2.Syntax

-- import Gibbon.LocExp
import Data.Maybe ()
import qualified Data.Maybe as S

--define data type that can be Region, Loc, LocExp
data DelayedBind = DelayRegion Region                                         --Data type to store the delayed bindings
                 | DelayLoc LocVar LocExp | DelayParRegion Region
  deriving (Show, Generic)

instance Out DelayedBind

type DelayedBindEnv = M.Map (S.Set LocVar) [DelayedBind]                      --define a Map from set to the DelayedBind data type  

regionsInwards :: Prog2 -> PassM Prog2
regionsInwards Prog{ddefs,fundefs,mainExp} = do
    let scopeSetMain = S.fromList $ map funName (M.elems fundefs)             --Init scopeSet with all the function names
        functionArgs = S.fromList $ concatMap funArgs (M.elems fundefs)       --Init functionArgs with all the function arguments, but contenating into one list 
        scopeSetFun  = scopeSetMain `S.union` functionArgs                    --scope set for function body is the union of function args and the function names   
    fds' <- mapM (placeRegionsInwardsFunBody scopeSetFun) (M.elems fundefs)   --Delay Regions for the function body 
    let fundefs' = M.fromList $ map (\f -> (funName f,f)) fds'
    mainExp' <- case mainExp of
        Nothing -> return Nothing
        Just (mn, ty)-> do 
          let env = M.empty                                                   --Use M.empty for creating the empty env 
              in Just . (,ty) <$> placeRegionInwards env scopeSetMain mn      --Delay Regions for the main function

    return $ Prog ddefs fundefs' mainExp'                                     --return new ddefs, fundefs and mainExpressions



placeRegionsInwardsFunBody :: S.Set Var -> FunDef2  -> PassM FunDef2
placeRegionsInwardsFunBody scopeSet f@FunDef{funBody}  = do
  let env     = M.empty                                                       --Create empty environment
  funBody' <- placeRegionInwards env scopeSet funBody                         --Recursively delay regions for function body
  return $ f {funBody = funBody'}


placeRegionInwards :: DelayedBindEnv -> S.Set Var -> Exp2 -> PassM Exp2       --Recursive funtion that will move the regions inwards
placeRegionInwards env scopeSet ex  =
  case ex of
    Ext ext ->
      case ext of

        LetRegionE r rhs -> do                                                --take care of regions
          let key' = S.singleton (regionToVar r)
              val' = [DelayRegion r]
              env' = M.insert key' val' env
              in placeRegionInwards env' scopeSet rhs 
  
        LetLocE loc phs rhs -> do                                             --take care of locations    
          case phs of

            StartOfLE r -> do                                                 
              let keyList' = M.keys env
                  key'     = F.find (S.member (regionToVar r)) keyList'
                  in case key' of
                    Nothing -> do
                      let key'' = S.singleton loc
                          val' = [DelayLoc loc phs]
                          env' = M.insert key'' val' env
                       in placeRegionInwards env' scopeSet rhs 
                    Just myKey -> do 
                      let valList  = M.findWithDefault [] myKey env 
                          myKey'   = S.insert loc myKey
                          valList' = valList ++ [DelayLoc loc phs]
                          tempDict = M.delete myKey env
                          newEnv   = M.insert myKey' valList' tempDict
                          in placeRegionInwards newEnv scopeSet rhs         --recurse on rhs using the newenv
            
            AfterConstantLE _ loc' -> do                                    --In case statement, actual match = AfterConstantLE integralVal loc'
              let keyList' = M.keys env
                  key'     = F.find (S.member loc') keyList'
                  in case key' of
                    Nothing -> do
                      let key'' = S.singleton loc
                          val' = [DelayLoc loc phs]
                          env' = M.insert key'' val' env
                        in placeRegionInwards env' scopeSet rhs 
                    Just myKey -> do
                      let valList  = M.findWithDefault [] myKey env
                          myKey'   = S.insert loc myKey
                          valList' = valList ++ [DelayLoc loc phs]
                          tempDict = M.delete myKey env
                          newEnv   = M.insert myKey' valList' tempDict
                          in placeRegionInwards newEnv scopeSet rhs 
                          
            AfterVariableLE _ loc' _ -> do                                  --In case statement, actual match = AfterVariableLE variable loc' boolVal
              let keyList' = M.keys env
                  key'     = F.find (S.member loc') keyList'
                  in case key' of
                    Nothing -> do
                        let key'' = S.singleton loc
                            val' = [DelayLoc loc phs]
                            env' = M.insert key'' val' env
                         in placeRegionInwards env' scopeSet rhs 
                    Just myKey -> do
                      let valList  = M.findWithDefault [] myKey env
                          myKey'   = S.insert loc myKey
                          valList' = valList ++ [DelayLoc loc phs]
                          tempDict = M.delete myKey env
                          newEnv   = M.insert myKey' valList' tempDict
                          in placeRegionInwards newEnv scopeSet rhs 

            InRegionLE r -> do
              let keyList' = M.keys env
                  key'     = F.find (S.member (regionToVar r) ) keyList'
                  in case key' of
                    Nothing -> error "No existing region found for this Location in case InRegionLE"
                    Just myKey -> do 
                      let valList  = M.findWithDefault [] myKey env 
                          myKey'   = S.insert loc myKey
                          valList' = valList ++ [DelayLoc loc phs]
                          tempDict = M.delete myKey env
                          newEnv   = M.insert myKey' valList' tempDict
                          in placeRegionInwards newEnv scopeSet rhs 

            FromEndLE loc' -> do
              let keyList' = M.keys env
                  key'     = F.find (S.member loc') keyList'
                  in case key' of
                    Nothing -> error "No existing variable found for this Location in case FromEndLE"
                    Just myKey -> do
                      let valList  = M.findWithDefault [] myKey env
                          myKey'   = S.insert loc myKey
                          valList' = valList ++ [DelayLoc loc phs]
                          tempDict = M.delete myKey env
                          newEnv   = M.insert myKey' valList' tempDict
                          in placeRegionInwards newEnv scopeSet rhs 

            FreeLE -> error "Free LE not implemented yet!"                       --For FreeLE we need to figure out how to handle this
         
        LetParRegionE r rhs -> do                                                --Handle a parallel LetRegion
          let key' = S.singleton (regionToVar r)
              val' = [DelayParRegion r]
              env' = M.insert key' val' env
              in placeRegionInwards env' scopeSet rhs 


        RetE locList _                                  -> do  {-Actual type is RetE locList variable, here you can't just return the expression, look at the locList to see which variables are alive in env and codegen them before-}
                                                            let (_, ex') = dischargeBinds' env (S.fromList locList) ex
                                                              in return ex'

        FromEndE _                                     -> return ex   {-Actual type is FromEndE loc, Don't need to worry about it will appear later in the pipeline, Just return the expression-}
        BoundsCheck{}                                  -> return ex {-Actual type is BoundsCheck integer l1 l2, Don't need to worry about it will appear later in the pipeline, Just return the expression-}
        AddFixed{}                                     -> return ex {-Actual type is AddFixed variable integer, Return the expression-}
        IndirectionE{}                                 -> return ex {-Actual type: IndirectionE tyCon dataCon (l1,v1) (l2,v2) rhs, skip the recursion, IndirectionE doesn't appear until later in the IR language, return the expression-}
        GetCilkWorkerNum                               -> return ex {-Just return the expression, there is no recusrion to do here-}
        LetAvail vs e                                  -> Ext . LetAvail vs <$> go e  {-Recurse on the rhs directly-}
        

     -- Straightforward recursion ...
    VarE{}                 -> return ex --Just return Nothing special here 
    LitE{}                 -> return ex --Just return Nothing special here 
    FloatE{}               -> return ex --Just return Nothing special here 
    LitSymE{}              -> return ex --Just return Nothing special here 
    AppE{}                 -> return ex --Just return Nothing special here 
    PrimAppE{}             -> return ex --Just return Nothing special here 

    DataConE loc _ _       -> let (d, ex') = dischargeBinds' env (S.singleton loc) ex {-DataConE loc dataCons args, Check if loc is there in env key.If its there then codegen before recursing on the args-}
                                in case ex' of
                                  DataConE loc' dataCons' args' -> do 
                                    args'' <- mapM (placeRegionInwards d scopeSet) args'
                                    return $ DataConE loc' dataCons' args''
                                  _ ->  do placeRegionInwards d scopeSet ex' {-I think it would be best to just recurse on this "new" expression again, how to prevent an infinite loop?-}

    ProjE i e              -> ProjE i <$> go e {-Simple recursion on e-}

    IfE a b c              -> do  {-Check if there are freeVariables in the condition a, if the set has any freeVars from "a" then codegen all the locations and regions before the IfE-}
                                     let (d, a') = dischargeBinds env scopeSet a  --If there are freeVariables in a then codgen bindings for those in a
                                     b' <- placeRegionInwards d scopeSet b        --Recurse on b (Then part) 
                                     c' <- placeRegionInwards d scopeSet c        --Recurse on c (Else part)
                                     return $ IfE a' b' c'                        --Return the new IfE expression

    MkProdE ls                    -> MkProdE <$> mapM go ls {-Recurse over all expression in the tuple in the expression ls-}
    LetE (v,locs,ty,rhs) bod      -> do {-The locs will be empty at this point, so just update scope set and recurse-}
                                     let newScope = S.insert v scopeSet
                                      in LetE . (v,locs,ty,) <$> placeRegionInwards env newScope rhs <*> placeRegionInwards env newScope bod  
    CaseE scrt brs                -> do      
      brs' <- mapM 
        (\(a,b,c) -> do let varList = fmap fst b                             --Get all the variables from the tuple list
                            newScope = scopeSet `S.union` S.fromList varList -- Make the newScope set by unioning the old one with the varList
                        let (d, c') = dischargeBinds env newScope c          -- Discharge the binds using the newScope and the dictionary
                        c'' <- placeRegionInwards d scopeSet c'
                        dbgTraceIt (sdoc (c,c')) (return (a,b,c''))) brs
      return $ CaseE scrt brs'
    TimeIt e ty b                 -> do
      e' <- go e
      return $ TimeIt e' ty b
    SpawnE{}                      -> pure ex   
    SyncE{}                       -> pure ex
    WithArenaE v e                -> WithArenaE v <$> go e
    MapE{}                        -> return ex --Is there a recursion element to this?
    FoldE{}                       -> return ex --Is there a recursion element to this?
  where 
    go = placeRegionInwards env scopeSet

    --Cases for which checking the local variables might be important
    --AppE
    --DataConE
    --IfE
    --LetE
    --CaseE
    --SpawnE

{-

foo x y = x

bar x y = y

initial inScope for main expression = (names of all top level functions)
initial inScope for functions = (names of all top level functions) + (arguments to functions)

-- lots of passes track Env2

-- allFreeVars :: Exp2 -> [Var]

main =
  let a = foo 1 2 in -- inScope = {foo, bar}
  let b = bar 2 3 in -- inScope = {foo, bar, a}
  (a,b)              -- inScope = {foo, bar, a, b}

letregion r bod -> inScope doesn't grow here

grow for:
- let expressions
- bound variables in pattern matches for case

-}

--This is a function to discharge binds given a dictionary, scopeSet and expression where free variables might exist
dischargeBinds :: DelayedBindEnv -> S.Set Var -> Exp2 -> (DelayedBindEnv, Exp2)
dischargeBinds env scopeSet exp2 =
  let free_vars = S.difference (S.fromList $ allFreeVars exp2) scopeSet   --Take the difference of the scopeSet with the set that freeVar gives.
  in codeGen free_vars env exp2

--This is a duplicate function to the one above but instead it takes a Set of LocVar to codeGen directly instead of the expression and scopeSet.
dischargeBinds' :: DelayedBindEnv -> S.Set LocVar -> Exp2 -> (DelayedBindEnv, Exp2)
dischargeBinds' env free_vars exp2 = do codeGen free_vars env exp2  

{-

--Use this function to recursively create Maybe Exp2, it terminates when you reach [] or the variable you are doing codeGen for
buildExp2 :: [DelayedBind] -> LocVar -> Bool -> (Maybe Exp2 , Bool)
buildExp2 valList var terminate = case (valList, var, terminate) of
  ([], _ , _)              -> (Nothing, True)
  (_ , _ , True)           -> (Nothing, True) 
  (x : xs, locVar, False)  -> case x of
    DelayRegion region     -> if region == locVar 
                                then LetRegionE region (buildExp2 xs locVar True)
                                else LetRegionE region (buildExp2 xs locVar False)
    DelayLoc variable locExp -> if variable == locVar 
                                then LetLocE variable locExp (buildExp2 xs locVar True)
                                else LetLocE variable locExp (buildExp2 xs locVar False)
    DelayParRegion region  -> if region == locVar 
                                then LetParRegionE region (buildExp2 xs locVar True)
                                else LetParRegionE region (buildExp2 xs locVar False) 
                                
-}

-- Use this function to codegen from the env by giving a set of variables you want to codegen from
codeGen :: S.Set LocVar -> DelayedBindEnv -> Exp2 -> (DelayedBindEnv, Exp2)
codeGen set env body =
  let allKeys  =  M.keys env --List of all keys from env
      keyList  = map (\variable -> F.find (S.member variable) allKeys ) (toList set)  -- For each var in the input set find its corresponding key
      keyList' = S.catMaybes keyList                                                  -- Filter out all the Nothing values from the list and let only Just values in the list
      valList  = concatMap (\key -> M.findWithDefault [] key env) keyList'            -- For each key in the keyList from before find the value associated with the key
      newEnv   = fmap (`M.delete` env) keyList'                                       -- Delete all the keys for which we are about to codegen from the env
      newEnv' = case newEnv of 
        [] -> M.empty 
        _  -> L.last newEnv 
      exps     = foldr bindDelayedBind body valList                                   -- Get all the bindings for all the expressions in the key  
   in dbgTraceIt (sdoc (set,env,body)) (newEnv', exps)                                -- This was for printing : dbgTraceIt (sdoc (set,env,body))

bindDelayedBind :: DelayedBind -> Exp2 -> Exp2
bindDelayedBind delayed body =
  case delayed of
    DelayRegion r -> Ext $ LetRegionE r body
    DelayParRegion r -> Ext $ LetParRegionE r body
    DelayLoc loc locexp -> Ext $ LetLocE loc locexp body

-- convertFromMaybe :: Maybe a -> a
-- convertFromMaybe maybeA = case maybeA of
--   Nothing -> null
--   Just a  -> a 
    