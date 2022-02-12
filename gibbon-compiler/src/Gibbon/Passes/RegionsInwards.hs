{-# LANGUAGE BlockArguments #-}

module Gibbon.Passes.RegionsInwards (regionsInwards) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Foldable as F

import Gibbon.Common
import Gibbon.L2.Syntax

import Gibbon.LocExp

--define data type that can be Region, Loc, LocExp
data DelayedBind = DelayRegion Region
                 | DelayLoc LocVar LocExp | DelayParRegion Region

--define a Map from set to the DelayedBind data type
type DelayedBindEnv = M.Map (S.Set LocVar) [DelayedBind]


regionsInwards :: Prog2 -> PassM Prog2
regionsInwards Prog{ddefs,fundefs,mainExp} = do
    fds' <- mapM placeRegionsInwardsFunBody $ M.elems fundefs
    let fundefs' = M.fromList $ map (\f -> (funName f,f)) fds'
    mainExp' <- case mainExp of
        Nothing -> return Nothing
        Just (mn, ty)-> do 
          let dict = M.empty      -- Use M.empty for creating the empty dictionary
              in Just <$> (,ty) <$> placeRegionInwards dict mn

    --returns the new  ddefs, fundefs and mainExpressions
    return $ Prog ddefs fundefs' mainExp'



placeRegionsInwardsFunBody :: FunDef2 -> PassM FunDef2
placeRegionsInwardsFunBody f@FunDef{funBody} = do
  let dict = M.empty
  funBody' <- placeRegionInwards dict funBody
  return $ f {funBody = funBody'}

-- Recursive funtion that will move the regions inwards
placeRegionInwards :: DelayedBindEnv-> Exp2 -> PassM Exp2
placeRegionInwards dictionary ex =
  case ex of
    Ext ext ->
      case ext of
        --take care of regions
        LetRegionE r rhs -> do
          let key' = S.singleton (regionToVar r)
              val' = [DelayRegion r]
              dictionary' = M.insert key' val' dictionary
              in placeRegionInwards dictionary' rhs

        --take care of locations      
        LetLocE loc phs rhs -> do
          case phs of
            StartOfLE r -> do
              let keyList' = M.keys dictionary
                  key'     = F.find (\a-> (S.member (regionToVar r) a)) keyList'
                  in case key' of
                    Nothing -> error "No region found for this Location"
                    Just myKey -> do 
                      let valList  = M.findWithDefault [] myKey dictionary 
                          myKey'   = S.insert loc myKey
                          valList' = valList ++ [DelayLoc loc phs]
                          tempDict = M.delete myKey dictionary
                          newDict  = M.insert myKey' valList' tempDict
                          --Recurse on rhs using the newDictionary
                          in placeRegionInwards newDict rhs
            
            AfterConstantLE integerVal loc' -> do
              let keyList' = M.keys dictionary
                  key'     = F.find (\a-> (S.member loc' a)) keyList'
                  in case key' of
                    Nothing -> error "No existing variable found for this Location"
                    Just myKey -> do
                      let valList  = M.findWithDefault [] myKey dictionary
                          myKey'   = S.insert loc myKey
                          valList' = valList ++ [DelayLoc loc phs]
                          tempDict = M.delete myKey dictionary
                          newDict  = M.insert myKey' valList' tempDict
                          in placeRegionInwards newDict rhs
                          
            AfterVariableLE variable loc' boolVal -> do
              let keyList' = M.keys dictionary
                  key'     = F.find (\a-> (S.member loc' a)) keyList'
                  in case key' of
                    Nothing -> error "No existing variable found for this Location"
                    Just myKey -> do
                      let valList  = M.findWithDefault [] myKey dictionary
                          myKey'   = S.insert loc myKey
                          valList' = valList ++ [DelayLoc loc phs]
                          tempDict = M.delete myKey dictionary
                          newDict  = M.insert myKey' valList' tempDict
                          in placeRegionInwards newDict rhs

            InRegionLE r -> do
              let keyList' = M.keys dictionary
                  key'     = F.find (\a-> (S.member (regionToVar r) a)) keyList'
                  in case key' of
                    Nothing -> error "No region found for this Location"
                    Just myKey -> do 
                      let valList  = M.findWithDefault [] myKey dictionary 
                          myKey'   = S.insert loc myKey
                          valList' = valList ++ [DelayLoc loc phs]
                          tempDict = M.delete myKey dictionary
                          newDict  = M.insert myKey' valList' tempDict
                          --Recurse on rhs using the newDictionary
                          in placeRegionInwards newDict rhs

            FromEndLE loc' -> do
              let keyList' = M.keys dictionary
                  key'     = F.find (\a -> (S.member loc' a)) keyList'
                  in case key' of
                    Nothing -> error "No existing variable found for this Location"
                    Just myKey -> do
                      let valList  = M.findWithDefault [] myKey dictionary
                          myKey'   = S.insert loc myKey
                          valList' = valList ++ [DelayLoc loc phs]
                          tempDict = M.delete myKey dictionary
                          newDict  = M.insert myKey' valList' tempDict
                          in placeRegionInwards newDict rhs

            FreeLE -> error "How do we handle a Free LE" --For FreeLE we need to figure out how to handle this
        
        --Handle a parallel LetRegion 
        LetParRegionE r rhs -> do
          let key' = S.singleton (regionToVar r)
              val' = [DelayParRegion r]
              dictionary' = M.insert key' val' dictionary
              in placeRegionInwards dictionary' rhs


        RetE locList variable                          -> return ex {-Here you can't just return the expression, you will need to look at the locList to see 
                                                                      which variables are in the dictionary which are alive (meaning codegen for them hasn't happened yet) 
                                                                      if they are not codegen yet we will just write them out before-}

        FromEndE loc                                   -> return ex {-Don't need to worry about it will appear later in the pipeline, Just return the expression-}

        BoundsCheck integer l1 l2                      -> return ex {-Don't need to worry about it will appear later in the pipeline, Just return the expression-}
        AddFixed variable integer                      -> return ex {-Return the expression-}
        IndirectionE tyCon dataCon (l1,v1) (l2,v2) rhs -> return ex {-We skip the recursion, the IndirectionE does not appear until later in the IR language, only return the expression-}
        GetCilkWorkerNum                               -> return ex {-Just return the expression, there is no recusrion to do here-}

        LetAvail vs e                           -> Ext <$> LetAvail vs <$> go e {-This will Recurse on the rhs directly-}
        

     -- Straightforward recursion ...
    VarE{}                        -> return ex --Just return Nothing special here 
    LitE{}                        -> return ex --Just return Nothing special here 
    FloatE{}                      -> return ex --Just return Nothing special here 
    LitSymE{}                     -> return ex --Just return Nothing special here 
    AppE{}                        -> return ex --Just return Nothing special here 
    PrimAppE{}                    -> return ex --Just return Nothing special here 
    DataConE loc dataCons args    -> _ {-Can't just return the expression here, We would need to check if the loc is 
                                                 there in any key in the dictionary, if its there then codegen and recuse on  
                                                 the args-}
    ProjE i e                     -> ProjE i <$> go e {-Simple recursion on e-}
    IfE a b c                     -> do  {-Check if there are freeVariales in the condition a, if the set has any freeVars from "a" then codegen all the locations and regions before the IfE-}
                                     let freeSet = freeVars a
                                          in case freeSet of
                                            Nothing      -> (IfE a) <$> go b <*> go c
                                            Just freeSet -> do
                                              

        
    MkProdE ls                    -> MkProdE <$> mapM go ls {-Recurse over all expression in the tuple in the expression ls-}
    LetE (v,locs,ty,rhs) bod      -> _ {-Need to check the locs in the keys of the set before recursing-}
    CaseE scrt mp                 -> error "For CaseE statements we would need to check for the free variable and decide to move the regions inwards or not"
    TimeIt e ty b                 -> do
      e' <- go e
      return $ TimeIt e' ty b
    SpawnE{}                      -> pure ex   
    SyncE{}                       -> pure ex
    WithArenaE v e                -> WithArenaE v <$> go e
    MapE{}                        -> return ex --Is there a recursion element to this?
    FoldE{}                       -> return ex --Is there a recursion element to this?
  where 
    go = placeRegionInwards dictionary

    --Cases for which checking the local variables might be important
    --AppE
    --DataConE
    --IfE
    --LetE
    --CaseE
    --SpawnE


codeGen :: [LocVar] -> DelayedBindEnv -> Exp2
codeGen list dict = do 
  let keyList' = M.keys dictionary
    in case list of
      []     -> Nothing
      (x:xs) -> do
        let key' = F.find (\a -> (S.member x a)) keyList'
            in case key' of 
              Nothing -> error "Could not find any existing variables"
              Just key' -> 