{-# LANGUAGE BlockArguments #-}

module Gibbon.Passes.RegionsInwards (regionsInwards) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Foldable as F

import Gibbon.Common
import Gibbon.L2.Syntax

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
          let keySet   = S.empty
              bindings = []
              dict     = M.singleton keySet bindings
              in Just <$> (,ty) <$> placeRegionInwards dict mn

    --returns the new  ddefs, fundefs and mainExpressions
    return $ Prog ddefs fundefs' mainExp'



placeRegionsInwardsFunBody :: FunDef2 -> PassM FunDef2
placeRegionsInwardsFunBody f@FunDef{funBody} = do
  let keySet   = S.empty 
      bindings = []
      dict     = M.singleton keySet bindings
  (funBody', varList) <- placeRegionInwards dict funBody
  return $ f {funBody = funBody'}

-- Recursive funtion that will move the regions inwards
placeRegionInwards :: DelayedBindEnv-> Exp2 -> (PassM Exp2, [LocVar])
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

            FreeLE -> error "How do we handle a Free LE" 
        
        --Handle a parallel LetRegion ? 
        LetParRegionE r rhs -> do
          let key' = S.singleton (regionToVar r)
              val' = [DelayParRegion r]
              dictionary' = M.insert key' val' dictionary
              in placeRegionInwards dictionary' rhs


        RetE locList variable                          -> return ex --Does this need to handle anything special?
        FromEndE loc                                   -> return ex --What do we do about this case, what is FromEndE loc?
        BoundsCheck integer l1 l2                      -> return ex --Does this need to handle anything special?
        AddFixed variable integer                      -> return ex --Does this need to handle anything special?
        IndirectionE tyCon dataCon (l1,v1) (l2,v2) rhs -> placeRegionInwards dictionary rhs --This will Recurse on the rhs directly, any special handling here?
        GetCilkWorkerNum                               -> return ex --Nothing special to handle here
        LetAvail varList rhs                           -> placeRegionInwards dictionary rhs --This will Recurse on the rhs directly, any special handling here?
        

     -- Straightforward recursion ...
    VarE{}                        -> return ex --Just return Nothing special here? 
    LitE{}                        -> return ex --Just return Nothing special here? 
    FloatE{}                      -> return ex --Just return Nothing special here? 
    LitSymE{}                     -> return ex --Just return Nothing special here? 
    AppE{}                        -> return ex --Just return Nothing special here? 
    PrimAppE{}                    -> return ex --Just return Nothing special here? 
    DataConE{}                    -> error "I am not sure that this means here, what is DataConE?"
    ProjE i e                     -> error "What is ProjE?"
    IfE a b c                     -> error "This needs to be implemented, here we would move the regions inwards or not"
    MkProdE ls                    -> error "What is MkProdE"
    LetE (v,locs,ty,rhs) bod      -> error "Don't really know what this is?"
    CaseE scrt mp                 -> error "For CaseE statements we would need to check for the free variable and decide to move the regions inwards or not"
    TimeIt e ty b                 -> error "What is TimeIt"
    SpawnE{}                      -> return ex --I think we can just return the expression for this
    SyncE{}                       -> return ex --I think we can just return the expression for this
    WithArenaE v e                -> error "Not sure what WithArena is"
    MapE{}                        -> return ex --Is there a recursion element to this?
    FoldE{}                       -> return ex --Is there a recursion element to this?