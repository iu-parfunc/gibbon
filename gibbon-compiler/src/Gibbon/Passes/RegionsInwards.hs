{-# LANGUAGE BlockArguments #-}

module Gibbon.Passes.RegionsInwards where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Foldable as F

import Gibbon.Common
import Gibbon.L2.Syntax
import Data.SCargot.Repr.Basic (_car)
import Data.IntMap (insertWith)
import qualified Data.IntMap as M.Lazy
import qualified Data.Vector.Internal.Check as M.Map

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

            FreeLE -> error "How do we handle a Free LE" 

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

        LetParRegionE r rhs -> do
          let key' = S.singleton (regionToVar r)
              val' = [DelayParRegion r]
              dictionary' = M.insert key' val' dictionary
              in placeRegionInwards dictionary' rhs


        RetE locList variable                          -> error "Haven't implemented this yet"
        FromEndE loc                                   -> error "Right now this is unimplemented, I guess we don't need this case afterall?"
        BoundsCheck integer l1 l2                      -> error "Did not implement this yet"
        AddFixed variable integer                      -> error "Did not implement this yet"
        IndirectionE tyCon dataCon (l1,v1) (l2,v2) rhs -> error "Did not implement this yet" 
        GetCilkWorkerNum                               -> error "Did not implement this yet"
        LetAvail varList rhs                           -> error "Did not implement this yet" 
        

     -- Straightforward recursion ...
    VarE{}                        -> error "Did not implement this yet"
    LitE{}                        -> error "Did not implement this yet"
    FloatE{}                      -> error "Did not implement this yet"
    LitSymE{}                     -> error "Did not implement this yet"
    AppE{}                        -> error "Did not implement this yet"
    PrimAppE{}                    -> error "Did not implement this yet"
    DataConE{}                    -> error "Did not implement this yet"
    ProjE i e                     -> error "Did not implement this yet"
    IfE a b c                     -> error "Did not implement this yet"
    MkProdE ls                    -> error "Did not implement this yet"
    LetE (v,locs,ty,rhs) bod      -> error "Did not implement this yet"
    CaseE scrt mp                 -> error "Did not implement this yet"
    TimeIt e ty b                 -> error "Did not implement this yet"
    SpawnE{}                      -> error "Did not implement this yet"
    SyncE{}                       -> error "Did not implement this yet"
    WithArenaE v e                -> error "Did not implement this yet"
    MapE{}                        -> error "Did not implement this yet"
    FoldE{}                       -> error "Did not implement this yet"