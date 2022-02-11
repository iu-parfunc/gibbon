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
                 | DelayLoc LocVar LocExp

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

-- getListfromTupleList :: [(a, b)] -> [a]
-- getListfromTupleList list1 = case list1 of 
--   [] -> []
--   ((a,b):xs) -> a : getListfromTupleList xs

{-This function should be recursive, it should eat the expression given to it and recurse on the next somehow?
  It should also store the letregion and letlocation in the dictionary.-}
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
            
            AfterConstantLE integerVal loc' -> error "Not implemented yet!" 
            _ -> error "Not implemented yet!"

            -- AfterVariableLE variable loc' boolVal -> error "Not implemented yet!" 
            -- InRegionLE r -> error "Not implemented yet!" 
            -- FreeLE -> error "Not implemented yet!" 
            -- FromEndE loc -> error "Not implemented yet!" 

          -- let dictionary' = M.insert loc (LetLocE loc phs) dictionary
          --                       let (rhs', cannotRemove) = optimizeExp dictionary' rhs
          --                       if cannotRemove then return (LetLocE loc phs rhs', True) else return (rhs', False)
                                {- Store this let location with the corresponding location in a Return a null for this match?   
                                 What to do about phs and rhs?-}
        -- LetParRegionE r rhs -> do
        --                         let dictionary' = M.insert (regionToVar r) (LetParRegionE r) dictionary
        --                         let (rhs', cannotRemove) = placeRegionInwards dictionary' rhs
        --                         if cannotRemove then return (LetParRegion r rhs') else return (rhs', False)

        -- RetE{}                                   -> return (ex, False)
        
        -- FromEndE{}                               -> return ex
        
        -- BoundsCheck{}                            -> return ex

        -- IndirectionE tc dc (l1,v1) (l2,v2) rhs   -> do
        --                                             (rhs', cannotRemove) <- placeRegionInwards dictionary rhs
        --                                             let newExpression' = IndirectionE tc dc (l1,v1) (l2,v2) rhs'
        --                                             return (newExpression', cannotRemove)
        --                                             -- I think the variable cannotRemove doesn't matter here
        
        --if there is an instance of the case expression
        -- CaseE e mp -> do
        --                variablesFree <- freeVars ext
        --                let variablesFreeList = toList variablesFree 

        _ -> return ex

        -- GetCilkWorkerNum    -> return ex

        

    _ -> return ex