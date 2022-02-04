{-# LANGUAGE BlockArguments #-}

module Gibbon.Passes.OptimizeLocs where

import qualified Data.Map as M
import qualified Data.Set as S

import Gibbon.Common
import Gibbon.L2.Syntax
import Data.SCargot.Repr.Basic (_car)
import Data.IntMap (insertWith)

--define data type that can be Region, Loc, LocExp
data DelayedBind = DelayRegion Region
                 | DelayLoc LocVar LocExp

--define a Map from set to the DelayedBind data type
type DelayedBindEnv = M.Map (S.Set LocVar) [DelayedBind]


optimizeLocs :: Prog2 -> PassM Prog2
optimizeLocs Prog{ddefs,fundefs,mainExp} = do
    fds' <- mapM optimizeFunBody $ M.elems fundefs
    let fundefs' = M.fromList $ map (\f -> (funName f,f)) fds'
    mainExp' <- case mainExp of
        Nothing -> return Nothing
        Just (mn, ty)-> Just <$> (,ty) <$> optimizeExp mn

    --returns the new  ddefs, fundefs and mainExpressions
    return $ Prog ddefs fundefs' mainExp'



optimizeFunBody :: FunDef2 -> PassM FunDef2
optimizeFunBody f@FunDef{funBody} = do
  return $ f {funBody}


{-This function should be recursive, it should eat the expression given to it and recurse on the next somehow?
  It should also store the letregion and letlocation in the dictionary.-}
optimizeExp :: DelayedBindEnv-> Exp2 -> (PassM Exp2, Bool)
optimizeExp dictionary ex =
    case ex of
    Ext ext ->
      case ext of
        LetRegionE r rhs -> do
                                let dictionary' = M.insert (regionToVar r) (LetRegionE r) dictionary
                                let (rhs', cannotRemove) = optimizeExp dictionary' rhs
                                if cannotRemove then return (LetRegionE r rhs', True) else return (rhs', False)
                                {- Store this let region with corresponding r in a dictionary?
                                Return Null for this match?What to do about rhs? -} 
        LetLocE loc phs rhs -> do
                                let dictionary' = M.insert loc (LetLocE loc phs) dictionary
                                let (rhs', cannotRemove) = optimizeExp dictionary' rhs
                                if cannotRemove then return (LetLocE loc phs rhs', True) else return (rhs', False)
                                {- Store this let location with the corresponding location in a Return a null for this match?   
                                 What to do about phs and rhs?-}
        LetParRegionE r rhs -> do
                                let dictionary' = M.insert (regionToVar r) (LetParRegionE r) dictionary
                                let (rhs', cannotRemove) = optimizeExp dictionary' rhs
                                if cannotRemove then return (LetParRegion r rhs') else return (rhs', False)

        RetE{}                                   -> return (ex, False)
        
        FromEndE{}                               -> return ex
        
        BoundsCheck{}                            -> return ex

        IndirectionE tc dc (l1,v1) (l2,v2) rhs   -> do
                                                    (rhs', cannotRemove) <- optimizeExp dictionary rhs
                                                    let newExpression' = IndirectionE tc dc (l1,v1) (l2,v2) rhs'
                                                    return (newExpression', cannotRemove)
                                                    -- I think the variable cannotRemove doesn't matter here
        
        --if there is an instance of the case expression
        CaseE e mp -> do
                       variablesFree <- freeVars ext
                       let variablesFreeList = toList variablesFree 

        _ -> return ex

        GetCilkWorkerNum    -> return ex

        

    _ -> return ex