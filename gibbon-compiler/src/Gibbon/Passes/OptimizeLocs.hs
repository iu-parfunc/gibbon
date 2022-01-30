
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
          LetRegionE r rhs -> let dictionary' = M.insert r ext dictionary
                                  (rhs', cannotRemove) = optimizeExp dictionary' rhs
                                  in if cannotRemove then (LetRegionE r rhs', True) else (rhs', False) {- Store this let region with corresponding r in a dictionary? Return Null for this match?What to do about rhs? -} 
          LetLocE loc phs rhs -> return ex  {- Store this let location with the corresponding location in a
                                               Return a null for this match?   
                                               What to do about phs and rhs?-}
          _ -> return ex                    {- Are there any other expressions that I need to take care of?-} 
    _ -> return ex


storeLocation :: Exp2 -> DelayedBindEnv -> DelayedBindEnv
storeLocation l2expression dictionary = 
    case l2expression of
        Ext ext -> 
            case ext of 
                LetRegionE r rhs -> dictionary.insert r l2expression dictionary

                LetLocE loc phs rhs -> dictionary.insert loc l2expression dictionary

                _ -> dictionary
        _ -> dictionary
