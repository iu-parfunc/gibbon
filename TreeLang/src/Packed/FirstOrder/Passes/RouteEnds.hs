{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | A pass to route end-witnesses as additional function returns.

module Packed.FirstOrder.Passes.RouteEnds 
    ( routeEnds ) where

import           Packed.FirstOrder.Common hiding (FunDef)
import qualified Packed.FirstOrder.L1_Source as L1
import qualified Packed.FirstOrder.LTraverse as L2

-- We use some pieces from this other attempt:
import           Packed.FirstOrder.LTraverse as L2
import           Packed.FirstOrder.Passes.Cursorize2 (cursorizeTy)
-- import           Packed.FirstOrder.Passes.InferEffects ()
import Data.List as L hiding (tail)
import Data.Map as M
import Text.PrettyPrint.GenericPretty

-- | Chatter level for this module:
lvl :: Int
lvl = 5


-- =============================================================================

witnessBinding :: forall t. t
witnessBinding = undefined

    
-- | The goal of this pass is to take effect signatures and translate
-- them into extra arguments and returns.  This pass does not worry
-- about where the witnesses come from to synthesize these extra
-- returns, it just inserts references to them that create demand.
-- 
-- 
routeEnds :: L2.Prog -> SyM L2.Prog
routeEnds L2.Prog{ddefs,fundefs,mainExp} = -- ddefs, fundefs
    dbgTrace lvl ("Starting routeEnds on "++show(doc fundefs)) $ do
    -- Prog emptyDD <$> mapM fd fundefs <*> pure Nothing

    fds' <- mapM fd $ M.elems fundefs

    -- let gloc = "global"
    mn <- case mainExp of
            Nothing -> return Nothing
            Just (x,t)  -> Just . (,t) <$> tail [] (M.empty) x
                -- do let initWenv = WE (M.singleton gloc (L1.VarE "gcurs")) M.empty
                --    tl' <- tail [] (M.empty,initWenv) x
                --    return $ Just $ L1.LetE ("gcurs", CursorTy, NewBuffer) tl'
    return L2.Prog{ fundefs = M.fromList $ L.map (\f -> (L2.funname f,f)) fds'
                  , ddefs = ddefs
                  , mainExp = mn
                  }
 where
   
  fd :: L2.FunDef -> SyM L2.FunDef
  fd (f@L2.FunDef{funname,funty,funarg,funbod}) =
      let (newTy@(ArrowTy inT _ _outT),newIn,newOut) = cursorizeTy funty in
      dbgTrace lvl ("Processing fundef: "++show(doc f)++"\n  new type: "++sdoc newTy) $
   do
      fresh <- gensym "tupin"
      let argLoc  = argtyToLoc (L2.mangle newArg) inT
          (newArg, bod, wenv) =
              if newIn == [] -- No injected cursor params..
              then (funarg, funbod, witnessBinding newArg argLoc)
              else ( fresh
                     -- We could introduce a let binding, but here we
                     -- just substitute instead:
                   -- , L1.subst funarg (L1.projNonFirst (length newIn) (L1.VarE fresh))
                   --            funbod
                   , LetE (funarg, fmap (const ()) inT,
                           (L1.projNonFirst (length newIn) (L1.VarE fresh)))
                          funbod
                   , witnessBinding fresh
                     (TupLoc $ L.map Fixed newIn ++ [argLoc]))
      let env = __env0 -- M.singleton newArg (L2.stripTyLocs inT, argLoc)
      exp' <- tail newOut env bod
      return $ L2.FunDef funname newTy newArg exp'

  tail :: [LocVar] -> Env -> L1.Exp -> SyM L1.Exp
  tail demanded env ex =
    let go = tail demanded env in
    case ex of 
      VarE v   -> pure$ VarE v
      LitE n   -> pure$ LitE n
      AppE v e -> AppE v <$> go e
      PrimAppE p ls      -> PrimAppE p <$> mapM go ls
      LetE (v,t,rhs) bod -> LetE <$> ((v,t,) <$> go rhs) <*> go bod
      ProjE i e      -> ProjE i <$> go e
      CaseE e ls     -> CaseE <$> go e <*> sequence
                           [ (k,vs,) <$> go e | (k,vs,e) <- ls ]
      MkProdE ls     -> MkProdE <$> mapM go ls
      MkPackedE k ls -> MkPackedE k <$> mapM go ls
      TimeIt e t     -> TimeIt <$> go e <*> pure t
      IfE a b c      -> IfE <$> go a <*> go b <*> go c
      -- MapE (v,t,rhs) bod -> MapE <$> ((v,t,) <$> go rhs) <*> go bod
      -- FoldE (v1,t1,r1) (v2,t2,r2) bod ->
      --     FoldE <$> ((v1,t1,) <$> go r1)
      --           <*> ((v2,t2,) <$> go r2)
      --           <*> go bod
