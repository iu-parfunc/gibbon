{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

-- | Strategy 2, not in use atm.

module Packed.FirstOrder.Passes.Cursorize2
    ( cursorize
    , cursorizeTy
    ) where

import Control.Monad
import Control.DeepSeq
import Packed.FirstOrder.Common hiding (FunDef)
import qualified Packed.FirstOrder.L1.Syntax as L1
import qualified Packed.FirstOrder.L2.Syntax as L2
import           Packed.FirstOrder.L1.Syntax (UrTy(..),pattern SymTy)
import           Packed.FirstOrder.L2.Syntax
    (argtyToLoc, Loc(..), ArrowTy(..), toEndVar,
     FunDef(..), Prog(..), Exp(..))
import Data.Maybe
import Data.List as L hiding (tail)
import Data.Set as S
import Data.Map as M
import Text.PrettyPrint.GenericPretty
import Debug.Trace

-- | Chatter level for this module:
lvl :: Int
lvl = 5

--------------------------------------------------------------------------------
-- STRATEGY TWO - see the future (dataflow analysis)
--------------------------------------------------------------------------------

-- The idea here is to have enough type information at the point of a
-- call to know *which* output cursors apply, based on the downstream
-- usage of the value returned by the functions.


-- Some general types and utilities:
-- =============================================================================

-- | Map every lexical variable in scope to an abstract location.
--   Some variables are cursors, and they are runtime witnesses to
--   particular location variables.
type Env = M.Map Var (L1.Ty,L2.Loc)

-- | This maps an abstract location variable onto an expression that
-- witnesses it.  These can be open terms with free location variables
-- that are not (yet) bound in the `WitnessEnv`
data WitnessEnv = WE { known :: M.Map LocVar L1.Exp
                     -- ^ Known open terms that witness locations.
                     , _open  :: M.Map LocVar Var
                     -- ^ "Holes" for unknown witnesses, and the
                     -- unbound variables that are expected to receive them.
                     }
  deriving (Read,Show,Eq,Ord, Generic, NFData)

instance Out WitnessEnv

--------------------------------------------------------------------------------


-- Injected cursor args go first in input and output:
_prependArgs :: [L2.Ty] -> L2.Ty -> L2.Ty
_prependArgs [] t = t
_prependArgs ls t = ProdTy $ ls ++ [t]

-- | Combines cursorize 1 and 2.  Returns (arrow, newIn, newOut)
cursorizeTy :: ArrowTy L2.Ty -> (ArrowTy L2.Ty, [LocVar], [LocVar])
cursorizeTy arr = (a2,b,c)
 where
  (a1,c) = L2.cursorizeUrTy arr
  (a2,b) = L2.cursorizeTy2 a1

_mkArrowTy :: L2.Ty -> L2.Ty -> ArrowTy L2.Ty
_mkArrowTy x y = ArrowTy x S.empty y

-- | Replace all packed types with something else.
_replacePacked :: L2.Ty -> L2.Ty -> L2.Ty
_replacePacked (t2::L2.Ty) (t::L2.Ty) =
  case t of
    IntTy  -> IntTy
    BoolTy -> BoolTy
    SymTy  -> SymTy
    (ProdTy x)    -> ProdTy $ L.map (_replacePacked t2) x
    (SymDictTy x) -> SymDictTy $ (_replacePacked t2) x
    PackedTy{}    -> t2
    ListTy _ -> error "_replacePacked: FINISHME lists"



-- | Binding a variable to a value at a given (abstract) location can
-- bring multiple witnesses into scope.
witnessBinding :: Var -> Loc -> WitnessEnv
witnessBinding vr loc = WE (M.fromList $ go loc (L1.VarE vr)) M.empty
  where
   go (TupLoc ls) ex =
       concat [ go x (L1.mkProj ix (length ls) ex)
              | (ix,x) <- zip [0..] ls ]
   go (Fresh v) e = [ (v,e) ]
   go (Fixed v) e = [ (v,e) ]
   go Top       _ = []
   go Bottom    _ = []

-- FIXME: We should be able to combine `Loc` and the annotated `Ty`
-- data types....
_witnessTypedBinds :: [(Var,L2.Ty)] -> WitnessEnv
_witnessTypedBinds [] = emptyWEnv
_witnessTypedBinds ((vr,ty):rst) =
    witnessBinding vr (argtyToLoc (L2.mangle vr) ty)  `unionWEnv`
    _witnessTypedBinds rst

-- This is only used for primitives for now
retTyToLoc :: L1.Ty -> Loc
retTyToLoc L1.IntTy  = Bottom
retTyToLoc L1.SymTy  = Bottom
retTyToLoc L1.BoolTy = Bottom
retTyToLoc t = error$ "retTyToLoc: expected only primitive return types, got: "++show t

-- | Combine two witness environments, where each may fill some of the
-- other's remaining open locvars.  Tie the knot.
unionWEnv :: WitnessEnv -> WitnessEnv -> WitnessEnv
unionWEnv (WE k1 o1) (WE k2 o2) = go (M.union k1 k2) -- FIXME: treat intersection
                                     (M.toList o1 ++ M.toList o2)
  where
    go :: M.Map LocVar L1.Exp -> [(LocVar,Var)] -> WitnessEnv
    go acc [] = WE acc M.empty
    go acc ((lv,v):r) =
       case M.lookup lv acc of
         Nothing -> let WE k o = go acc r
                    in  WE k (M.insert lv v o)
         -- Here we get to remove from the open list, and we subst the
         -- solution into the rest of the WEnv entries:
         Just ex -> go (M.map (L1.subst v ex) acc) r

emptyWEnv :: WitnessEnv
emptyWEnv = WE M.empty M.empty

-- | Add a witness GIVEN witnesses of a list of other locations.
addWitness :: LocVar -> [LocVar] -> ([L1.Exp] -> L1.Exp) -> WitnessEnv -> SyM WitnessEnv
addWitness locvar ls fn orig@WE{known} =
    do (es, free) <- unzip <$> mapM lkp ls
       return $ WE (M.singleton locvar (fn es)) (M.fromList (catMaybes free))
                  `unionWEnv` orig
  where
   lkp lvr = case M.lookup lvr known of
              Just wex -> return (wex,Nothing)
              Nothing -> do tmp <- gensym $ toVar "hl"
                            return (L1.VarE tmp, Just (lvr,tmp))

_extendEnv :: [(Var,(L1.Ty,Loc))] -> Env -> Env
_extendEnv ls e = (M.fromList ls) `M.union` e


--------------------------------------------------------------------------------
-- Patterns used in the pass below:
----------------

-- Annotate each type in a let binding with which location it flows to a
-- pattern Ann

-- Insert location annotations on each let-bound variable.  These
-- record facts about the subsequent dataflow of the value, and ultimately
-- determine which output cursors to use.
-- insertLocs

-- pattern MarkCursor e = AppE "MarkCursor" (MkProdE [e])
pattern MarkCursor c e <- AppE (Var "MarkCursor") (AppE c e)
  where MarkCursor c e = AppE (toVar "MarkCursor") (AppE c e)

pattern GlobalC :: Var
pattern GlobalC <- (Var "GlobalC")
  where GlobalC = toVar "GlobalC"

--------------------------------------------------------------------------------

-- | The goal of this pass is to take effect signatures and translate
-- them into extra arguments and returns.  This pass does not worry
-- about where the witnesses come from to synthesize these extra
-- returns, it just inserts references to them that create demand.
cursorize :: L2.Prog -> SyM L2.Prog
cursorize L2.Prog{ddefs,fundefs,mainExp} = -- ddefs, fundefs
    dbgTrace lvl ("Starting routeEnds on "++show(doc fundefs)) $ do
    -- Prog emptyDD <$> mapM fd fundefs <*> pure Nothing

    fds' <- mapM fd $ M.elems fundefs

    -- let gloc = "global"
    mn <- case mainExp of
            Nothing -> return Nothing
            Just (x,t)  -> Just . (,t) <$> tail [] (M.empty,emptyWEnv) x
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
      fresh <- gensym $ toVar "tupin"
      let argLoc  = argtyToLoc (L2.mangle newArg) inT
          (newArg, bod, wenv) =
              if newIn == [] -- No injected cursor params..
              then (funarg, funbod, witnessBinding newArg argLoc)
              else ( fresh
                     -- We could introduce a let binding, but here we
                     -- just substitute instead:
                   -- , L1.subst funarg (projNonFirst (length newIn) (L1.VarE fresh))
                   --            funbod
                   , LetE (funarg, fmap (const ()) inT,
                           (L1.projNonFirst (length newIn) (L1.VarE fresh)))
                          funbod
                   , witnessBinding fresh
                     (TupLoc $ L.map Fixed newIn ++ [argLoc]))
      let env = M.singleton newArg (L2.stripTyLocs inT, argLoc)
      exp' <- tail newOut (env,wenv) bod
      return $ L2.FunDef funname newTy newArg exp'

  -- Process the "spine" of a flattened program.
  --
  -- When processing something in tail position, we take a DEMAND for
  -- a certain list of location witnesses.  We produce a tuple containing
  -- those witnesses prepended to the original return value.
  tail :: [LocVar] -> (Env,WitnessEnv) -> L1.Exp -> SyM L1.Exp
  tail demanded (env,wenv) e =
   dbgTrace lvl ("\n[cursorize] Processing tail, demanding "++show demanded++
                 ": "++show (doc e)++"\n  with wenv: "++show wenv) $
   let cursorRets = L.map (meetDemand wenv) demanded in
   case e of
     ------------------- Return cases -----------------
     -- Trivial return cases need to just pluck from the environment:
     L1.LitE _ -> return $ mkProd cursorRets e
     L1.VarE _ -> return $ mkProd cursorRets e
     -- INVARIANT: None of the primapps yield new witnesses:
     L1.PrimAppE p ls -> L1.assertTrivs ls $
        return $ mkProd cursorRets (L1.PrimAppE p ls)

     -- L1.MkPackedE k ls -> L1.assertTrivs ls $
     --    return $ mkProd cursorRets (L1.MkPackedE k ls)

     ------------------ Flattened Spine ---------------

     -- Here we shatter the constructor into a separate write tag
     -- action and field population actions.  These will subsequently
     -- need reordering, because the field writes may have ALREADY
     -- occured at this point.
     L1.LetE (v,tv, L1.MkPackedE k ls) bod ->
       L1.LetE (v,tv, MarkCursor GlobalC (L1.MkPackedE k ls)) <$>
         let env' = M.insert v (tv, Fixed v) env in
         tail demanded (env',wenv) bod

     -- Here we route through extra arguments.
     L1.LetE (v,tv,erhs) bod ->
       do (new,rhs',rty,rloc) <- rhs (env,wenv) erhs
          -- rty will always be a known number of cursors (0 or more)
          -- prepended to the value that was returned in the orignal prog
          let env'  = M.insert v (tv,rloc) env
          if L.null new
             -- assert rty == tv
           then do bod' <- tail demanded (env',wenv) bod -- No new witnesses.
                   return $ L1.LetE (v, tv, rhs') bod'
           else do _tmp <- gensym $ toVar "tmp"
                   let go []       _  we = return we
                       go (lc:rst) ix we =
                        go rst (ix+1) =<<
                          let Just v = L2.getLocVar lc in
                          addWitness v [] (\[] -> L1.ProjE ix (L1.VarE (toVar "tmp"))) we
                   wenv' <- go new 0 wenv
                   _bod' <- tail demanded (env',wenv') bod -- No new witnesses.
                   let ix = length new
                   return $ L1.LetE ((toVar "tmp"), rty, rhs') $
                            L1.LetE (v, tv, L1.projNonFirst ix (L1.VarE (toVar "tmp"))) $
                            finishEXP

     L1.IfE a b c -> do
         (new,a',aty,aloc) <- rhs (env,wenv) a
         maybeLetTup (new++[aloc]) (aty,a') wenv $ \ ex wenv' -> do
            b' <- tail demanded (env,wenv') b
            c' <- tail demanded (env,wenv') c
            return $ L1.IfE ex b' c'


     L1.CaseE e1 mp ->
         do (new,e1',e1ty,loc) <- rhs (env,wenv) e1
            maybeLetTup new (e1ty,e1') wenv $ \ ex wenv1 -> do
              ls' <- forM mp
                        (\ (dcon,vrs,rhs) -> do
                           let tys    = lookupDataCon ddefs dcon
                           tys' <- mapM L2.tyWithFreshLocs tys
                           -- No actual information here:
                           -- let wenv2 = (witnessTypedBinds zipped) `unionWEnv` wenv1
                           let locvar = case L2.getLocVar loc of
                                          Just l -> l
                                          Nothing -> error$ "cursorize/CaseE expected locvar: "++show loc

                           --       if all(static):
                           wenv3 <- addWitness (endOf loc) [locvar]
                                         (\ [lv_wit] ->
                                            L1.PrimAppE L1.AddP
                                            [ lv_wit
                                            , L1.LitE $ 1 + sum (L.map (tySize ddefs) tys)])
                                         wenv1

                           let -- tys'' = mapPacked (\ k l -> __) tys
                               env' = (M.fromList $
                                       fragileZip vrs (zip tys (L.zipWith argtyToLoc vrs tys')))
                                        `M.union` env

                           rhs' <- tail demanded (env',wenv3) rhs
                           return (dcon,vrs,rhs'))
              return$ L1.CaseE ex ls'


     _ -> error$ "cursorize/tail - FINISHME:\n  "++sdoc e

  -- Process an expression that occurs on the RHS of a let.  This may
  -- "push" new witnesses back to the caller (as opposed to the "pull"
  -- of demand arguments to tail).  The pressure comes from any call site
  -- that returns witnesses, which then must be passed along up to the spine.
  --
  -- Return values:
  --
  --  (1) A list corresponding to the cursor values ADDED to the
  --      return type, containing their locations.
  --  (2) The updated expression, possibly with a tupled return type
  --      thereby including the new cursor returns.
  --  (3) The type of the new result, including the added returns.
  --  (4) The location of the processed expression, NOT including the
  --      added returns.
  rhs :: (Env,WitnessEnv) -> L1.Exp -> SyM ([Loc], L1.Exp, L1.Ty, Loc)
  rhs (env,wenv) e =
    dbgTrace lvl ("\n[cursorize] Processing exp: "++show (doc e)++"\n  with wenv: "++show wenv) $
    case e of
     L1.VarE v  -> let (ty,loc) = env # v in return ([], e, ty, loc)
     L1.LitE _  -> return ([], e, L1.IntTy, Bottom)

     L1.PrimAppE p ls -> L1.assertTrivs ls $
        let ty = L2.primRetTy p in
        return ([], e, ty, retTyToLoc ty)

     -- This returns an updated cursor witnessing
     -- L1.MkPackedE
{-

     -- Here's where the magic happens, we must populate new cursor arguments:
     L1.AppE f e ->
       maybeLet e $ \ (extra,rands) ->
        finishEXP

     L1.IfE a b c -> do
       (a',aloc) <- rhs wenv a
       -- If we need to route traversal results out of the branches,
       -- we need to change the type of these branches.
       return (finishEXP, finishLOC)
-}
--     _ -> return ([], finishEXP, finishTYP, finishLOC)
     _ -> error $ "ERROR: cursorize/rhs: unfinished, needs to handle:\n "++sdoc e


-- | Make a product type while avoiding unary products.
mkProd :: [Exp] -> Exp -> Exp
mkProd [] e = e
mkProd ls e = MkProdE $ ls++[e]

endOf :: Loc -> LocVar
endOf (Fixed a) = toEndVar a
endOf l = error $ "endOf: should not take endOf this location: "++show l

tySize :: DDefs L1.Ty -> L1.Ty -> Int
tySize ddefs t =
  case t of
    L1.IntTy -> 8
    L1.SymTy -> 8
    L1.Packed k -> let tys = lookupDataCon ddefs k in
                   1 + sum (L.map (tySize ddefs) tys)
    _ -> error $ "tySize: should not have packed field of type: "++sdoc t


-- | Let bind IFF there are extra cursor results.
maybeLetTup :: [Loc] -> (L1.Ty, L1.Exp) -> WitnessEnv
            -> (L1.Exp -> WitnessEnv -> SyM L1.Exp) -> SyM L1.Exp
maybeLetTup locs (ty,ex) env fn =
  case locs of
   -- []  -> error$ "maybeLetTup: didn't expect zero locs:\n  " ++sdoc (ty,ex)
   -- Zero extra cursor return values
   [] -> fn ex env
   -- Otherwise the layout of the tuple is (cursor0,..cursorn, origValue):
   _   -> do
     -- The name doesn't matter, just that it's in the environment:
     tmp <- gensym $ toVar "mlt"
     -- Let-bind all the new things that come back with the exp
     -- to bring them into the environment.
     let env' = witnessBinding tmp (TupLoc locs) `unionWEnv` env
         n = length locs
     bod <- fn (L1.mkProj (n - 1) n (L1.VarE tmp)) env'
     return $ L1.LetE (tmp, ty, ex) bod


-- | Dig through an environment to find a witness.
meetDemand :: WitnessEnv -> LocVar -> L1.Exp
meetDemand we@WE{known} vr =
  trace ("Attempting to meet demand for loc "++show vr++" in env "++sdoc we) $
  known # vr
  -- go (M.toList env)
  -- where
  --  go [] = error$ "meetDemand: internal error, got to end of"++
  --                   " environment without finding loc: "++show vr


finishEXP :: L1.Exp
finishEXP = (L1.VarE (toVar "FINISHME"))

_finishLOC :: Loc
_finishLOC = Fresh (toVar "FINISHME")

_finishTYP :: L1.Ty
_finishTYP = L1.Packed "FINISHME"
