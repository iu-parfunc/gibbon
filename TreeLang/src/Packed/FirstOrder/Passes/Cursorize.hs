{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

-- | Inserting cursors and lowering to the target language.
--   This shares a lot with the effect-inference pass.

module Packed.FirstOrder.Passes.Cursorize
    (cursorize, lower) where

import Control.Monad
import Control.DeepSeq
import Packed.FirstOrder.Common hiding (FunDef)
import qualified Packed.FirstOrder.L1_Source as L1
import           Packed.FirstOrder.LTraverse as L2
import qualified Packed.FirstOrder.Target as T
import Data.Maybe
import Data.List as L hiding (tail)
import Data.Set as S
import Data.Map as M
import Text.PrettyPrint.GenericPretty
import Debug.Trace

-- | Chatter level for this module:
lvl :: Int
lvl = 5


-- =============================================================================

-- | Map every lexical variable in scope to an abstract location.
--   Some variables are cursors, and they are runtime witnesses to
--   particular location variables.
type Env = M.Map Var (L1.Ty,Loc)

-- | This maps an abstract location variable onto an expression that
-- witnesses it.  These can be open terms with free location variables
-- that are not (yet) bound in the `WitnessEnv` 
data WitnessEnv = WE { known :: M.Map LocVar L1.Exp
                     -- ^ Known open terms that witness locations.
                     , open  :: M.Map LocVar Var
                     -- ^ "Holes" for unknown witnesses, and the
                     -- unbound variables that are expected to receive them.
                     }
  deriving (Read,Show,Eq,Ord, Generic, NFData)

instance Out WitnessEnv
                
-- | This inserts cursors and REMOVES effect signatures.  It returns
--   the new type as well as how many extra params were added to input
--   and return types.
cursorizeTy :: ArrowTy Ty -> (ArrowTy Ty, [LocVar], [LocVar])
cursorizeTy (ArrowTy inT ef ouT) = (newArr, newIn, newOut)
 where
  newArr = ArrowTy newInTy S.empty newOutTy
  newInTy  = prependArgs (L.map mkCursorTy newIn)
                         (mapPacked (\_ l -> mkCursorTy l) inT)
  -- Let's turn output values into updated-output-cursors:a
  newOutTy = prependArgs (L.map mkCursorTy newOut)
                         (mapPacked (\_ l -> mkCursorTy (toEndVar l)) ouT)
                         -- Or they could be void...

  -- Every packed input means another output (new return value for the
  -- moved cursor), and conversely, every output cursor must have had
  -- an original position (new input param):
  newOut   = [ toEndVar v  -- This determines the ORDER of added inputs.
             | Traverse v <- S.toList ef ]
  newIn    = allLocVars ouT -- These stay in their original order (preorder) 

-- Injected cursor args go first in input and output:
prependArgs :: [Ty] -> Ty -> Ty
prependArgs [] t = t
prependArgs ls t = ProdTy $ ls ++ [t]

             
mkArrowTy :: Ty -> Ty -> ArrowTy Ty
mkArrowTy x y = ArrowTy x S.empty y

-- | Replace all packed types with something else.
replacePacked :: Ty -> Ty -> Ty
replacePacked (t2::Ty) (t::Ty) =
  case t of
    IntTy  -> IntTy
    BoolTy -> BoolTy
    SymTy  -> SymTy
    (ProdTy x)    -> ProdTy $ L.map (replacePacked t2) x
    (SymDictTy x) -> SymDictTy $ (replacePacked t2) x
    PackedTy{}    -> t2

mapPacked :: (Var -> LocVar -> Ty) -> Ty -> Ty
mapPacked fn t =
  case t of
    IntTy  -> IntTy
    BoolTy -> BoolTy
    SymTy  -> SymTy
    (ProdTy x)    -> ProdTy $ L.map (mapPacked fn) x
    (SymDictTy x) -> SymDictTy $ mapPacked fn x
    PackedTy k l  -> fn k l


-- | Binding a variable to a value at a given (abstract) location can
-- bring multiple witnesses into scope.  
witnessBinding :: Var -> Loc -> WitnessEnv
witnessBinding vr loc = WE (M.fromList $ go loc (L1.VarE vr)) M.empty
  where
   go (TupLoc ls) ex =
       concat [ go x (L1.ProjE ix ex)
              | (ix,x) <- zip [0..] ls ]
   go (Fresh v) e = [ (v,e) ]
   go (Fixed v) e = [ (v,e) ]
   go Top       _ = []
   go Bottom    _ = []

-- FIXME: We should be able to combine `Loc` and the annotated `Ty`
-- data types....
witnessTypedBinds :: [(Var,Ty)] -> WitnessEnv 
witnessTypedBinds [] = emptyWEnv
witnessTypedBinds ((vr,ty):rst) =
    witnessBinding vr (argtyToLoc (mangle vr) ty)  `unionWEnv`
    witnessTypedBinds rst

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

addWitness :: LocVar -> [LocVar] -> ([L1.Exp] -> L1.Exp) -> WitnessEnv -> SyM WitnessEnv
addWitness locvar ls fn orig@WE{known} =
    do (es, free) <- unzip <$> mapM lkp ls
       return $ WE (M.singleton locvar (fn es)) (M.fromList (catMaybes free))
                  `unionWEnv` orig
  where
   lkp lvr = case M.lookup lvr known of
              Just wex -> return (wex,Nothing)
              Nothing -> do tmp <- gensym "hl"
                            return (L1.VarE tmp, Just (lvr,tmp))

extendEnv :: [(Var,(L1.Ty,Loc))] -> Env -> Env
extendEnv ls e = (M.fromList ls) `M.union` e
                                   
-- | A compiler pass that inserts cursor-passing for reading and
-- writing packed values.
cursorize :: Prog -> SyM Prog  -- [T.FunDecl]
cursorize Prog{ddefs,fundefs,mainExp} = -- ddefs, fundefs
    dbgTrace lvl ("Starting cursorize on "++show(doc fundefs)) $ do 
    -- Prog emptyDD <$> mapM fd fundefs <*> pure Nothing

    fds' <- mapM fd $ M.elems fundefs
    mn <- case mainExp of
            Nothing -> return Nothing
            Just x  -> Just <$> tail [] (M.empty,emptyWEnv) x
    return Prog{ fundefs = M.fromList $ L.map (\f -> (funname f,f)) fds'
               , ddefs = ddefs
               , mainExp = mn
               }
 where
  fd :: FunDef -> SyM FunDef
  fd (f@FunDef{funname,funty,funarg,funbod}) = 
      let (newTy@(ArrowTy inT _ _outT),newIn,newOut) = cursorizeTy funty in
      dbgTrace lvl ("Processing fundef: "++show(doc f)++"\n  new type: "++sdoc newTy) $
   do      
      fresh <- gensym "tupin"
      let argLoc  = argtyToLoc (mangle newArg) inT
          (newArg, bod, wenv) =
              if newIn == [] -- No injected cursor params..
              then (funarg, funbod, witnessBinding newArg argLoc)
              else ( fresh
                     -- We could introduce a let binding, but here we
                     -- just substitute instead:
                   , L1.subst funarg (L1.ProjE (length newIn) (L1.VarE fresh))
                              funbod
                   , witnessBinding fresh
                     (TupLoc $ L.map Fixed newIn ++ [argLoc]))
      let env = M.singleton newArg (stripTyLocs inT, argLoc)
      exp' <- tail newOut (env,wenv) bod
      return $ FunDef funname newTy newArg exp'

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
     L1.MkPackedE k ls -> L1.assertTrivs ls $ 
        return $ mkProd cursorRets (L1.MkPackedE k ls)
     -- INVARIANT: None of the primapps yield new witnesses:
     L1.PrimAppE p ls -> L1.assertTrivs ls $
        return $ mkProd cursorRets (L1.PrimAppE p ls)
                
     ------------------ Flattened Spine ---------------
     -- Here we route through extra arguments.
     L1.LetE (v,tv,erhs) bod ->
       do (new,rhs',rty,rloc) <- rhs (env,wenv) erhs
          -- ty will always be a known number of cursors (0 or more)
          -- prepended to the value that was already 
          tmp <- gensym "tmp"
          let ix = length new
          return $ L1.LetE ("tmp", rty, rhs') $
                   L1.LetE (v, tv, L1.ProjE ix (L1.VarE "tmp")) $
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
              ls' <- forM (M.toList mp)
                        (\ (dcon, (vrs,rhs)) -> do
                           let tys    = lookupDataCon ddefs dcon
                           tys' <- mapM tyWithFreshLocs tys
                           -- No actual information here:
                           -- let wenv2 = (witnessTypedBinds zipped) `unionWEnv` wenv1
                           let Just locvar = getLocVar loc

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
                           return (dcon, (vrs,rhs')))
              return$ L1.CaseE ex (M.fromList ls')


     _ -> error$ "cursorize/tail - FINISHME:\n  "++sdoc e
     _ -> error$ "cursorize, tail expression expected, found:\n  "++sdoc e
             
  -- Process an expression that occurs on the RHS of a let.  This may 
  -- "push" new witnesses back to the caller (as opposed to the "pull"
  -- of demand arguments to tail).  The pressure comes from any call site
  -- that returns witnesses, which then must be passed along up to the spine.
  --
  -- Return values:
  -- 
  --  (1) A list corresponding to the cursor values ADDED to the
  --      return type, containing their locations.
  --  (2) The updated expression, possible with a tupled return type
  --      thereby including the new cursor returns.
  --  (3) The type of the new result, including the added returns.
  --  (4) The location of the processed expression, not including the
  --      added returns.
  rhs :: (Env,WitnessEnv) -> L1.Exp -> SyM ([Loc], L1.Exp, L1.Ty, Loc)
  rhs (env,wenv) e = 
    dbgTrace lvl ("\n[cursorize] Processing exp: "++show (doc e)++"\n  with wenv: "++show wenv) $
    case e of
     L1.VarE v  -> let (ty,loc) = env # v in return ([], e, ty, loc)
     L1.LitE _  -> return ([], e, L1.IntTy, Bottom)

     L1.PrimAppE p ls -> L1.assertTrivs ls $
        let ty = L1.primRetTy p in
        return ([], e, ty, retTyToLoc ty)
{-
     L1.LitE  _ -> return (__, Bottom)

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
     tmp <- gensym "mlt"
     -- Let-bind all the new things that come back with the exp
     -- to bring them into the environment.       
     let env' = witnessBinding tmp (TupLoc locs) `unionWEnv` env
     bod <- fn (L1.ProjE (length locs - 1) (L1.VarE tmp)) env'
     return $ L1.LetE (tmp, ty, ex) bod

          
-- | Dig through an environment to find
meetDemand :: WitnessEnv -> LocVar -> L1.Exp
meetDemand we@WE{known} vr =
  trace ("Attempting to meet demand for loc "++show vr++" in env "++sdoc we) $
  known # vr
  -- go (M.toList env)
  -- where
  --  go [] = error$ "meetDemand: internal error, got to end of"++
  --                   " environment without finding loc: "++show vr
                    

mkProd :: [L1.Exp] -> L1.Exp -> L1.Exp
mkProd [] e = e
mkProd ls e = L1.MkProdE $ ls++[e]
                                                        

finishEXP :: L1.Exp
finishEXP = (L1.VarE "FINISHME")

finishLOC :: Loc
finishLOC = Fresh "FINISHME"

finishTYP :: L1.Ty
finishTYP = L1.Packed "FINISHME" 
            
-- =============================================================================

-- | Convert into the target language.  This does not make much of a
-- change, but it checks the changes that have already occurred.
--
-- The only substantitive conversion here is of tupled arguments to
-- multiple argument functions.
lower :: L2.Prog -> SyM T.Prog
lower prg@L2.Prog{fundefs,ddefs,mainExp} = do
  let mn = tail <$> mainExp
  mn <- case mainExp of
          Nothing -> return Nothing
          Just x  -> (Just . T.PrintExp) <$> tail x
  T.Prog <$> mapM fund (M.elems fundefs) <*> pure mn
 where
  fund :: L2.FunDef -> SyM T.FunDecl
  fund FunDef{funname,funty=(L2.ArrowTy inty _ outty),funarg,funbod} = do
      tl <- tail funbod
      return $ T.FunDecl { T.funName = funname
                         , T.funArgs = [(funarg, typ' inty)]
                         , T.funRetTy = typ' outty
                         , T.funBody = tl } 

  tail :: L1.Exp -> SyM T.Tail
  tail ex = 
   case ex of
--    L1.LetE (v,t,rhs) bod -> T.LetE (v,t,tail rhs) (tail bod)
    L1.VarE v          -> pure$ T.RetValsT [T.VarTriv v]

    -- TWO OPTIONS HERE: we could push equality prims into the target lang.
    -- Or we could map directly onto the IfEqT form:
    -- L1.IfE (L1.PrimAppE L1.EqP __ ) b c -> __
                          
    L1.IfE a b c       -> do b' <- tail b
                             c' <- tail c
                             return $ T.Switch (triv "if test" a)
                                      (T.IntAlts [(0, b')])
                                      (Just c')

    L1.AppE v e        -> return $ T.TailCall v [triv "operand" e]

                          
    -- FIXME: No reason errors can't stay primitive at Target:
    L1.PrimAppE (L1.ErrorP _str _ty) [] ->
      pure T.ErrT 
    L1.LetE (_,_,L1.PrimAppE (L1.ErrorP _str _) []) _ ->
      pure T.ErrT 

    -- Whatever, a little just in time flattening.  Should obsolete this:
    L1.PrimAppE p ls -> do
      tmp <- gensym "flt"
      tail (L1.LetE (tmp, L1.primRetTy p, L1.PrimAppE p ls) (L1.VarE tmp))
       
    L1.LetE (v,t,L1.PrimAppE p ls) bod ->
        T.LetPrimCallT [(v,typ t)]
             (prim p)
             (L.map (triv "prim rand") ls) <$>
             (tail bod)

    L1.LetE (v,t,L1.AppE f arg) bod -> do
        T.LetCallT [(v,typ t)] f
             [(triv "app rand") arg]
             <$>
             (tail bod)    

    L1.CaseE e ls ->
        return $ T.Switch{} -- (tail e) (M.map (\(vs,er) -> (vs,tail er)) ls)

    _ -> error$ "lower: unexpected expression in tail position:\n  "++sdoc ex
             
{-    
    L1.LitE _          -> ex
    
    L1.PrimAppE p ls   -> L1.PrimAppE p $ L.map tail ls
    
    L1.ProjE i e       -> L1.ProjE i (tail e)
    L1.CaseE e ls      -> L1.CaseE (tail e) (M.map (\(vs,er) -> (vs,tail er)) ls)
    L1.MkProdE ls      -> L1.MkProdE $ L.map tail ls
    L1.MkPackedE k ls  -> L1.MkPackedE k $ L.map tail ls
    L1.TimeIt e        -> L1.TimeIt $ tail e
    
-}

  triv :: String -> L1.Exp -> T.Triv
  triv msg e0 =
    case e0 of
      (L1.VarE x) -> T.VarTriv x
      (L1.LitE x) -> T.IntTriv x
      -- TODO: I think we should allow tuples and projection in trivials:
--      (ProjE x1 x2) -> __
--      (MkProdE x) -> __
      _ -> error $ "lower/triv, expected trivial in "++msg++", got "++sdoc e0
  
  typ :: L1.Ty -> T.Ty
  typ t =
    case t of
      L1.IntTy -> T.IntTy
      L1.SymTy -> T.SymTy
      L1.BoolTy -> T.IntTy
      (L1.ProdTy xs) -> T.ProdTy $ L.map typ xs
      (L1.SymDictTy x) -> T.SymDictTy $ typ x
      -- t | isCursorTy t -> T.CursorTy
      (L1.Packed k)
          | k == L2.con L2.cursorTy -> T.CursorTy
          | otherwise -> error "lower/typ: should not encounter "

  typ' :: L2.Ty -> T.Ty
  typ' = error "FINISHME lower/typ'"

  prim :: L1.Prim -> T.Prim
  prim p =
    case p of
      L1.AddP -> T.AddP
      L1.SubP -> T.SubP
      L1.MulP -> T.MulP
      L1.EqSymP  -> error "lower/prim should only have eq? directly in comparison of an If"
      L1.DictInsertP -> T.DictInsertP
      L1.DictLookupP -> T.DictLookupP
      L1.ErrorP{} -> error$ "lower/prim: internal error, should not have got to here: "++show p

-- ================================================================================

