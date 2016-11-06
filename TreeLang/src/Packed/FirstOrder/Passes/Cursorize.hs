{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

-- | Inserting cursors and lowering to the target language.
--   This shares a lot with the effect-inference pass.

module Packed.FirstOrder.Passes.Cursorize
    ( cursorize
    , cursorDirect
    , pattern WriteInt, pattern ReadInt, pattern NewBuffer
    , pattern CursorTy, pattern ScopedBuffer
    ) where

import Control.Monad
import Control.Applicative
import Control.DeepSeq
import Packed.FirstOrder.Common hiding (FunDef)
import qualified Packed.FirstOrder.L1_Source as L1
import qualified Packed.FirstOrder.LTraverse as L2
import           Packed.FirstOrder.L1_Source (Ty1(..),pattern SymTy)
import           Packed.FirstOrder.LTraverse
    (argtyToLoc, Loc(..), ArrowTy(..), Effect(..), toEndVar,
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
-- STRATEGY ONE - inline until we have direct cursor handoff
--------------------------------------------------------------------------------

-- Some expressions are ready to take and return cursors.  Namely,
-- data constructors and function calls that return one packed type.  Examples:

--   MkFoo 3 (MkBar 4)
--   MkFoo 3 (fn 4)

-- In either case, the MkFoo constructor can thread its output cursor
-- directly to MkBar or "fn".  But this direct-handoff approach
-- doesn't work for everything.  Functions that return multiple packed
-- values foil it and require sophisticated routing.

-- Direct handoff binds cursor-flow to control-flow.  Conditionals are
-- fine:

--   MkFoo (if a b c)

----------------------------------------

-- | Cursor insertion, strategy one.
cursorDirect :: L2.Prog -> SyM L2.Prog
cursorDirect L2.Prog{ddefs,fundefs,mainExp} = do
  ---- Mostly duplicated boilerplate with cursorize below ----
  ------------------------------------------------------------
  fds' <- mapM fd $ M.elems fundefs
  -- let gloc = "global"
  mn <- case mainExp of
          Nothing -> return Nothing
          Just (x,mainTy) -> Just . (,mainTy) <$>
             if L1.hasPacked mainTy
             then -- Allocate into a global cursor:
                  do cur <- gensym "globcur"
                     LetE (cur,CursorTy,NewBuffer) <$>
                      exp2 cur x
             else exp x
  return L2.Prog{ fundefs = M.fromList $ L.map (\f -> (L2.funname f,f)) fds'
                , ddefs = ddefs
                , mainExp = mn
                }
 where
  fd :: L2.FunDef -> SyM L2.FunDef
  fd (f@L2.FunDef{funname,funty,funarg,funbod}) =
      let (newTy@(ArrowTy inT _ outT),newIn,newOut) = cursorizeTy funty in
      dbgTrace lvl ("Processing fundef: "++show(doc f)++"\n  new type: "++sdoc newTy) $
   do
      fresh <- gensym "tupin"
      let (newArg, bod) =
              if newIn == [] -- No injected cursor params..
              then (funarg, funbod)
              else ( fresh
                   , LetE (funarg, fmap (\_->()) inT,
                           (projNonFirst (length newIn) (L1.VarE fresh)))
                          funbod)
      exp' <- if L1.hasPacked outT
              then exp bod
              else do curs <- gensym "curs"
                      LetE (curs, CursorTy, projVal (VarE newArg)) <$>
                        exp2 curs bod
      return $ L2.FunDef funname newTy newArg exp'
  ------------------------------------------------------------

  -- | Here we are not in a context that flows to Packed data, thus no
  --   destination cursor.
  exp :: Exp -> SyM Exp
  exp ex =
    dbgTrace lvl (" 1. Processing expr in non-packed context, exp:\n  "++sdoc ex) $ 
    case ex of
      VarE _ -> return ex
      LitE _ -> return ex
      MkPackedE k ls -> error "cursorDirect: Should not have encountered MkPacked if type is not packed."

      -- If we're not returning a packed type in the current
      -- context, the we can only possibly see one that does NOT
      -- escape.  I.e. a temporary one:
      LetE (v,ty,rhs) bod
          | L1.hasPacked ty -> do tmp <- gensym  "tmpbuf"
                                  rhs' <- LetE (tmp,CursorTy,NewBuffer) <$>
                                           exp2 tmp rhs
                                  LetE (v,strip ty,rhs') <$> exp bod
          | otherwise -> do rhs' <- exp rhs
                            LetE (v,ty,rhs') <$> exp bod

      AppE v e      -> AppE v     <$> exp e
      PrimAppE p ls -> PrimAppE p <$> mapM exp ls
      ProjE i e  -> ProjE i <$> exp e
      CaseE scrtE ls -> do
         cur0 <- gensym "cursIn"
         -- Issue reads to get out all the fields:
         let unpack :: (Constr, [Var]) -> Exp -> SyM Exp
             unpack (k,vrs) ex =
              let go c _off [] = exp ex -- Everything is now in scope for the body.
                               -- TODO: we could witness the end if we still have the offset.
                  go c offset ((vr,ty):rs) = do
                    tmp <- gensym "tptmp"
                    let c' = case rs of
                               [] -> "end"
                               (v2,_):_ -> witnessOf v2
                    case ty of
                      -- TODO: Generalize to other scalar types:
                      IntTy ->
                        LetE (tmp, addCurTy IntTy, ReadInt c) <$>
                         LetE (c', CursorTy, projCur tmp) <$>
                          LetE (vr, IntTy, projVal (VarE tmp)) <$>
                           go c' (liftA2 (+) (L1.sizeOf IntTy) offset) rs
                      ty | L1.hasPacked ty -> do
                       -- Strategy: ALLOW unbound witness variables. A later traversal will reorder.
                       case offset of
                         Nothing -> go c' Nothing rs
                         -- POLICY: Name the witness the same as the variable for now:
                         Just n -> LetE (vr, CursorTy, add n (VarE cur0)) <$>
                                   go c' Nothing rs
                  add 0 e = e
                  add n e = PrimAppE L1.AddP [e, LitE n]
              in go cur0 (Just 0) (zip vrs (lookupDataCon ddefs k))

         scrtE' <- if allocFree scrtE
                   then exp scrtE
                   else do tmp <- gensym "scopd"
                           LetE (tmp,CursorTy,ScopedBuffer) <$>
                            projVal <$> exp2 tmp scrtE
         dbgTrace 1 (" 3. Case scrutinee "++sdoc scrtE++" alloc free?"++show (allocFree scrtE')) $ return ()
         -- Because the scrutinee is, naturally, of packed type, it
         -- will be represented as a pair of (start,end) pointers.                 
         CaseE scrtE' <$>
               (forM ls $ \ (k,vrs,e) -> do
                  e' <- unpack (k,vrs) e
                  return (k,[cur0],e'))
      MkProdE ls -> MkProdE <$> mapM exp ls
      TimeIt e t -> TimeIt <$> exp e <*> pure t
      IfE a b c  -> IfE <$> exp a <*> exp b <*> exp c
--        MapE (v,t,rhs) bod -> __
--        FoldE (v1,t1,r1) (v2,t2,r2) bod -> __

  -- Establish convention of putting end-cursor after value (or start-cursor):
  addCurTy ty = ProdTy[ty,CursorTy]
  projCur v = ProjE 1 (VarE v)
  projVal e = ProjE 0 e

  -- | Take a destination cursor.  Assume only a single packed output.
  --   Here we are in a context that flows to Packed data, we follow
  --   a convention of returning a pair of value/end.
  exp2 :: Var -> Exp -> SyM Exp
  exp2 destC ex =
    dbgTrace lvl (" 2. Processing expr in packed context, cursor "++show destC++", exp:\n  "++sdoc ex) $ 
    let go = exp2 destC in
    case ex of
      -- Here the allocation has already been performed:
      VarE _ -> return ex
      LitE _ -> error$ "cursorDirect/exp2: Should not encounter Lit in packed context: "++show ex

      -- Every return context expecting a packed value must now accept
      -- TWO values, a (st,en) pair, where "en" becomes the output cursor.
      MkPackedE k ls -> do
       -- tmp1  <- gensym "tmp"
       dest' <- gensym "cursplus1_"
       
       -- This stands for the  "WriteTag" operation:
       LetE (dest', PackedTy (getTyOfDataCon ddefs k) (),
                  MkPackedE k [VarE destC]) <$>
          let go d [] = return $ MkProdE [VarE destC, VarE d]
                 -- ^ The final return value lives at the position of the out cursor
              go d ((rnd,IntTy):rst) | L1.isTriv rnd = do
                  d'    <- gensym "curstmp"
                  LetE (d', CursorTy, WriteInt d rnd ) <$>
                    (go d' rst)
              -- Here we recursively transfer control
              go d ((rnd,ty@PackedTy{}):rst) = do
                  tup  <- gensym "tup"
                  d'   <- gensym "dest"
                  rnd' <- exp2 d rnd
                  LetE (tup, cursPairTy, rnd') <$>
                   LetE (d', CursorTy, ProjE 1 (VarE tup) ) <$>
                    (go d' rst)
          in go dest' (zip ls (lookupDataCon ddefs k))

      LetE (v,_,rhs) bod -> __

      AppE v e      -> AppE v     <$> go e
      PrimAppE p ls -> PrimAppE p <$> mapM go ls
      ProjE i e  -> ProjE i <$> go e
      CaseE e ls -> CaseE <$> (projVal <$> go e) <*>
                     mapM (\(k,vrs,e) -> (k,vrs,) <$> go e) ls
      MkProdE ls -> MkProdE <$> mapM go ls
      TimeIt e t -> TimeIt <$> go e <*> pure t
      IfE a b c  -> IfE <$> go a <*> go b <*> go c
--        MapE (v,t,rhs) bod -> __
--        FoldE (v1,t1,r1) (v2,t2,r2) bod -> __

cursPairTy :: L1.Ty
cursPairTy = ProdTy [CursorTy, CursorTy]

-- | Is the expression statically guaranteed to not need an output cursor?
allocFree :: Exp -> Bool 
allocFree ex =
 case ex of   
   (VarE x)         -> True
   (LitE x)         -> True
   (PrimAppE x1 x2) -> True

   (AppE x1 x2)     -> False                       
   (MkPackedE x1 x2) -> False
                       
   (LetE (_,_,e1) e2) -> allocFree e1 && allocFree e2
   (IfE x1 x2 x3) -> allocFree x1 && allocFree x2 && allocFree x3
   (ProjE _ x2)   -> allocFree x2
   (MkProdE x)    -> all allocFree x
   (CaseE x1 x2)  -> allocFree x1 && all (\(_,_,e) -> allocFree e) x2
   (TimeIt e _)   -> allocFree e
   (MapE (_,_,x1) x2) -> allocFree x1 && allocFree x2 
   (FoldE (_,_,x1) (_,_,x2) x3) -> allocFree x1 && allocFree x2 && allocFree x3

-- | Witness the location of a local variable.
witnessOf :: Var -> Var
witnessOf = ("witness_"++)

-- Packed types are gone, replaced by cursPair:
strip :: L1.Ty -> L1.Ty
strip ty =
  case ty of
    IntTy  -> ty
    BoolTy -> ty
    (ProdTy x) -> ProdTy $ L.map strip x
    (SymDictTy x) -> SymDictTy $ strip x
    (PackedTy x1 x2) -> cursPairTy
    (ListTy x) -> ListTy $ strip x

tyOfCase :: Out a => DDefs a -> Exp -> L1.Ty
tyOfCase dd (CaseE _ ((k,_,_):_)) = PackedTy (getTyOfDataCon dd k) ()


-- template:
        -- VarE v -> __
        -- LitE n -> __
        -- AppE v e -> __
        -- PrimAppE _ ls -> __
        -- LetE (v,_,rhs) bod -> __
        -- ProjE _ e -> __
        -- CaseE e ls -> __
        -- MkProdE ls     -> __
        -- MkPackedE _ ls -> __
        -- TimeIt e _ -> __
        -- IfE a b c -> __
        -- MapE (v,t,rhs) bod -> __
        -- FoldE (v1,t1,r1) (v2,t2,r2) bod -> __


--------------------------------------------------------------------------------
-- STRATEGY TWO - see the future (dataflow analysis)
--------------------------------------------------------------------------------

-- Annotate each type in a let binding with which location it flows to.a
-- pattern Ann

-- Insert location annotations on each let-bound variable.  These
-- record facts about the subsequent dataflow of the value, and ultimately
-- determine which output cursors to use.
-- insertLocs


--------------------------------------------------------------------------------

pattern NewBuffer = AppE "NewBuffer" (MkProdE [])

-- | output buffer space that is known not to escape the current function.
pattern ScopedBuffer = AppE "ScopedBuffer" (MkProdE [])
                    
-- Tag writing is still modeled by MkPackedE.
pattern WriteInt v e = AppE "WriteInt" (MkProdE [VarE v, e])
-- One cursor in, (int,cursor') output.
pattern ReadInt v = AppE "ReadInt" (MkProdE [VarE v])

pattern CursorTy = PackedTy "CURSOR_TY" () -- Tempx

-- pattern MarkCursor e = AppE "MarkCursor" (MkProdE [e])
pattern MarkCursor c e = AppE "MarkCursor" (AppE c e)

pattern GlobalC = "GlobalC"

      
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
                     , open  :: M.Map LocVar Var
                     -- ^ "Holes" for unknown witnesses, and the
                     -- unbound variables that are expected to receive them.
                     }
  deriving (Read,Show,Eq,Ord, Generic, NFData)

instance Out WitnessEnv

-- | This inserts cursors and REMOVES effect signatures.  It returns
--   the new type as well as how many extra params were added to input
--   and return types.
cursorizeTy :: L2.ArrowTy L2.Ty -> (ArrowTy L2.Ty, [LocVar], [LocVar])
cursorizeTy (ArrowTy inT ef ouT) = (newArr, newIn, newOut)
 where
  newArr = ArrowTy newInTy S.empty newOutTy
  newInTy  = prependArgs (L.map L2.mkCursorTy newIn)
                         (mapPacked (\_ l -> L2.mkCursorTy l) inT)
  -- Let's turn output values into updated-output-cursors:
  newOutTy = prependArgs (L.map L2.mkCursorTy newOut)
                         (mapPacked (\_ l -> L2.mkCursorTy (toEndVar l)) ouT)
                         -- Or they could be void...

  -- Every packed input means another output (new return value for the
  -- moved cursor), and conversely, every output cursor must have had
  -- an original position (new input param):
  newOut   = [ toEndVar v  -- This determines the ORDER of added inputs.
             | Traverse v <- S.toList ef ]
  newIn    = L2.allLocVars ouT -- These stay in their original order (preorder)

-- Injected cursor args go first in input and output:
prependArgs :: [L2.Ty] -> L2.Ty -> L2.Ty
prependArgs [] t = t
prependArgs ls t = ProdTy $ ls ++ [t]


mkArrowTy :: L2.Ty -> L2.Ty -> ArrowTy L2.Ty
mkArrowTy x y = ArrowTy x S.empty y

-- | Replace all packed types with something else.
replacePacked :: L2.Ty -> L2.Ty -> L2.Ty
replacePacked (t2::L2.Ty) (t::L2.Ty) =
  case t of
    IntTy  -> IntTy
    BoolTy -> BoolTy
    SymTy  -> SymTy
    (ProdTy x)    -> ProdTy $ L.map (replacePacked t2) x
    (SymDictTy x) -> SymDictTy $ (replacePacked t2) x
    PackedTy{}    -> t2

mapPacked :: (Var -> LocVar -> L2.Ty) -> L2.Ty -> L2.Ty
mapPacked fn t =
  case t of
    IntTy  -> IntTy
    BoolTy -> BoolTy
    SymTy  -> SymTy
    (ProdTy x)    -> ProdTy $ L.map (mapPacked fn) x
    (SymDictTy x) -> SymDictTy $ mapPacked fn x
    PackedTy k l  -> fn k l


mkProj :: (Eq a, Num a) => Int -> a -> L1.Exp -> L1.Exp
mkProj 0 1 e = e
mkProj ix _ e = L1.ProjE ix e


-- | Binding a variable to a value at a given (abstract) location can
-- bring multiple witnesses into scope.
witnessBinding :: Var -> Loc -> WitnessEnv
witnessBinding vr loc = WE (M.fromList $ go loc (L1.VarE vr)) M.empty
  where
   go (TupLoc ls) ex =
       concat [ go x (mkProj ix (length ls) ex)
              | (ix,x) <- zip [0..] ls ]
   go (Fresh v) e = [ (v,e) ]
   go (Fixed v) e = [ (v,e) ]
   go Top       _ = []
   go Bottom    _ = []

-- FIXME: We should be able to combine `Loc` and the annotated `Ty`
-- data types....
witnessTypedBinds :: [(Var,L2.Ty)] -> WitnessEnv
witnessTypedBinds [] = emptyWEnv
witnessTypedBinds ((vr,ty):rst) =
    witnessBinding vr (argtyToLoc (L2.mangle vr) ty)  `unionWEnv`
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

-- | Add a witness GIVEN witnesses of a list of other locations.
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


--------------------------------------------------------------------------------

-- | A compiler pass that inserts cursor-passing for reading and
-- writing packed values.
cursorize :: L2.Prog -> SyM L2.Prog  -- [T.FunDecl]
cursorize L2.Prog{ddefs,fundefs,mainExp} = -- ddefs, fundefs
    dbgTrace lvl ("Starting cursorize on "++show(doc fundefs)) $ do
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
      fresh <- gensym "tupin"
      let argLoc  = argtyToLoc (L2.mangle newArg) inT
          (newArg, bod, wenv) =
              if newIn == [] -- No injected cursor params..
              then (funarg, funbod, witnessBinding newArg argLoc)
              else ( fresh
                     -- We could introduce a let binding, but here we
                     -- just substitute instead:
                   , L1.subst funarg (projNonFirst (length newIn) (L1.VarE fresh))
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


       -- do tmp <- gensym "buftmp"
       --    __ $ L1.LetE (tmp, CursorTy, NewBuffer) $
       --           L1.LetE (v,tv,_) _
       --    error $ "Process constructor in env: "++show (env,wenv)

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
           else do tmp <- gensym "tmp"
                   let go []       _  we = return we
                       go (lc:rst) ix we =
                        go rst (ix+1) =<<
                          let Just v = L2.getLocVar lc in
                          addWitness v [] (\[] -> L1.ProjE ix (L1.VarE "tmp")) we
                   wenv' <- go new 0 wenv
                   bod' <- tail demanded (env',wenv') bod -- No new witnesses.
                   let ix = length new
                   return $ L1.LetE ("tmp", rty, rhs') $
                            L1.LetE (v, tv, projNonFirst ix (L1.VarE "tmp")) $
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
        let ty = L1.primRetTy p in
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


-- | Project something which had better not be the first thing in a tuple.
projNonFirst :: Int -> L1.Exp -> L1.Exp
projNonFirst 0 e = error $ "projNonFirst: expected nonzero index into expr: "++sdoc e
projNonFirst i e = L1.ProjE i e

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
         n = length locs
     bod <- fn (mkProj (n - 1) n (L1.VarE tmp)) env'
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

-- | Remove all occurrences of tuples except:
--   (1) returned to a let binding by a funcall on RHS
--   (2) returned to a let binding by an if-expr on RHS
detuple :: L2.Prog -> SyM L2.Prog
detuple = undefined
