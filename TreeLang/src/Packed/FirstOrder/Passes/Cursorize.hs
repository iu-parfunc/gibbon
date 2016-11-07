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
    ( cursorDirect
    , routeEnds
    , findWitnesses
    , pattern WriteInt, pattern ReadInt, pattern NewBuffer
    , pattern CursorTy, pattern ScopedBuffer
    ) where

import Control.Monad
import Control.Applicative
import Control.DeepSeq
import           Packed.FirstOrder.Common hiding (FunDef)
import qualified Packed.FirstOrder.L1_Source as L1
import qualified Packed.FirstOrder.LTraverse as L2
import           Packed.FirstOrder.L1_Source (Ty1(..),pattern SymTy)
import           Packed.FirstOrder.LTraverse
    (argtyToLoc, Loc(..), ArrowTy(..), Effect(..), toEndVar,
     FunDef(..), Prog(..), Exp(..))

-- We use some pieces from this other attempt:
import           Packed.FirstOrder.Passes.Cursorize2 (cursorizeTy)
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
--
-- Here we go to a "dilated" representation of packed values, where
-- every `Packed T` is represented by a pair, `(Cursor,Cursor)`.
--
-- We proceed with two loops, corresponding to packed and unpacked
-- context.  When the type of the current expression satisfies
-- `hasPacked`, that's when we're in packed context.  And, when in
-- packed context, we return dilated values.
-- 
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
  fd L2.FunDef{funname,funty,funarg,funbod} = do
     -- We don't add new function arguments yet, rather we leave
     -- unbound references to the function's output cursors, named
     -- "f_1, f_2..." for a function "f".    
     let ArrowTy _inT _ outT = funty
         outCurs = [ funname ++"_"++ show ix | ix <- [1 .. countPacked outT] ]
     exp' <- case outCurs of
               [] -> exp funbod -- not hasPacked
               [cur] -> 
                  do -- LetE (cur, CursorTy, projVal (VarE funarg)) <$>
                     projVal <$> exp2 cur funbod
               _ -> error $ "cursorDirect: add support for functionwith multiple output cursors: "++ funname

     return $ L2.FunDef funname funty funarg exp'

  -- Move elsewhere: where we actually change the calling convention.
  _fd' :: L2.FunDef -> SyM L2.FunDef
  _fd' (f@L2.FunDef{funname,funty,funarg,funbod}) =
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
                        projVal <$> exp2 curs bod
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
          | isPacked ty -> do tmp <- gensym  "tmpbuf"
                              rhs' <- LetE (tmp,CursorTy,NewBuffer) <$>
                                       exp2 tmp rhs
                              withDilated ty rhs' $ \rhs'' ->
                                -- Here we've reassembled the non-dialated view, original type:
                                LetE (v,ty, rhs'') <$> exp bod
          | L1.hasPacked ty -> error "cursorDirect: finishme, let bound tuple containing packed."
          | otherwise -> do rhs' <- exp rhs
                            LetE (v,ty,rhs') <$> exp bod

      AppE f e ->
          -- If the function argument is of a packed type, we need to switch modes:
          -- data ArrowTy t = ArrowTy { arrIn :: t, arrEffs:: (Set Effect), arrOut:: t }
          let ArrowTy arg _ _ret = L2.getFunTy fundefs f in
          case arg of
            -- TODO: apply the allocFree trick that we use below.  Abstract it out.
            PackedTy{} -> do cr <- gensym "argbuf"
                             LetE (cr,CursorTy,ScopedBuffer) <$> do
                               e' <- exp2 cr e                             
                               withDilated arg e' $ \e'' ->                                                               
                                 pure $ AppE f e''

            ty | L1.hasPacked ty -> error $
                    "cursorDirect: need to handle function argument of tupled packed types: "++show ty
               | otherwise -> AppE f <$> exp e

      PrimAppE p ls -> PrimAppE p <$> mapM exp ls
      ProjE i e  -> ProjE i <$> exp e
      CaseE scrtE ls -> do
         let tyScrut = tyOfCaseScrut ddefs ex
         cur0 <- gensym "cursIn"
         -- Issue reads to get out all the fields:
         let 
             unpack :: (Constr, [Var]) -> Exp -> SyM Exp
             unpack (k,vrs) ex = go cur0 (Just 0) vsts
                where 
                  vsts = zip vrs (lookupDataCon ddefs k)
                  (lastV,_) = L.last vsts

                  add 0 e = e
                  add n e = PrimAppE L1.AddP [e, LitE n]

                  go c _off [] = exp ex -- Everything is now in scope for the body.
                               -- TODO: we could witness the end if we still have the offset.
                  go c offset ((vr,ty):rs) = do
                    tmp <- gensym "tptmp"
                    -- Each cursor position is either the witness of
                    -- the next thing, or the witness of the end of the last thing.
                    let witNext e =
                            case rs of
                               []       -> e
                               (v2,_):_ -> LetE (witnessOf v2, CursorTy, projCur (VarE tmp)) e
                    case ty of
                      -- TODO: Generalize to other scalar types:
                      IntTy ->
                        LetE (tmp, addCurTy IntTy, ReadInt c) <$>
                         witNext <$>                  
                          LetE (toEndVar vr, CursorTy, projCur (VarE tmp)) <$>
                           LetE (vr, IntTy, projVal (VarE tmp)) <$>
                            go (toEndVar vr) (liftA2 (+) (L1.sizeOf IntTy) offset) rs
                      ty | isPacked ty -> do
                       -- Strategy: ALLOW unbound witness variables. A later traversal will reorder.
                       case offset of
                         Nothing -> go (toEndVar vr) Nothing rs
                         Just n -> LetE (witnessOf vr, CursorTy, add n (VarE cur0)) <$>
                                   go (toEndVar vr) Nothing rs


         -- Because the scrutinee is, naturally, of packed type, it
         -- will be represented as a pair of (start,end) pointers.
         -- But we take a little shortcut here and don't bother
         -- creating a new scoped region if we don't need to.
         scrtE' <- if allocFree scrtE
                   then Left <$> exp scrtE
                   else do tmp <- gensym "scopd"
                           Right <$>
                            LetE (tmp,CursorTy,ScopedBuffer) <$>
                             exp2 tmp scrtE

         dbgTrace 1 (" 3. Case scrutinee "++sdoc scrtE++" alloc free?"++show (allocFree scrtE)) $ return ()
         let mkit e = 
              CaseE e <$>
                (forM ls $ \ (k,vrs,e) -> do
                   e' <- unpack (k,vrs) e
                   return (k,[cur0],e'))
         case scrtE' of
           Left  e  -> mkit e
           Right di -> withDilated tyScrut di mkit

      MkProdE ls -> MkProdE <$> mapM exp ls
      TimeIt e t -> TimeIt <$> exp e <*> pure t
      IfE a b c  -> IfE <$> exp a <*> exp b <*> exp c
--        MapE (v,t,rhs) bod -> __
--        FoldE (v1,t1,r1) (v2,t2,r2) bod -> __

  -- | Take a destination cursor.  Assume only a single packed output.
  --   Here we are in a context that flows to Packed data, we follow a
  --   convention of returning a pair of value/end.
  --  CONTRACT: The caller must deal with this pair.
  exp2 :: Var -> Exp -> SyM Exp
  exp2 destC ex =
    dbgTrace lvl (" 2. Processing expr in packed context, cursor "++show destC++", exp:\n  "++sdoc ex) $ 
    let go = exp2 destC in
    case ex of
      -- Here the allocation has already been performed:
      -- Our variable in the lexical environment is bound to the start only, not (st,en).
      -- To follow the calling convention, we are reponsible for tagging on the end here:
      VarE v -> -- ASSERT: isPacked
          return $ MkProdE [VarE (witnessOf v), VarE (toEndVar v)] -- FindEndOf v
      LitE _ -> error$ "cursorDirect/exp2: Should not encounter Lit in packed context: "++show ex

      -- Here's where we write the dest cursor:
      MkPackedE k ls -> do
       -- tmp1  <- gensym "tmp"
       dest' <- gensym "cursplus1_"
       
       -- This stands for the  "WriteTag" operation:
       LetE (dest', PackedTy (getTyOfDataCon ddefs k) (),
                  MkPackedE k [VarE destC]) <$>
          let go2 d [] = return $ MkProdE [VarE destC, VarE d]
                 -- ^ The final return value lives at the position of the out cursor
              go2 d ((rnd,IntTy):rst) | L1.isTriv rnd = do
                  d'    <- gensym "curstmp"
                  LetE (d', CursorTy, WriteInt d rnd ) <$>
                    (go2 d' rst)
              -- Here we recursively transfer control
              go2 d ((rnd,ty@PackedTy{}):rst) = do
                  tup  <- gensym "tup"
                  d'   <- gensym "dest"
                  rnd' <- exp2 d rnd
                  LetE (tup, cursPairTy, rnd') <$>
                   LetE (d', CursorTy, ProjE 1 (VarE tup) ) <$>
                    (go2 d' rst)
          in go2 dest' (zip ls (lookupDataCon ddefs k))

      -- LetE (v,_,rhs) bod -> error "cursorDirect: finish let binding."

      -- AppE v e      -> AppE v  <$> go e
      -- PrimAppE p ls -> PrimAppE p <$> mapM go ls
      PrimAppE _ _ -> error$ "cursorDirect: unexpected PrimAppE in packed context: "++sdoc ex
      -- ProjE i e  -> ProjE i <$> go e
      -- CaseE e ls -> CaseE <$> (projVal <$> go e) <*>
      --                mapM (\(k,vrs,e) -> (k,vrs,) <$> go e) ls
      -- MkProdE ls -> MkProdE <$> mapM go ls
      -- TimeIt e t -> TimeIt <$> go e <*> pure t
      -- IfE a b c  -> IfE <$> go a <*> go b <*> go c
      _ -> error$ "cursorDirect: needs finishing:\n  "++sdoc ex
-- --        MapE (v,t,rhs) bod -> __
-- --        FoldE (v1,t1,r1) (v2,t2,r2) bod -> __


-- | A given return context for a type satisfying `hasPacked` either
-- flows to a single cursor or to multiple cursors.
data Dests = Cursor Var
           | TupOut [Maybe Dests] -- ^ The layout of this matches the
                                  -- ProdTy, but not every field contains a Packed.

--------------------------- Dilation Conventions -------------------------------

-- newtype DiExp = Di Exp
type DiExp = Exp

-- Establish convention of putting end-cursor after value (or start-cursor):
addCurTy :: Ty1 () -> Ty1 ()
addCurTy ty = ProdTy[ty,CursorTy]

cursPairTy :: L1.Ty
cursPairTy = ProdTy [CursorTy, CursorTy]

projCur :: Exp -> Exp
projCur e = ProjE 1 e

projVal :: Exp -> Exp
projVal e = ProjE 0 e
            
-- Packed types are gone, replaced by cursPair:
dilate :: Ty1 a -> L1.Ty
dilate ty =
  case ty of
    IntTy  -> IntTy 
    BoolTy -> BoolTy
    (ProdTy x) -> ProdTy $ L.map dilate x
    (SymDictTy x) -> SymDictTy $ dilate x
    (PackedTy x1 x2) -> cursPairTy
    (ListTy x) -> ListTy $ dilate x

-- Todo: could use DiExp newtype here:
-- | Type-directed dissassembly/reassembly of a dilated value.
withDilated :: Show a => Ty1 a -> DiExp -> (Exp -> SyM Exp) -> SyM Exp
withDilated ty edi fn =
  dbgTrace 5 ("withDilated: "++show ty++", diexp:\n "++sdoc edi) $
  case ty of
    IntTy  -> fn edi
    BoolTy -> fn edi
--    (ProdTy []) -> fn edi
--    (ProdTy (t:ts)) ->
    (ProdTy ls) ->
       let go _ [] acc = fn (MkProdE (reverse acc))
           go ix (t:ts) acc =
             withDilated t (ProjE ix edi) $ \t' ->
               go (ix+1) ts (t':acc)
        in go 0 ls []
                   
    (PackedTy{}) -> do
      pr <- gensym "dilate"
      LetE (pr, dilate ty, edi) <$>
         fn (projVal (VarE pr))

    (SymDictTy _) -> fn edi
    (ListTy _)    -> error$ "withDilated: unfinished: "++ show ty
  

--------------------------------------------------------------------------------

-- We could create an application for a "find-me-this-witness"
-- primitive.  Or we could just allow introduction of unbound
-- variables.
pattern FindEndOf v = AppE "FindEndWitness" (VarE v)
-- pattern FindEndOf v <- AppE "FindEndWitness" (VarE v) where
--   FindEndOf v = VarE (toEndVar v)



-- | If a tuple is returned, how many packed values occur?  This
-- determines the number of output cursors.
countPacked :: Ty1 a -> Int
countPacked ty =
  case ty of
    IntTy  -> 0
    BoolTy -> 0
    (ProdTy x) -> sum $ L.map countPacked x
    (SymDictTy x) | L1.hasPacked x -> error "countPacked: current invariant broken, packed type in dict."
                  | otherwise   -> 0
    (PackedTy x1 x2) -> 1
    (ListTy x)       -> 1
   
isPacked :: Ty1 t -> Bool
isPacked PackedTy{} = True
isPacked _ = False
                        
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
-- witnessOf v = v
witnessOf = ("witness_"++)


tyOfCaseScrut :: Out a => DDefs a -> Exp -> L1.Ty
tyOfCaseScrut dd (CaseE _ ((k,_,_):_)) = PackedTy (getTyOfDataCon dd k) ()
tyOfCaseScrut _ e = error $ "tyOfCaseScrut, takes only Case:\n  "++sdoc e 


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
-- Pure traversal
    -- let go = __ in
    -- case ex of 
    --   VarE v   -> VarE v
    --   LitE n   -> LitE n
    --   AppE v e -> AppE v (go e)
    --   PrimAppE p ls      -> PrimAppE p (L.map go ls)
    --   LetE (v,t,rhs) bod -> LetE (v,t,go rhs) (go bod)
    --   ProjE i e      -> ProjE i (go e)
    --   CaseE e ls     -> CaseE (go e) [ (k,vs,go e) | (k,vs,e) <- ls ]
    --   MkProdE ls     -> MkProdE (L.map go ls)
    --   MkPackedE k ls -> MkPackedE k (L.map go ls)
    --   TimeIt e t     -> TimeIt (go e) t
    --   IfE a b c      -> IfE (go a) (go b) (go c)
    --   MapE (v,t,rhs) bod -> MapE (v,t,go rhs) (go bod)
    --   FoldE (v1,t1,r1) (v2,t2,r2) bod -> FoldE (v1,t1,go r1) (v2,t2,go r2) (go bod)

-- Monadic traversal:
    -- case ex of 
    --   VarE v   -> pure$ VarE v
    --   LitE n   -> pure$ LitE n
    --   AppE v e -> AppE v <$> go e
    --   PrimAppE p ls      -> PrimAppE p <$> mapM go ls
    --   LetE (v,t,rhs) bod -> LetE <$> ((v,t,) <$> go rhs) <*> go bod
    --   ProjE i e      -> ProjE i <$> go e
    --   CaseE e ls     -> CaseE <$> go e <*> sequence
    --                        [ (k,vs,) <$> go e | (k,vs,e) <- ls ]
    --   MkProdE ls     -> MkProdE <$> mapM go ls
    --   MkPackedE k ls -> MkPackedE k <$> mapM go ls
    --   TimeIt e t     -> TimeIt <$> go e <*> pure t
    --   IfE a b c      -> IfE <$> go a <*> go b <*> go c
    --   MapE (v,t,rhs) bod -> MapE <$> ((v,t,) <$> go rhs) <*> go bod
    --   FoldE (v1,t1,r1) (v2,t2,r2) bod ->
    --       FoldE <$> ((v1,t1,) <$> go r1)
    --             <*> ((v2,t2,) <$> go r2)
    --             <*> go bod



-- | This pass must find witnesses if the exist in the lexical
-- environment, and it must *reorder* let bindings to bring start/end
-- witnesses into scope.
findWitnesses :: L2.Prog -> SyM L2.Prog
findWitnesses = L2.mapMExprs fn
 where
  fn _ ex = return (go ex)
  go ex =
    case ex of 
      VarE v   -> VarE v
      LitE n   -> LitE n
      AppE v e -> AppE v (go e)
      PrimAppE p ls      -> PrimAppE p (L.map go ls)
      LetE (v,t,rhs) bod -> LetE (v,t,go rhs) (go bod)
      ProjE i e      -> ProjE i (go e)
      CaseE e ls     -> CaseE (go e) [ (k,vs,go e) | (k,vs,e) <- ls ]
      MkProdE ls     -> MkProdE (L.map go ls)
      MkPackedE k ls -> MkPackedE k (L.map go ls)
      TimeIt e t     -> TimeIt (go e) t
      IfE a b c      -> IfE (go a) (go b) (go c)
      MapE (v,t,rhs) bod -> MapE (v,t,go rhs) (go bod)
      FoldE (v1,t1,r1) (v2,t2,r2) bod -> FoldE (v1,t1,go r1) (v2,t2,go r2) (go bod)

-- =============================================================================

witnessBinding = undefined

-- | Map every lexical variable in scope to an abstract location.
type Env = M.Map Var Loc

    
-- | The goal of this pass is to take effect signatures and translate
-- them into extra arguments and returns.  This pass does not worry
-- about where the witnesses come from to synthesize these extra
-- returns, it just inserts references to them that create demand.
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
                   -- , L1.subst funarg (projNonFirst (length newIn) (L1.VarE fresh))
                   --            funbod
                   , LetE (funarg, fmap (const ()) inT,
                           (projNonFirst (length newIn) (L1.VarE fresh)))
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

                                         
-- Conventions encoded inside the existing Core IR 
-- =============================================================================

pattern NewBuffer = AppE "NewBuffer" (MkProdE [])

-- | output buffer space that is known not to escape the current function.
pattern ScopedBuffer = AppE "ScopedBuffer" (MkProdE [])
                    
-- Tag writing is still modeled by MkPackedE.
pattern WriteInt v e = AppE "WriteInt" (MkProdE [VarE v, e])
-- One cursor in, (int,cursor') output.
pattern ReadInt v = AppE "ReadInt" (VarE v)

pattern CursorTy = PackedTy "CURSOR_TY" () -- Tempx


-- More misc helpers
-- =============================================================================
                   
-- | Project something which had better not be the first thing in a tuple.
projNonFirst :: Int -> L1.Exp -> L1.Exp
projNonFirst 0 e = error $ "projNonFirst: expected nonzero index into expr: "++sdoc e
projNonFirst i e = L1.ProjE i e
