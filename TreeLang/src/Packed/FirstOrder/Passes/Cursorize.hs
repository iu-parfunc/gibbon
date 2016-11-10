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
    , pattern WriteInt, pattern ReadInt, pattern NewBuffer
    , pattern CursorTy, pattern ScopedBuffer
    ) where

import Control.Monad
import Control.Applicative
import           Packed.FirstOrder.Common hiding (FunDef)
import qualified Packed.FirstOrder.L1_Source as L1
import qualified Packed.FirstOrder.LTraverse as L2
import           Packed.FirstOrder.L1_Source (Ty1(..),pattern SymTy)
import           Packed.FirstOrder.LTraverse
    (argtyToLoc, Loc(..), ArrowTy(..), Effect(..), toEndVar, toWitnessVar,
     FunDef(..), Prog(..), Exp(..))

-- We use some pieces from this other attempt:
import Data.Maybe
import qualified Data.Set as S
import Data.List as L hiding (tail)
import Data.Map as M
import Text.PrettyPrint.GenericPretty
import Prelude hiding (exp)

-- | Chatter level for this module:
lvl :: Int
lvl = 4

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
-- every `Packed T` is represented by a pair, `(Cursor,Cursor)`.  At
-- least this is the LOCAL representation of packed values.  The
-- inter-procedural representation does not change.  When passing a
-- packed value to another function, it is the "start" component of
-- the (start,end) pair which is sent.  Likewise end cursors come back
-- traveling on their own.
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
                  do dests <- tyToCursors "globcur" mainTy
                     mkLets [ (cur,CursorTy,NewBuffer)
                            | cur <- allCursors dests ] <$>
                      -- Return the original type:
                      undilate <$> exp2 dests x
             else exp x
  return L2.Prog{ fundefs = M.fromList $ L.map (\f -> (L2.funname f,f)) fds'
                , ddefs = ddefs
                , mainExp = mn
                }
 where
  tyToCursors :: Var -> L1.Ty -> SyM Dests
  tyToCursors nm ty =
    case ty of
      ProdTy ls -> TupOut <$> mapM (tyToCursors nm) ls
      t | L2.isCursorTy t -> Cursor <$> gensym nm
        | otherwise       -> pure NoCursor
                             
  allCursors d = case d of
                   NoCursor -> []
                   Cursor a -> [a]
                   TupOut ls -> concatMap allCursors ls
                              
  mkProdTy [t] = t
  mkProdTy ls  = ProdTy ls
                                
  fd :: L2.FunDef -> SyM L2.FunDef
  fd L2.FunDef{funname,funty,funarg,funbod} =
     dbgTrace lvl (" [cursorDirect] processing fundef "++show(funname,funty)) $ do
     -- We don't add new function arguments yet, rather we leave
     -- unbound references to the function's output cursors, named
     -- "f_1, f_2..." for a function "f".    
     let (funty'@(ArrowTy _ ef outTFull),_) = L2.cursorizeTy2 funty
         -- And without those prepended RouteEnds:
         outT = if S.null ef
                then outTFull
                else let ProdTy ls = outTFull
                     in mkProdTy (L.drop (S.size ef) ls)
     outDests <- tyToCursors funname (fmap (const ()) outT)
         -- outCurs = [ funname ++"_"++ show ix | ix <- [1 .. countPacked outT] ]
     let outCurs  = allCursors outDests
     (arg,exp') <-
           dbgTrace lvl (" [cursorDirect] for function "++funname++" outcursors: "
                         ++show outCurs++" for ty "++show outT) $ 
           case outCurs of
               -- Keep the orginal argument name.
               [] -> (funarg,) <$> exp funbod -- not hasPacked
               -- TODO: handle more than one output cursor:
               [_cur] ->                  
                  do tmp <- gensym "fnarg"
                     -- 1st: Bind (out) cursor arguments:
                     b <- mkLets [ (cur, CursorTy, mkProjE ix (VarE tmp))
                                 | (cur,ix) <- zip outCurs [0..] ] <$>
                           -- 2nd: Unpack the "real" argument, which is after the prepended output cursors:
                           LetE ( funarg
                                , fmap (const ()) (arrIn funty')
                                , mkProjE (length outCurs) (VarE tmp)) <$>
                            -- 3rd: Return value at the original type:
                            undilate <$> exp2 outDests funbod
                     return (tmp,b)
               _ -> error $ "cursorDirect: add support for functionwith multiple output cursors: "++ funname
     return $ L2.FunDef funname funty' arg exp'

  ------------------------------------------------------------

  -- We DONT want to have both witness and "regular" references to the
  -- same variable after this pass.  We need these binders to
  -- have teeth, thus either ALL occurrences must be marked as witnesses, or NONE:             
  -- binderWitness = toWitnessVar
             
  -- TODO: To mark ALL as witnesses we'll need to keep a type
  -- environment so that we can distinguish cursor and non-cursor
  -- values.  For now it's easier to strip all markers:
  binderWitness v = v
                    
  -- | Here we are not in a context that flows to Packed data, thus no
  --   destination cursor.
  exp :: Exp -> SyM Exp
  exp ex0 =
    dbgTrace lvl (" 1. Processing expr in non-packed context, exp:\n  "++sdoc ex0) $ 
    case ex0 of
      VarE _ -> return ex0
      LitE _ -> return ex0
      MkPackedE _ _ -> error$ "cursorDirect: Should not have encountered MkPacked if type is not packed: "++sdoc ex0

      -- If we're not returning a packed type in the current
      -- context, then we can only possibly encounter one that does NOT
      -- escape.  I.e. a temporary one:
      LetE (v,ty,rhs) bod
          | L2.isRealPacked ty -> do tmp <- gensym  "tmpbuf"
                                     rhs2 <- onDi (LetE (tmp,CursorTy,ScopedBuffer)) <$>
                                                exp2 (Cursor tmp) rhs
                                     LetE (v,ty, undilate rhs2) <$> exp bod
                                     -- withDilated ty rhs2 $ \rhs3 ->
                                     --    -- Here we've reassembled the non-dialated view, original type:
                                     --    LetE (v,ty, rhs3) <$> exp bod
          | L2.hasRealPacked ty -> error "cursorDirect: finishme, let bound tuple containing packed."
          | otherwise -> do rhs' <- exp rhs
                            LetE (v,ty,rhs') <$> exp bod

      AppE f e -> do Left e' <- doapp Nothing f e
                     return e'

      PrimAppE p ls -> PrimAppE p <$> mapM exp ls
      ProjE i e  -> mkProjE i <$> exp e
      CaseE scrtE ls -> do
          Left x <- docase Nothing (scrtE,tyOfCaseScrut ddefs ex0) ls
          return x 

      MkProdE ls -> MkProdE <$> mapM exp ls
      TimeIt e t -> TimeIt <$> exp e <*> pure t
      IfE a b c  -> IfE <$> exp a <*> exp b <*> exp c
--        MapE (v,t,rhs) bod -> __
--        FoldE (v1,t1,r1) (v2,t2,r2) bod -> __


  -- | Handle a case expression in packed or unpacked context.  Take a
  -- cursor in the former case and not in the latter.
  docase :: Maybe Dests -> (Exp,L1.Ty) -> [(Constr,[Var],Exp)] -> SyM (Either Exp DiExp)
  docase mcurs (scrtE, _tyScrut) ls = do         
         cur0 <- gensym "cursIn" -- our read cursor

         -- Because the scrutinee is, naturally, of packed type, it
         -- will be represented as a pair of (start,end) pointers.
         -- But we take a little shortcut here and don't bother
         -- creating a new scoped region if we don't need to.
         scrtE' <- if allocFree scrtE
                   then exp scrtE
                   else do tmp <- gensym "scopd"
                           undilate <$>
                            onDi (LetE (tmp,CursorTy,ScopedBuffer)) <$>
                             exp2 (Cursor tmp) scrtE

         dbgTrace 1 (" 3. Case scrutinee "++sdoc scrtE++" alloc free?"++show (allocFree scrtE)) $ return ()
         let mkit e =
              -- Danger: this is an "Exp", but type of this expression
              -- varies based on which mode we're in:
              CaseE e <$>
                (forM ls $ \ (k,vrs,e) -> do
                   let unpackit = unpackDataCon cur0 (k,vrs)
                   e'  <- case mcurs of
                            Nothing -> unpackit =<< exp e
                            Just dc -> unpackit =<< fromDi <$> exp2 dc e
                   return (k,[cur0],e'))
         case mcurs of
           Nothing -> Left       <$> mkit scrtE'
           Just _  -> Right . Di <$> mkit scrtE'

  doapp :: Maybe Dests -> Var -> Exp -> SyM (Either Exp DiExp)
  doapp mcurs f e = do 
          ------------------ Result handling ----------------------
          -- If the function argument is of a packed type, we need to switch modes:
          let at@(ArrowTy argTy _ retTy) = L2.getFunTy fundefs f
              (nat@(ArrowTy newArg _ _newRet),_) = L2.cursorizeTy2 at
              mkapp arg'' =
                 case mcurs of
                   Nothing -> return $ AppE f arg''
                   -- FINISHME: GENERALIZE: need to generalize this to handle arbitrary-in/arbitrary-out combinations.
                   Just (Cursor destCurs) ->
                     -- Here with just one cursor the result type must be (Packed _)
                     case (newArg,retTy) of                       
                       (ProdTy [curt,_],PackedTy{}) | L2.isCursorTy curt -> do                                
                          -- In this particular case, there's only one return val, so we don't need
                          -- to unpack the result. 
                          endCur <- gensym "endCur" -- FIXME: this needs to have a name based on LOC
                          return $
                           -- GENERALIZE: here we need to match N arguments and redilate:
                           LetE (endCur,CursorTy, AppE f (MkProdE [VarE destCurs,arg''])) $
                            -- The calling convention is that we get back the updated cursor.  But
                            -- we are returning from exp2, so we are expected to return a dilated,
                            -- packed value.   Here's the layout for that:           
                            MkProdE [ VarE destCurs -- Starting cursor position becomes value witness.
                                    , VarE endCur ]
                       _ -> error $ "cursorDirect: Need to handle app with ty: "++show nat
          ------------------ Argument handling ----------------------
          new <- case argTy of
                  -- TODO: apply the allocFree trick that we use above.  Abstract it out.
                  PackedTy{} -> do cr <- gensym "argbuf"
                                   LetE (cr,CursorTy,ScopedBuffer) <$> do
                                     e' <- exp2 (Cursor cr) e
                                     mkapp (undilate e')
                                     -- withDilated arg e' $ \e'' -> mkapp e''

                  ty | L1.hasPacked ty -> error $
                          "cursorDirect: need to handle function argument of tupled packed types: "++show ty
                     | otherwise -> mkapp =<< exp e
          -- Restore more type-safety by tagging the output appropriately.
          case mcurs of
            Nothing -> return $ Left new
            Just _  -> return $ Right $ Di new
                                    
                       
  -- | Given a cursor to the position right after the tag, unpack the
  -- fields of a datacon, and return the given expression in that context.
  unpackDataCon :: Var -> (Constr, [Var]) -> Exp -> SyM Exp
  unpackDataCon cur0 (k,vrs) rhs = go cur0 (Just 0) vsts
     where 
       vsts = zip vrs (lookupDataCon ddefs k)
       -- (lastV,_) = L.last vsts

       add 0 e = e
       add n e = PrimAppE L1.AddP [e, LitE n]

       -- Issue reads to get out all the fields:
       go _c _off [] = return rhs -- Everything is now in scope for the body.
                     -- TODO: we could witness the end if we still have the offset.
       go c offset ((vr,ty):rs) = do
         tmp <- gensym "tptmp"
         -- Each cursor position is either the witness of
         -- the next thing, or the witness of the end of the last thing.
         let witNext e =
                 case rs of
                    []       -> e
                    (v2,_):_ -> LetE (binderWitness v2, CursorTy, cdrCursor (VarE tmp)) e
         case ty of
           -- TODO: Generalize to other scalar types:
           IntTy ->
             -- Warning: this is not a dilated type per se, it's a specific record for this prim:
             LetE (tmp, snocCursor IntTy, ReadInt c) <$>
              witNext <$>                  
               LetE (toEndVar vr, CursorTy, cdrCursor (VarE tmp)) <$>
                LetE (vr, IntTy, carVal (VarE tmp)) <$>
                 go (toEndVar vr) (liftA2 (+) (L1.sizeOf IntTy) offset) rs
           ty | isPacked ty -> do
            -- Strategy: ALLOW unbound witness variables. A later traversal will reorder.
            case offset of
              Nothing -> go (toEndVar vr) Nothing rs
              Just n -> LetE (binderWitness vr, CursorTy, add n (VarE cur0)) <$>
                        go (toEndVar vr) Nothing rs


  -- | Take a destination cursor set.  Here we are in a context that
  --   flows to Packed data, we follow a convention of returning an
  --   expression that generates a dilated value.  See `DiExp` below.
  --  
  exp2 :: Dests -> Exp -> SyM DiExp
  exp2 NoCursor ex = dilateTrivial <$> exp ex
  exp2 destC ex0 =
    -- dbgTrace lvl (" 2. Processing expr in packed context, cursor "++show destC++", exp:\n  "++sdoc ex0) $ 
    let go = exp2 destC in
    case ex0 of
      -- Here the allocation has already been performed:
      -- Our variable in the lexical environment is bound to the start only, not (st,en).
      -- To follow the calling convention, we are reponsible for tagging on the end here:
      VarE v -> -- ASSERT: isPacked
          return $ Di $ MkProdE [VarE (binderWitness v), VarE (toEndVar v)] -- FindEndOf v
      LitE _ -> error$ "cursorDirect/exp2: Should not encounter Lit in packed context: "++show ex0

      -- Here's where we write the dest cursor:
      MkPackedE k ls ->
       case destC of
        (Cursor curs) -> do            
          dest' <- gensym "cursplus1_"
          let thetype = PackedTy (getTyOfDataCon ddefs k) ()
          -- This stands for the  "WriteTag" operation:
          Di . LetE (dest', thetype,
                     MkPackedE k [VarE curs]) <$>
             let go2 d [] = return $ MkProdE [VarE curs, VarE d]
                    -- ^ The final return value lives at the position of the out cursor
                 go2 d ((rnd,IntTy):rst) | L1.isTriv rnd = do
                     d'    <- gensym "curstmp"
                     LetE (d', CursorTy, WriteInt d rnd ) <$>
                       (go2 d' rst)
                 -- Here we recursively transfer control
                 go2 d ((rnd,ty@PackedTy{}):rst) = do
                     tup  <- gensym "tup"
                     d'   <- gensym "dest"
                     Di rnd' <- exp2 (Cursor d) rnd
                     LetE (tup, dilateTy thetype, rnd') <$>
                      LetE (d', CursorTy, projCur (Di (VarE tup)) ) <$>
                       (go2 d' rst)
             in go2 dest' (zip ls (lookupDataCon ddefs k))

      -- This is already a witness binding, we leave it alone.
      LetE (v,ty,rhs) bod | L2.isCursorTy ty -> do
         let rhs' = case rhs of
                     PrimAppE {} -> rhs
                     VarE _      -> rhs
                     _           -> error$ "Cursorize: broken assumptions about what a witness binding should look like:\n  "
                                    ++sdoc ex0           
         onDi (LetE (v,ty,rhs')) <$> go bod

      -- For the most part, we just dive under the let and address its body.
      LetE (v,ty, tr) bod | L1.isTriv tr -> onDi (LetE (v,ty,tr))            <$> go bod
      LetE (v,ty, PrimAppE p ls) bod     -> onDi (LetE (v,ty,PrimAppE p ls)) <$> go bod
              
      -- LetE (v1,t1, LetE (v2,t2, rhs2) rhs1) bod ->
      --    go $ LetE (v2,t2,rhs2) $ LetE (v1,t1,rhs1) bod
              
      LetE (v,_,rhs) bod -> error$ "cursorDirect: finish let binding cases in packed context:\n "++sdoc ex0

      -- An application that returns packed values is treated just like a 
      -- MkPackedE constructor: cursors are routed to it, and returned from it.
      AppE v e ->  -- To appear here, the function must have at least one Packed result.
        do Right e <- doapp (Just destC) v e
           return e

      -- This should not be possible.  Types don't work out:
      PrimAppE _ _ -> error$ "cursorDirect: unexpected PrimAppE in packed context: "++sdoc ex0

      -- Here we route the dest cursor to both braches.  We switch
      -- back to the other mode for the (non-packed) test condition.
      IfE a b c  -> do Di b' <- go b
                       Di c' <- go c
                       a'    <- exp a
                       return $ Di $ IfE a' b' c'

      -- An allocating case is just like an allocating If: 
      CaseE scrtE ls ->
          do Right de <- docase (Just destC) (scrtE, tyOfCaseScrut ddefs ex0) ls
             return de
      -- CaseE <$> (projVal <$> go e) <*>
      --                mapM (\(k,vrs,e) -> (k,vrs,) <$> go e) ls

      -- In order to divide-and-conquer, we need navigate our bundle
      -- of output cursors and also recombine the end-cursors returned
      -- from our dilated results.
      MkProdE ls -> do 
        let loop = loop
            tys = __
        case destC of
          TupOut ds -> do
            -- First, we compute all the individual, dialed results:
            es <- mapM (\(dst,e) -> exp2 dst e)
                       (fragileZip ds ls)
            -- Next, we recombine each of the individual dilated values:
            combineDilated (zip ds es)

          _ -> error$ "cursorDirect: MkProdE, cursor argument "++show destC
                      ++ " does not match tuple: "++show ls

     -- ProjE i e  -> ProjE i <$> go e
                        
      TimeIt e t -> do Di e' <- go e
                       return $ Di $ TimeIt e' t

      _ -> error$ "cursorDirect: packed case needs finishing:\n  "++sdoc ex0
-- --        MapE (v,t,rhs) bod -> __
-- --        FoldE (v1,t1,r1) (v2,t2,r2) bod -> __

combineDilated :: [(Dests, DiExp)] -> SyM DiExp
combineDilated ls = do
  let (ds,es) = unzip ls
  bigpkg <- concatProds ds (L.map projCur es)
  return $ 
    Di $ MkProdE [ MkProdE (L.map projVal es)
                 , bigpkg ]

-- Concatenation for tuple values:
concatProds :: [Dests] -> [Exp] -> SyM Exp
concatProds dests prods = do
  let lens = L.map countCursors dests
  flats <- sequence [ gensym "flat" | _ <- prods ]
  -- We let-bind each tuple to avoid code duplication:
  return $
   mkLets [ (flat, mkCursorProd len, pexp)
          | (len,pexp,flat) <- zip3 lens prods flats ]
    -- Then we can build one big tuple expression combining everything:
    (MkProdE
     [ mkProjE ix (VarE flat)
     | (len,flat) <- zip lens flats
     , ix <- [0..(len-1)] ])

-- | The type of end-cursor witness packages is very simple in this representation:
mkCursorProd :: Int -> L1.Ty
mkCursorProd len = ProdTy $ replicate len CursorTy

countCursors :: forall a. Num a => Dests -> a
countCursors (Cursor _) = 1
countCursors NoCursor   = 0
countCursors (TupOut ls) = sum $ L.map countCursors ls
    
-- | A given return context for a type satisfying `hasPacked` either
-- flows to a single cursor or to multiple cursors.
data Dests = Cursor Var
           | NoCursor
           | TupOut [Dests]  -- ^ The layout of this matches the
                             -- ProdTy, but not every field contains a Packed.
 deriving (Eq,Show,Ord,Read) 
             
-------------------------- Dilation Conventions -------------------------------

-- | If an expression `e` returns type `T`, then a dilated version of
-- `e` returns a tuple (T,Cursors), where cursors contains a flat
-- record of end-cursors corresponding exactly to all the components
-- of T which are PackedTy.
-- 
newtype DiExp = Di Exp
--type DiExp = Exp

onDi :: (Exp -> Exp) -> DiExp -> DiExp
onDi f (Di x) = Di (f x)

fromDi :: DiExp -> Exp
fromDi (Di x) = x
                
-- Pairs of (<something>,Cursor) which may not be proper dilated type:
----------------------------------------------------------------------
-- | Utility function: blindly add one cursor to the end.
snocCursor :: Ty1 () -> Ty1 ()
snocCursor ty = ProdTy[ty,CursorTy]

cdrCursor :: Exp -> Exp
cdrCursor = mkProjE 1

carVal :: Exp -> Exp
carVal  = mkProjE 0
----------------------------------------                


-- | Project the cursor package from a dilated expression.             
projCur :: DiExp -> Exp
projCur (Di e) = mkProjE 1 e

-- | Project the original value from a dilated expression.
projVal :: DiExp -> Exp
projVal (Di e) = mkProjE 0 e
            
-- Packed types are gone, replaced by cursPair:
{-
dilate :: Ty1 a -> L1.Ty
dilate ty =
  case ty of
    IntTy  -> IntTy 
    BoolTy -> BoolTy
    (ProdTy x) -> ProdTy $ L.map dilate x
    (SymDictTy x) -> SymDictTy $ dilate x
    (PackedTy _ _) -> cursPairTy
    (ListTy x) -> ListTy $ dilate x
-}

-- | Take a regular type and generate its corresponding dilated type.
-- There are two major choices here.  End cursors can either be ZIPPED
-- and live right next to their corresponding start cursors, or they
-- can reside in a separate package, grouped together.
--
-- This version implements a separate package.
dilateTy :: Show a => Ty1 a -> L1.Ty
dilateTy ty0 = if L.null ls
             then ty'
             else L2.ProdTy [ty', ProdTy ls]
  where
   ty' = fmap (const ()) ty0
   ls = go ty0
   go ty = 
    case ty of
      IntTy  -> []
      BoolTy -> []
      (ProdTy x) -> concatMap go x
      (SymDictTy x) | L1.hasPacked x -> error $ "cursorize: dictionaries containing packed types not allowed yet: "++ show ty
                    | otherwise -> []

      (PackedTy _ _) -> [CursorTy]
--      (ListTy x) -> ListTy $ dilate x


-- | Drop the extra record of cursors on the floor.
undilate :: DiExp -> Exp
undilate = projVal -- redundant, we can get rid of this.

-- | For non-cursor types, dilation is very simple:
dilateTrivial :: Exp -> DiExp
dilateTrivial e = Di $ MkProdE [e, MkProdE []]
                  
dilate :: forall t. t
dilate = __

{-                  
-- | Type-directed dissassembly/reassembly of a dilated value.
--   Take a function that wants to operate on the original portion of the value,
--   ignoring end-cursors.  Assume that the function does NOT change the type,
--   and reattach the end-cursors when it's done.
withDilated :: Show a => Ty1 a -> DiExp -> (Exp -> SyM DiExp) -> SyM DiExp
withDilated ty (Di edi) fn =
  case countPacked ty of
    -- End-cursors are just an empty record:
    0 -> do e' <- fn (projVal edi)
            return $ Di $ MkProdE [e', MkProdE []]
-}
{-    
  -- dbgTrace 5 ("withDilated: "++show ty++", diexp:\n "++sdoc edi) $
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
-}  

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
    -- These can be here from the routeEnds pass:
    -- ty | L2.isCursorTy ty -> 0
    (PackedTy _ _) -> 1
    -- (ListTy _)       -> 1
   
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


tyOfCaseScrut :: Out a => DDefs a -> Exp -> L1.Ty
tyOfCaseScrut dd (CaseE _ ((k,_,_):_)) = PackedTy (getTyOfDataCon dd k) ()
tyOfCaseScrut _ e = error $ "tyOfCaseScrut, takes only Case:\n  "++sdoc e 

mkLets :: [(Var,L1.Ty,Exp)] -> Exp -> Exp
mkLets [] bod = bod
mkLets (b:bs) bod = LetE b (mkLets bs bod)

-- | Smart constructor that immediately destroys products if it can:
mkProjE ix (MkProdE ls) = ls !! ix
mkProjE ix e = ProjE ix e
                    

-- Conventions encoded inside the existing Core IR 
-- =============================================================================

pattern NewBuffer = AppE "NewBuffer" (MkProdE [])

-- | output buffer space that is known not to escape the current function.
pattern ScopedBuffer = AppE "ScopedBuffer" (MkProdE [])
                    
-- | Tag writing is still modeled by MkPackedE.
pattern WriteInt v e = AppE "WriteInt" (MkProdE [VarE v, e])

-- | One cursor in, (int,cursor') output.
pattern ReadInt v = AppE "ReadInt" (VarE v)

pattern CursorTy = PackedTy "CURSOR_TY" () -- Tempx


