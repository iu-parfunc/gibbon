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
import           Packed.FirstOrder.Passes.InlinePacked (pattern NamedVal)
-- We use some pieces from this other attempt:
import Data.Maybe
import Data.Tuple (swap)
import qualified Data.Set as S
import Data.List as L hiding (tail)
import Data.Map as M
import Text.PrettyPrint.GenericPretty
import Prelude hiding (exp)

-- | Chatter level for this module:
lvl :: Int
lvl = 4

type ProjStack = [Int]
      
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
--  PRECONDITIONS:
--
--
--  POSTCONDITIONS:
--   (1) The NamedVal pattern is gone.
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
                         undilate <$> exp2 True dests x
             else exp True x
  return L2.Prog{ fundefs = M.fromList $ L.map (\f -> (L2.funname f,f)) fds'
                , ddefs = ddefs
                , mainExp = mn
                }
 where
  tyToCursors :: Var -> L1.Ty -> SyM Dests
  tyToCursors nm ty =
    case ty of
      ProdTy ls -> TupOut <$> mapM (tyToCursors nm) ls
      t | L2.isCursorTy t    -> Cursor <$> gensym nm
        | otherwise          -> pure NoCursor
                             
  allCursors d = case d of
                   NoCursor -> []
                   Cursor a -> [a]
                   TupOut ls -> concatMap allCursors ls
                                                              
  fd :: L2.FunDef -> SyM L2.FunDef
  fd L2.FunDef{funname,funty,funarg,funbod} =
     dbgTrace lvl (" [cursorDirect] processing fundef "++show(funname,funty)) $ do
     -- We don't add new function arguments yet, rather we leave
     -- unbound references to the function's output cursors, named
     -- "f_1, f_2..." for a function "f".    
     let (funty'@(ArrowTy _ ef outTFull),_) = L2.cursorizeTy2 funty
         -- And without those prepended RouteEnds:
         outT = getCoreOutTy funty'
     -- Enumerate packed types in our core output:
     outDests0 <- tyToCursors funname (fmap (const ()) outT)
     -- We do not provide a cursor param for end-witnesses added by RouteEnds
     let outDests = mkTupOut$ (L.replicate (S.size ef) NoCursor) ++ [outDests0]
                            

     let outCurs  = allCursors outDests
     (arg,exp') <-
           dbgTrace lvl (" [cursorDirect] for function "++funname++" outDests: "
                         ++show outDests++" for full return ty (undilated): "++show outTFull) $ 
           case outCurs of
               -- Keep the orginal argument name.
               [] -> (funarg,) <$> exp False funbod -- not hasPacked
               -- TODO: handle more than one output cursor:
               [_cur] ->                  
                  do tmp <- gensym "fnarg"
                     -- 1st: Bind (out) cursor arguments:
                     b <- mkLets [ (cur, CursorTy, mkProjE ix (VarE tmp))
                                 | (cur,ix) <- zip outCurs [0..] ] <$>
                           -- 2nd: Unpack the "real" argument, which is after the prepended output cursors:
                           LetE ( funarg
                                , fmap (const ()) (arrIn funty')
                                , mkProjE (length outCurs) (VarE tmp)) <$> do
                            -- 3rd: Bind the result of the function body so we can operate on it:
                            Di bod2 <- exp2 False outDests funbod
                            btmp <- gensym "bodtmp"
                            LetE (btmp, dilateTy outTFull, bod2) <$> do
                              -- 4th: separate updated input cursors from the core return type:
                              upds <- sequence$ replicate (S.size ef) (gensym "updtCurs" )
                              mkLets [ (upd, CursorTy, ProjE ix (VarE btmp))
                                     | (ix,upd) <- zip [0..] upds ] <$> do
                               -- 5th: perform surgery on the tuple, replacing the "start" values in
                               -- the output with corresponding "end" values from the dilated representation:
                               newCore <- spliceUpdatedCursors
                                          (Di (VarE btmp)) -- (ProjE (S.size ef) (VarE btmp))
                                          (fmap (const ()) outT)
                               -- 6th: Return value at the type outTFull, including updated input cursors:
                               return $
                                  dbgTrace 5 ("Finished wrapping up body: outTFull/dilated "
                                              ++ show (outTFull, dilateTy outTFull)
                                              ++ " core starts at "++show (ProjE (S.size ef) (VarE btmp))) $
                                  L1.mkProd $ [ VarE u | u <- upds ] ++ [newCore]
                     return (tmp,b)
               _ -> error $ "cursorDirect: add support for functionwith multiple output cursors: "++ funname
     return $ L2.FunDef funname funty' arg exp'

  ------------------------------------------------------------

  -- We DONT want to have both witness and "regular" references to the
  -- same variable after this pass.  We need these binders to
  -- have teeth, thus either ALL occurrences must be marked as witnesses, or NONE:             
  -- binderWitness = toWitnessVar
  -- 
  -- TODO: To mark ALL as witnesses we'll need to keep a type
  -- environment so that we can distinguish cursor and non-cursor
  -- values.  For now it's easier to strip all markers:
  binderWitness v = v
                    
  -- | Here we are not in a context that flows to Packed data, thus no
  --   destination cursor.
  exp :: Bool -> Exp -> SyM Exp
  exp isMain ex0 =
    let go = exp isMain in 
    dbgTrace lvl (" 1. Processing expr in non-packed context, exp:\n  "++sdoc ex0) $ 
    case ex0 of
      VarE _ -> return ex0
      LitE _ -> return ex0
      MkPackedE _ _ -> error$ "cursorDirect: Should not have encountered MkPacked if type is not packed: "++sdoc ex0

      NamedVal _ _ _ -> error$ "cursorDirect: only expected NamedVal convention in packed context:\n "++sdoc ex0
                       
      -- If we're not returning a packed type in the current
      -- context, then we can only possibly encounter one that does NOT
      -- escape.  I.e. a temporary one:
      LetE (v,ty,rhs) bod
          | L2.isRealPacked ty -> do tmp <- gensym  "tmpbuf"
                                     rhs2 <- onDi (LetE (tmp,CursorTy,chooseBuffer isMain)) <$>
                                                exp2 isMain (Cursor tmp) rhs
                                     LetE (v,ty, undilate rhs2) <$> go bod
                                     -- withDilated ty rhs2 $ \rhs3 ->
                                     --    -- Here we've reassembled the non-dialated view, original type:
                                     --    LetE (v,ty, rhs3) <$> go bod
          | L2.hasRealPacked ty -> error "cursorDirect: finishme, let bound tuple containing packed."
          | otherwise -> do rhs' <- go rhs
                            LetE (v,ty,rhs') <$> go bod

      AppE f e -> do Left e' <- doapp [] isMain Nothing f e
                     return e'

      PrimAppE p ls -> PrimAppE p <$> mapM go ls
      ProjE i e  -> mkProjE i <$> go e
      CaseE scrtE ls -> do
          Left x <- docase isMain Nothing (scrtE,tyOfCaseScrut ddefs ex0) ls
          return x 

      MkProdE ls -> MkProdE <$> mapM go ls
      TimeIt e t b -> TimeIt <$> go e <*> pure t <*> pure b
      IfE a b c  -> IfE <$> go a <*> go b <*> go c
--        MapE (v,t,rhs) bod -> __
--        FoldE (v1,t1,r1) (v2,t2,r2) bod -> __

  chooseBuffer isMain = if isMain
                        then NewBuffer
                        else ScopedBuffer

  -- | Handle a case expression in packed or unpacked context.  Take a
  -- cursor in the former case and not in the latter.
  docase :: Bool -> Maybe Dests -> (Exp,L1.Ty) -> [(Constr,[Var],Exp)] -> SyM (Either Exp DiExp)
  docase isMain mcurs (scrtE, _tyScrut) ls = do         
         cur0 <- gensym "cursIn" -- our read cursor

         -- Because the scrutinee is, naturally, of packed type, it
         -- will be represented as a pair of (start,end) pointers.
         -- But we take a little shortcut here and don't bother
         -- creating a new scoped region if we don't need to.
         scrtE' <- if allocFree scrtE
                   then exp isMain scrtE
                   else do tmp <- gensym "scopd"
                           undilate <$>
                            onDi (LetE (tmp,CursorTy,chooseBuffer isMain)) <$>
                             exp2 isMain (Cursor tmp) scrtE

         dbgTrace 1 (" 3. Case scrutinee "++sdoc scrtE++" alloc free?="++show (allocFree scrtE)) $ return ()
         let mkit e =
              -- Danger: this is an "Exp", but type of this expression
              -- varies based on which mode we're in:
              CaseE e <$>
                (forM ls $ \ (k,vrs,e) -> do
                   let unpackit = unpackDataCon cur0 (k,vrs)
                   e'  <- case mcurs of
                            Nothing -> unpackit =<< exp isMain e
                            Just dc -> unpackit =<< fromDi <$> exp2 isMain dc e
                   return (k,[cur0],e'))
         case mcurs of
           Nothing -> Left       <$> mkit scrtE'
           Just _  -> Right . Di <$> mkit scrtE'

  -- | The very tricky handling of an application+context, `(let (y1,..yn) = f (x1,..xm) in yi)`,
  --   which includes matching dest cursor(s) to `xi`, and finally dilating the result `yi`.
  --
  --  Inputs:
  --     (1) prjstk - encodes how to extract `yi` from the result tuple.
  --     (3) mcurs  - destination(s) to which `yi` must flow
  --  
  --   Note that the provided package of cursors corresponds only to the output of `yi`.
  doapp :: ProjStack -> Bool -> Maybe Dests -> Var -> Exp -> SyM (Either Exp DiExp)
  doapp prjstk isMain mcurs f argE = do
          ------------------ Result handling ----------------------
          -- If the function argument is of a packed type, we may need to switch modes:
          let at@(ArrowTy argTy ef retTy) = L2.getFunTy fundefs f
              (nat@(ArrowTy newArgT _ newRetT),_) = L2.cursorizeTy2 at
              --                                     ^ ASSERT:  retTy == newRetT 
              numNewOut = S.size ef                      -- end witnesses
              numNewIn  = countPacked (getCoreOutTy at) -- output cursors

          -- Destinations including results not projected.  With the
          -- current InlinePacked strategy, we push constructors and
          -- constructing functions into a syntactic context where
          -- they can only produce output to one destination.  Thus we are limited here:
          let fullDests = let lp stk _ | stk == prjstk = let Just c = mcurs in c
                              lp stk (ProdTy ls) = TupOut [ lp (ix:stk) t | (ix,t) <- zip [0..] ls ]
                              lp _ _ = NoCursor -- Limitation!
                          in lp [] newRetT
              finalTy = flipEnds $ projTy prjstk newRetT

          -- Now we need to use the type work backwards through the
          -- function call and find the output-arguments that should receive the data.
          -- First, we retrieve the endVars that correspond to our destinations:
          let getEndVs :: Dests -> L2.Ty -> [(Var,LocVar)]
              getEndVs (TupOut l1) (ProdTy l2) = concat $ zipWith getEndVs l1 l2
              getEndVs (Cursor dst) ty -- @(PackedTy _ l)
                  | L2.isCursorTy ty = [(dst,L2.cursorTyLoc ty)]                                                              
                  -- | L2.isCursorTy ty = error$ "cursorDirect/doapp: did not expect cursor to correspond to destination cursor: "
                  --                             ++ show (dst, ty)
                  -- | otherwise = [(dst,l)]
              getEndVs NoCursor _ = []                                    
              getEndVs d t = error $ "cursorDirect/doapp: should not need a destination cursor ("
                             ++ show d++ ") for this type: "++show t

              -- Relate each cursor var to the abstract end-location that flows to/through it
              endVs = getEndVs fullDests newRetT

              getArgPos :: LocVar -> L2.Ty -> Maybe [Int]
              getArgPos locv ty | L2.isCursorTy ty = if L2.cursorTyLoc ty == locv 
                                                     then Just []
                                                     else Nothing
              getArgPos locv (ProdTy ls) =
                  case catMaybes [ fmap (ix:) (getArgPos locv t)
                                 | (ix,t) <- zip [0..] ls ] of
                    []      -> Nothing
                    (hit:_) -> Just hit
              getArgPos _ _ = Nothing

              -- For each cursor var, figure out which argument it must flow to in the function call inputs.
              argPoss = L.map (\(d,v) ->
                                let Just ev  = L2.fromEndVar v
                                    Just pos = getArgPos ev newArgT
                                in (d,pos)) endVs

              -- Map positions in the argument list, to the cursor values that fill them:
              cursMap = M.fromList (L.map swap argPoss)

              -- Finalyl, take the core argument and augment it with extra positions.
              augmentArg ae = L1.mkProd $ [ VarE (cursMap # [ix]) | ix <- [0..numNewIn-1]] ++ [ae]
                          
          let -- Build the AppE and handle results.  This takes the original arg (type argTy), but
              -- must augment it with output cursors to make something of type newArgT.
              mkapp arg'' =
                 -- Attach the cursor arguments:
                 let aarg = augmentArg arg'' in
                 dbgTrace 1
                      ("\n>>>>>> [cursorDirect] doapp/mkapp: building APP: "
                       ++show (prjstk,isMain,mcurs, f, argE)++"\n  final type: "++show nat
                       ++"\n  fulldests: "++show fullDests++"\n endVars: "++show endVs
                       ++"\n argPoss: "++show (argPoss)
                       ++"\n augmented arg: "++sdoc aarg
                       ++"\n final type projected: "++show finalTy
                       )$
                 -- The return context is ALREADY expecting the extra args, after RouteEnds:
                 case mcurs of                   
                   Nothing -> return $ AppE f aarg -- But here no dilation is expected
                   _ -> do tmp <- gensym "unpkcall"
                           return $
                            -- No change needed for the end-Witness cursors, only the core:
                                  
                           -- Our total call result includes (1) end witnesses, (2) out-cursor updates.
                           -- We need to take those and build the dilated result.
                            LetE (tmp, l1Ty newRetT, AppE f aarg) $
                               -- Here we impedence match between conventions.  The function returns
                               -- updated out-cursors to us, but we need to flip the sense of all of
                               -- those to instead return start-of-value witnesses.
                               let flp ce (PackedTy k l) =
                                     case getArgPos (fromEndV l) newArgT of 
                                       Just pos -> VarE $ cursMap # pos
                                       Nothing -> error $ "function application: couldn't find witness to location "++show (fromEndV l)
                                                          ++ " amoung arguments of type "++show newArgT 
                                   flp ce (ProdTy ts) = L1.mkProd [ flp (ProjE ix ce) t | (ix,t) <- zip [0..] ts]
                                   flp ce _ = ce
                                   flpd = flp coreE coreT                                         
                                   (_endWitsET, (coreE,coreT)) = splitFunResult nat (VarE tmp)
                                   
                                   -- While flp eliminates the normal return values, they are
                                   -- exactly what we want for the second half of the dilated value.
                                   -- But we need to flatten the tuple structure, because that's our current dilation convention.
                                   flat pos (PackedTy _ l) = [(pos,l)]
                                   flat pos (ProdTy ts) = concat [ flat (ix:pos) t
                                                                 | (ix,t) <- zip [0..] ts ]
                                   flat _  _ = []

                                   allEnds = [ buildProjE ps (L1.mkProj numNewOut (1+numNewOut) (VarE tmp))
                                             | (ps,_lc) <- flat [] coreT ]
                               in
                                buildProjE prjstk $ 
                                 L1.mkProd [ flpd, L1.mkProd allEnds ]

          ------------------ Argument hanling ----------------------
          -- Here we handle the evaluation of the *original* input arguments.
          new <- case argTy of
                  PackedTy{}
                      | allocFree argE -> mkapp =<< exp isMain argE
                      | otherwise -> do cr <- gensym "argbuf"
                                        LetE (cr,CursorTy,chooseBuffer isMain) <$> do
                                          e' <- exp2 isMain (Cursor cr) argE
                                          mkapp (undilate e')
                                          -- withDilated arg e' $ \e'' -> mkapp e''

                  ty | L1.hasPacked ty -> error $
                          "cursorDirect: need to handle function argument of tupled packed types: "++show ty
                     | otherwise -> mkapp =<< exp isMain argE
          ------------------------------------------------------------
          -- Restore more type-safety by tagging the output appropriately.
          case mcurs of
            Nothing -> return $ Left new
            Just _  -> return $ Right $ Di new
                                    
                       
  -- | Given a cursor to the position right after the tag, unpack the
  -- fields of a datacon, and return the given expression in that context.
  -- This also has the job of inserting `end_x2==start_x1` witnesses.
  unpackDataCon :: Var -> (Constr, [Var]) -> Exp -> SyM Exp
  unpackDataCon cur0 (k,vrs) rhs =
      dbgTrace 5 ("unpackDataCon: "++show(cur0, (k,vsts))) $
      go cur0 (Just 0) vsts
     where 
       vsts = zip vrs (lookupDataCon ddefs k)
       -- (lastV,_) = L.last vsts

       add 0 e = e
       add n e = PrimAppE L1.AddP [e, LitE n]

       -- Loop over fields.  Issue reads to get out all the fields:
       go _c _off [] = return rhs -- Everything is now in scope for the body.
                     -- TODO: we could witness the end if we still have the offset.
       go c offset ((vr,ty):rs) = do
         tmp <- gensym "tptmp"
         let -- Each end-cursor position is either the witness of
             -- the next thing, or the witness of the end of the last thing:
             witNext :: Exp -> Exp -> Exp 
             witNext end_this e =
               dbgTrace 5 ("WITNESS NEXT: "++show(vr,rs, end_this)) $
                 case rs of
                    []       -> e
                    (v2,_):_ -> LetE (binderWitness v2, CursorTy, end_this) e
             go2 = -- Do the type-specific reading of the fields:
              case ty of
                -- TODO: Generalize to other scalar types:
                IntTy ->
                  -- Warning: this is not a dilated type per se, it's a specific record for this prim:
                  LetE (tmp, snocCursor IntTy, ReadInt c) <$>
                    LetE (toEndVar vr, CursorTy, cdrCursor (VarE tmp)) <$>
                     LetE (vr, IntTy, carVal (VarE tmp)) <$>
                       let gorst = go (toEndVar vr) (liftA2 (+) (L1.sizeOf IntTy) offset) rs
                       in if offset == Nothing
                          then witNext (cdrCursor (VarE tmp)) <$> gorst
                          else gorst -- If offset is still static, don't need to to help our dowstream find themselves.
                ty | isPacked ty -> 
                 -- Strategy: ALLOW unbound witness variables. A later traversal will reorder.
                 witNext (VarE (toEndVar vr)) <$> 
                   go (toEndVar vr) Nothing rs
                                                 
         -- No matter what type of field is next, we always prefer a static witness:
         case offset of
            -- Statically sized, we know right where it is:
            Just n  -> LetE (binderWitness vr, CursorTy, add n (VarE cur0)) <$> go2              
            -- Dynamically sized, we can still chain things
            -- together, but we don't know the answer straight out.
            Nothing -> go2


  -- | Take a destination cursor set.  Here we are in a context that
  --   flows to Packed data, we follow a convention of returning an
  --   expression that generates a dilated value.  See `DiExp` below.
  --  
  exp2 :: Bool -> Dests -> Exp -> SyM DiExp
  exp2 isMain NoCursor ex = dilateTrivial <$> exp isMain ex
  exp2 isMain destC ex0 =
    -- dbgTrace lvl (" 2. Processing expr in packed context, cursor "++show destC++", exp:\n  "++sdoc ex0) $ 
    let go = exp2 isMain destC
        -- | Projections in the packed case.
        doproj stk ix ex          
          | (MkProdE ls)         <- ex  = case stk of
                                            []     -> go (ls !! ix)
                                            (i:is) -> doproj is i (ls !! ix)
          | (ProjE i2 e2)        <- ex  = doproj (ix:stk) i2 e2
          | (NamedVal vr ty val) <- ex  = onDi (\e -> LetE (vr,ty,e) (VarE vr)) <$>
                                           (doproj stk ix val)
          | AppE f arg           <- ex  =
             -- A function call plus projecting one of its results.  Here, WHICH result we project
             -- determines which tree output we consume, and thus which output cursor we connect.
             -- Because of our projection, our current cursor argument corresponds to the type of the
             -- projected component, not the type of the whole function result.
             do Right di <- doapp (ix:stk) isMain (Just destC) f arg
                return di

          | otherwise = 
                error$ "cursorDirect: copy missing, cannot route cursors to:\n  "++sdoc ex0

    in
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
                     Di rnd' <- exp2 isMain (Cursor d) rnd
                     LetE (tup, dilateTy thetype, rnd') <$>
                      LetE (d', CursorTy, projCur (Di (VarE tup)) ) <$>
                       (go2 d' rst)
             in go2 dest' (zip ls (lookupDataCon ddefs k))

      -- Eliminate this form, while leaving bindings around.
      NamedVal nm ty val -> do Di val' <- go val
                               tmp <- gensym "dsgNamed"
                               return $ Di $ LetE (tmp, dilateTy ty, val') $
                                              LetE (nm, ty, undilate (Di (VarE tmp))) $
                                               (VarE nm)
                
      -- This is already a witness binding, we leave it alone.
      LetE (v,ty,rhs) bod | L2.isCursorTy ty -> do
         if isWitnessExpr rhs
         then onDi (LetE (v,ty,rhs)) <$> go bod
         else error$ "Cursorize: broken assumptions about what a witness binding should look like:\n  "
                                    ++sdoc ex0           
                                      
      -- For the most part, we just dive under the let and address its body.
      LetE (v,ty, tr) bod | L1.isTriv tr -> onDi (LetE (v,ty,tr))            <$> go bod
      LetE (v,ty, PrimAppE p ls) bod     -> onDi (LetE (v,ty,PrimAppE p ls)) <$> go bod
              
      -- LetE (v1,t1, LetE (v2,t2, rhs2) rhs1) bod ->
      --    go $ LetE (v2,t2,rhs2) $ LetE (v1,t1,rhs1) bod
              
      LetE bnd _ -> error$ "cursorDirect: finish let binding cases in packed context:\n "++sdoc bnd

      -- An application that returns packed values is treated just like a 
      -- MkPackedE constructor: cursors are routed to it, and returned from it.
      AppE v e ->  -- To appear here, the function must have at least one Packed result.
        do Right e <- doapp [] isMain (Just destC) v e
           return e

      -- This should not be possible.  Types don't work out:
      PrimAppE _ _ -> error$ "cursorDirect: unexpected PrimAppE in packed context: "++sdoc ex0

      -- Here we route the dest cursor to both braches.  We switch
      -- back to the other mode for the (non-packed) test condition.
      IfE a b c  -> do Di b' <- go b
                       Di c' <- go c
                       a'    <- exp isMain a
                       return $ Di $ IfE a' b' c'

      -- An allocating case is just like an allocating If: 
      CaseE scrtE ls ->
          do Right de <- docase isMain (Just destC) (scrtE, tyOfCaseScrut ddefs ex0) ls
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
            es <- mapM (\(dst,e) -> exp2 isMain dst e)
                       (fragileZip ds ls)
            -- Next, we recombine each of the individual dilated values:
            combineDilated (zip ds es)

          _ -> error$ "cursorDirect:\n  MkProdE, cursor argument "++show destC
                      ++ "\n  does not match tuple: "++show ls

                         
      -- We should probably run the unariser before this pass:
      ProjE i ex -> doproj [] i ex
                        
      TimeIt e t b -> do Di e' <- go e
                         return $ Di $ TimeIt e' t b

      MapE{}  -> error$ "cursorDirect: packed case needs finishing:\n  "++sdoc ex0
      FoldE{} -> error$ "cursorDirect: packed case needs finishing:\n  "++sdoc ex0
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

mkTupOut :: [Dests] -> Dests
mkTupOut [o] = o
mkTupOut ls  = TupOut ls
                           
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
dilateTy ty0 = if L.null tls
               then ty'
               else L1.mkProdTy [ty', L1.mkProdTy tls]
  where
   ty' = fmap (const ()) ty0
   tls = L.map (const CursorTy) $ allPackedTys ty0

-- | Provide a flat list of all the packed types in a type (preorder traversal)
allPackedTys :: Show a => Ty1 a -> [a]
allPackedTys ty = 
    case ty of
      IntTy  -> []
      BoolTy -> []
      (ProdTy x) -> concatMap allPackedTys x
      (SymDictTy x) | L1.hasPacked x -> error $ "cursorize: dictionaries containing packed types not allowed yet: "++ show ty
                    | otherwise -> []
      (PackedTy _ l) -> [l]
--      (ListTy x) -> ListTy $ dilate x


-- | Drop the extra record of cursors on the floor.
undilate :: DiExp -> Exp
undilate = projVal -- redundant, we can get rid of this.

-- | Undilate throws away the end-cursors, but when we return from a
-- function, the calling convention demands something different, that
-- we return updated output cursors in place of each of the original
-- packed-value outputs.
--
-- This takes (1) a trivial expression referring the DILATED core return value.
-- And (2), the type of the core return value.
spliceUpdatedCursors :: DiExp -> L1.Ty -> SyM Exp
spliceUpdatedCursors (Di trv) ty0
    | not (L1.hasPacked ty0) = return trv                     
    | otherwise =
        dbgTrace 6 ("spliceUpdatedCursors, processing "++show(trv,ty0)
                   ++" val/ends refs: "++show(valStart,endsStart)) $ 
        return $ go [] orig 0 
    -- If there are no packed types, there's nothing to splice.                            
  where 
   ProdTy [orig, endCurs]  = dilateTy ty0 
   allEndCurs = toList endCurs
                             
   toList (ProdTy ls) = ls
   toList t = [t]

   valStart  = projVal (Di trv)
   endsStart = projCur (Di trv)   

   refVal [] = valStart
   refVal (ix:is) = ProjE ix (refVal is)

   refEnds :: Int -> Exp
   refEnds ix = L1.mkProj ix (length allEndCurs) endsStart
                    
   go :: [Int] -> L1.Ty -> Int -> Exp
   go stk ty endIx =
    dbgTrace 6 ("splice loop "++show (stk,ty,endIx)) $ 
    case ty of
     -- Replace with the value from the endCursors
     (PackedTy _ _) -> refEnds endIx
     -- Divide and conquer:
     (ProdTy flds) -> L1.mkProd
                      [ go (ix:stk) fld
                           (endIx + sum (L.map countPacked (take ix flds)))
                      | (ix,fld) <- zip [0..] flds ]
      
     IntTy         -> refVal stk
     BoolTy        -> refVal stk
     (SymDictTy _) -> refVal stk                
     (ListTy _) -> error "spliceUpdatedCursors -- unfinished, list case"


                   
-- | For non-cursor types, dilation is very simple:
dilateTrivial :: Exp -> DiExp
dilateTrivial e = Di $ MkProdE [e, MkProdE []]

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


-- | Split a function result into (1) end witnesses, (2) core value.  Given a trivial expression
--   representing the function return value, return new trivial expressions representing these
--   subcomponents.
splitFunResult :: ArrowTy L2.Ty -> Exp -> ([(Exp,L2.Ty)], (Exp, L2.Ty))
splitFunResult arrT@(ArrowTy _ ef _) et =
    ( [ (ProjE ix et, ty)
      | (ix,ty) <- zip [0..] witTys ]
    , (ProjE (S.size ef) et, coreTy) )
  where    
    coreTy = getCoreOutTy arrT
    witTys = getEndWitTys arrT

-- | Strip off the end-witness returns and give the core return type.
getCoreOutTy :: forall a. ArrowTy (Ty1 a) -> Ty1 a
getCoreOutTy (ArrowTy inT ef outTFull) = 
    if S.null ef
    then outTFull
    else let ProdTy ls = outTFull
         in mkProdTy (L.drop (S.size ef) ls)

getEndWitTys :: forall a. ArrowTy (Ty1 a) -> [Ty1 a]
getEndWitTys (ArrowTy _ ef outTFull) = 
    if S.null ef
    then []
    else let ProdTy ls = outTFull
         in L.take (S.size ef) ls

            
mkProdTy :: forall a. [Ty1 a] -> Ty1 a
mkProdTy [t] = t
mkProdTy ls  = ProdTy ls            

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
   (VarE _x)         -> True
   (LitE _x)         -> True
   (PrimAppE _x1 _x2) -> True

   (AppE _x1 _x2)     -> False                       
   (MkPackedE _x1 _x2) -> False

   (NamedVal _ _ e)   -> allocFree e
   (LetE (_,_,e1) e2) -> allocFree e1 && allocFree e2
   (IfE x1 x2 x3) -> allocFree x1 && allocFree x2 && allocFree x3
   (ProjE _ x2)   -> allocFree x2
   (MkProdE x)    -> all allocFree x
   (CaseE x1 x2)  -> allocFree x1 && all (\(_,_,e) -> allocFree e) x2
   (TimeIt e _ _) -> allocFree e
   (MapE (_,_,x1) x2) -> allocFree x1 && allocFree x2 
   (FoldE (_,_,x1) (_,_,x2) x3) -> allocFree x1 && allocFree x2 && allocFree x3

-- | Due to global invariants on the compiler, there are only certain
-- expressions that can serve as witnesses to an abstract location.
isWitnessExpr :: Exp -> Bool 
isWitnessExpr = go 
 where
  go ex =
   case ex of   
   (VarE _x)         -> True
   (LitE _x)         -> True
   (PrimAppE _x1 _x2) -> True -- ^ For adding offsets.

   (NamedVal _ _ e)  -> go e
   (LetE (_,_,e1) e2) -> go e1 && go e2
   (ProjE _ x2)   -> go x2

   (AppE _x1 _x2)      -> False
   (MkPackedE _x1 _x2) -> False                         
   (IfE _x1 _x2 _x3)    -> False 
   (MkProdE x)    -> False -- all go x
   (CaseE x1 x2)  -> False -- go x1 && all (\(_,_,e) -> go e) x2
   (TimeIt e _ _) -> False -- go e
   (MapE (_,_,x1) x2) -> False -- go x1 && go x2 
   (FoldE (_,_,x1) (_,_,x2) x3) -> False -- go x1 && go x2 && go x3


                                   
tyOfCaseScrut :: Out a => DDefs a -> Exp -> L1.Ty
tyOfCaseScrut dd (CaseE _ ((k,_,_):_)) = PackedTy (getTyOfDataCon dd k) ()
tyOfCaseScrut _ e = error $ "tyOfCaseScrut, takes only Case:\n  "++sdoc e 

mkLets :: [(Var,L1.Ty,Exp)] -> Exp -> Exp
mkLets [] bod = bod
mkLets (b:bs) bod = LetE b (mkLets bs bod)

-- | Smart constructor that immediately destroys products if it can:
mkProjE :: Int -> Exp -> Exp
mkProjE ix (MkProdE ls) = ls !! ix
mkProjE ix e = ProjE ix e
                    
l1Ty :: L2.Ty -> L1.Ty
l1Ty = fmap (const ())


projTy :: Show t => ProjStack -> Ty1 t -> Ty1 t
projTy [] t = t
projTy (ix:is) (ProdTy ls) = projTy is (ls!!ix)
projTy stk t = error $ "cursorize/projTy: could not project indices "++show stk++" from type "++show t

-- | Recursively build a projection               
buildProjE :: ProjStack -> Exp -> Exp
buildProjE [] e = e
buildProjE (ix:is) e = buildProjE is (ProjE ix e)
               
-- | Flip end markers to start markers:
flipEnds :: L2.Ty -> L2.Ty
flipEnds ty =
  case ty of
    IntTy  -> IntTy
    BoolTy -> BoolTy
    (ProdTy x) -> ProdTy $ L.map flipEnds x
    (SymDictTy x) | L1.hasPacked x -> error "countPacked: current invariant broken, packed type in dict."
                  | otherwise   -> ty
    (PackedTy k l) -> case L2.fromEndVar l of
                        Just v  -> PackedTy k v
                        Nothing -> PackedTy k l
    -- (ListTy _)  -> 

fromEndV :: LocVar -> LocVar
fromEndV v = case L2.fromEndVar v of
                 Just v2 -> v2
                 Nothing -> error$ "fromEndVar: expected end-var, got: "++show v
                           
                                           
               
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

       
