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
    ) where

import Control.Monad
import Control.Applicative
import Control.Exception
import           Packed.FirstOrder.Common hiding (FunDef)
import qualified Packed.FirstOrder.L1_Source as L1
import qualified Packed.FirstOrder.L2_Traverse as L2
import           Packed.FirstOrder.L1_Source (mkLets)
import           Packed.FirstOrder.L2_Traverse
import           Packed.FirstOrder.Passes.InlinePacked (pattern NamedVal)
import           Packed.FirstOrder.Passes.Flatten (typeExp, TEnv)
-- We use some pieces from this other attempt:
import Data.Maybe
import Data.Tuple (swap)
import qualified Data.Set as S
import Data.List as L hiding (tail)
import Data.Map as M
import Text.PrettyPrint.GenericPretty
import Prelude hiding (exp)

-- | Baseline chatter level for this module:
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
-- least, this is the LOCAL representation of packed values.  The
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
--   (1) In function types, CursorTy occurs only in end-witnesses.
--   (2) Let-bindings for end-witnesses use a restricted set of forms.
--
--  POSTCONDITIONS:
--   (1) The NamedVal pattern is gone.
--
cursorDirect :: L2.Prog -> SyM L2.Prog
cursorDirect prg0@L2.Prog{ddefs,fundefs,mainExp} = do
  ---- Mostly duplicated boilerplate with other cursorize ----
  ------------------------------------------------------------
  fds' <- mapM fd $ M.elems fundefs
  
  mn <- case mainExp of
          Nothing -> return Nothing
          Just (x,mainTy) -> Just . (,mainTy) <$>
             if L1.hasPacked mainTy
             then -- Allocate into a global cursor:
                  do dests <- tyToCursors "globcur" mainTy
                     mkLets [ (cur,CursorTy (),NewBuffer)
                            | cur <- allCursors dests ] <$>
                         -- Return the original type:
                         undilate <$> exp2 M.empty True dests x
             else exp M.empty True x
  return L2.Prog{ fundefs = M.fromList $ L.map (\f -> (L2.funname f,f)) fds'
                , ddefs = ddefs
                , mainExp = mn
                }
 where
  env2 = L2.progToEnv prg0
  recoverType = typeExp (ddefs,env2)

  -- | Create a let binding, but only after type-checking the RHS.
  --   EXPENSIVE: another source of quadratic behavior.
  -- 
  --   NOTE: even though we 
  safeLet :: TEnv -> (Var,L1.Ty,Exp) -> Exp -> Exp
  safeLet tenv (v,t,rhs) bod =
    let received = recoverType tenv rhs
        strip = L2.mapPacked (\_ l -> L2.mkCursorTy l)
    in
    if strip received == strip t
    then LetE (v,t,rhs) bod
    else error $ "safeLet: expected type: "++show t++" when binding, got "
             ++show received++"\n RHS expression: "++sdoc rhs

  -- | Which output types constitute new output params.
  tyToCursors :: Var -> L1.Ty -> SyM Dests
  tyToCursors nm ty =
    case ty of
      ProdTy ls -> TupOut <$> mapM (tyToCursors nm) ls
      t | L2.isRealPacked t  -> Cursor <$> gensym ("out_"++nm)
        | otherwise          -> pure NoCursor
                             
  allCursors d = case d of
                   NoCursor -> []
                   Cursor a -> [a]
                   TupOut ls -> concatMap allCursors ls
                                                              
  fd :: L2.FunDef -> SyM L2.FunDef
  fd L2.FunDef{funname,funty,funarg,funbod} =
     dbgTrace (lvl) (" [cursorDirect] processing fundef "++show(funname,funty)) $ do
     -- We don't add new function arguments yet, rather we leave
     -- unbound references to the function's output cursors, named
     -- "f_1, f_2..." for a function "f".    
     let (ArrowTy _ _ oldOut) = funty
         (funty'@(ArrowTy inT ef newOutFull),newIn) = L2.cursorizeTy2 funty
         -- And without those prepended RouteEnds:
         oldCoreOut = getCoreOutTy funty
         -- newoutT    = fmap toEndVar oldCoreOut -- Flip ins to outs.
     -- Enumerate packed types in our core output:
     outDests0 <- tyToCursors funname (fmap (const ()) oldCoreOut)
     -- We do not provide a cursor param for end-witnesses added by RouteEnds
     let outDests = mkTupOut$ (L.replicate (S.size ef) NoCursor) ++ [outDests0]

     let outDilated = dilateTy oldOut
                    
     let initEnv = M.singleton funarg (fmap (const ()) inT)
     let outCurs  = allCursors outDests
     (arg,exp') <-
           dbgTrace lvl (" [cursorDirect] for function "++funname++" outDests: "
                         ++show outDests++", outCurs "++show outCurs++", newIn "
                         ++show newIn++",\n,  old return ty (undilated): "
                         ++show oldOut++"\n   and dilated: "++show (dilateTy oldOut)
                         ++ ",\n   old core return type: "++show oldCoreOut
                        ) $ 
           case outCurs of
               -- Keep the orginal argument name.
               [] -> (funarg,) <$> exp initEnv False funbod -- not hasPacked
               -- TODO: handle more than one output cursor:
               [_cur] ->                  
                  do fnargtmp <- gensym "fnarg" 
                     -- 1st: Bind (out) cursor arguments:
                     b <- mkLets [ (cur, CursorTy (), mkProjE2 ix (length outCurs+1) (VarE fnargtmp)) -- outCurs non null.
                                 | (cur,ix) <- zip outCurs [0..] ] <$>
                           -- 2nd: Unpack the "real" argument, which is after the prepended output cursors:
                           let argTy = fmap (const ()) (arrIn funty') in
                           LetE ( funarg, argTy
                                , mkProjE2 (length outCurs) (length outCurs+1) (VarE fnargtmp)) <$> do
                            let tenv = M.union (M.fromList ((funarg,argTy):(zip outCurs (repeat (CursorTy())))))
                                               initEnv
                            -- 3rd: Bind the result of the function body so we can operate on it:
                            Di bod2 <- exp2 tenv False outDests funbod
                            btmp <- gensym "bodtmp"
                            safeLet tenv (btmp, outDilated, bod2) <$> do
                              -- 4th: separate updated input cursors from the core return type:
                              upds <- sequence$ replicate (S.size ef) (gensym "updtCurs" )
                              mkLets [ (upd, CursorTy (), ProjE ix (VarE btmp))
                                     | (ix,upd) <- zip [0..] upds ] <$> do
                               -- 5th: perform surgery on the tuple, replacing the "start" values in
                               -- the output with corresponding "end" values from the dilated representation:
                               -- This works on the DILATED rep, and returns another dilated.
                               newCore <- spliceUpdatedCursors
                                          (Di (VarE btmp)) -- (ProjE (S.size ef) (VarE btmp))
                                          (fmap (const ()) oldCoreOut) -- core type, non-dialated, no end-wits.
                               -- 6th: Return value at the type newOutFull, including updated input cursors:
                               return $
                                  dbgTrace 5 ("Finished wrapping up body: newOutFull/dilated "
                                              ++ ndoc newOutFull ++" / "++ ndoc outDilated 
                                              ++ "\n core starts at "++show (ProjE (S.size ef) (VarE btmp))) $
                                  -- 7th: At the very end, attach end-witnesses:
                                  L1.mkProd $ [ VarE u | u <- upds ] ++ [newCore]
                     return (fnargtmp,b)
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
  exp :: TEnv -> Bool -> Exp -> SyM Exp
  exp tenv isMain ex0 =
    let go = exp tenv isMain in 
    dbgTrace (lvl+1) (" 1. Processing expr in non-packed context, exp:\n  "++sdoc ex0) $ 
    case ex0 of
      VarE _ -> return ex0
      LitE _ -> return ex0
      MkPackedE _ _ -> error$ "cursorDirect: Should not have encountered MkPacked if type is not packed: "++ndoc ex0

      NamedVal _ _ _ -> error$ "cursorDirect: only expected NamedVal convention in packed context: "++ndoc ex0
                       
      -- If we're not returning a packed type in the current
      -- context, then we can only possibly encounter one that does NOT
      -- escape.  I.e. a temporary one:
      LetE (v,ty,rhs) bod
          | L2.isRealPacked ty -> do tmp <- gensym  "tmpbuf"
                                     rhs2 <- let tenv' = M.insert tmp (CursorTy ()) tenv in
                                             onDi (LetE (tmp, CursorTy (), chooseBuffer isMain)) <$>
                                                exp2 tenv' isMain (Cursor tmp) rhs
                                     -- POLICY: we leave the original name bound at the original type:
                                     LetE (v,ty, undilate rhs2) <$>
                                        exp (M.insert v ty tenv) isMain bod
                                     -- withDilated ty rhs2 $ \rhs3 ->
                                     --    -- Here we've reassembled the non-dialated view, original type:
                                     --    LetE (v,ty, rhs3) <$> go bod
          | L2.hasRealPacked ty -> error "cursorDirect: finishme, let bound tuple containing packed."
          | otherwise -> do rhs' <- go rhs
                            LetE (v,ty,rhs') <$>
                              exp (M.insert v ty tenv) isMain bod

      L2.AddCursor _ _ -> return ex0
      _ | L2.isExtendedPattern ex0 -> error$ "cursorDirect/exp: Unhandled extended L2 pattern: "++ndoc ex0 
      AppE f e -> do Left e' <- doapp [] tenv isMain Nothing f e
                     return e'

      PrimAppE p ls -> PrimAppE p <$> mapM go ls
      ProjE i e  -> ProjE i <$> go e
      CaseE scrtE ls -> do
          Left x <- docase tenv isMain Nothing (scrtE,tyOfCaseScrut ddefs ex0) ls
          return x 

      MkProdE ls -> MkProdE <$> mapM go ls
      TimeIt e t b -> TimeIt <$> go e <*> pure t <*> pure b
      IfE a b c  -> IfE <$> go a <*> go b <*> go c

      MapE (_v,_t,_rhs) _bod -> error "FINISHME - Cursorize - list support"
      FoldE {} -> error "FINISHME - Cursorize - list support" --(v1,t1,r1) (v2,t2,r2) bod -> __

  chooseBuffer isMain = if isMain
                        then NewBuffer
                        else ScopedBuffer

  -- | Handle a case expression in packed or unpacked context.  Take a
  -- cursor in the former case and not in the latter.
  docase :: TEnv -> Bool -> Maybe Dests -> (Exp,L1.Ty) -> [(Constr,[Var],Exp)] -> SyM (Either Exp DiExp)
  docase tenv isMain mcurs (scrtE, _tyScrut) ls = do         
         cur0 <- gensym "cursIn" -- our read cursor

         -- Because the scrutinee is, naturally, of packed type, it
         -- will be represented as a pair of (start,end) pointers.
         -- But we take a little shortcut here and don't bother
         -- creating a new scoped region if we don't need to.
         scrtE' <- if allocFree scrtE
                   then exp tenv isMain scrtE
                   else do tmp <- gensym "scopd"
                           LetE (tmp, CursorTy (), chooseBuffer isMain) <$>
                             undilate <$>
                               exp2 (M.insert tmp (CursorTy ()) tenv) isMain (Cursor tmp) scrtE

         dbgTrace 2 (" 3. Case scrutinee "++sdoc scrtE++" alloc free?="++show (allocFree scrtE)) $ return ()
         let mkit e =
              -- Danger: this is an "Exp", but type of this expression
              -- varies based on which mode we're in:
              CaseE e <$>
                (forM ls $ \ (k,vrs,e) -> do
                   let unpackit = unpackDataCon cur0 (k,vrs)
                       vsts = zip vrs (lookupDataCon ddefs k)
                       tenv' = M.union (M.fromList vsts) tenv
                   e'  <- case mcurs of
                            Nothing -> unpackit =<< exp tenv' isMain e
                            Just dc -> unpackit =<< fromDi <$> exp2 tenv' isMain dc e
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
  doapp :: ProjStack -> TEnv -> Bool -> Maybe Dests -> Var -> Exp -> SyM (Either Exp DiExp)
  doapp prjstk _tenv isMain mcurs f argE =  
          ------------------ Result handling ----------------------
          -- If the function argument is of a packed type, we may need to switch modes:
          let at@(ArrowTy argTy ef _retTy) = L2.getFunTy fundefs f
              (nat@(ArrowTy newArgT _ newRetT),_) = L2.cursorizeTy2 at
              --                                     ^ ASSERT:  retTy == newRetT 
              numNewOut = S.size ef                      -- end witnesses
              numNewIn  = countPacked (getCoreOutTy at) -- output cursors

          in dbgTrace (lvl+1) (" 3. doApp, rator= "++ show f ++", argTy= "++show argTy++", arg: "++ndoc argE) $ 
          do 
                          
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
                               let flp _ce (PackedTy _k l) =
                                     -- Because we grabbed these from `at` instead of `nat`, we don't need to fromEndV:
                                     case getArgPos l newArgT of 
                                       Just pos -> VarE $ cursMap # pos
                                       Nothing -> error $ "function application: couldn't find witness to location "++show l
                                                          ++ " among arguments of type "++show newArgT 
                                   flp ce (ProdTy ts) = L1.mkProd [ flp (ProjE ix ce) t | (ix,t) <- zip [0..] ts]
                                   flp ce _ = ce
                                   flpd = dbgTrace 5 (" [cursorize] flipping the sense of return type: "++
                                                      show coreT++", context: "++ take 80 (show coreE)++"...")
                                          flp coreE coreT
                                   (_endWitsET, (coreE,coreT)) = splitFunResult at (VarE tmp)
                                   
                                   -- While flp eliminates the normal return values, they are
                                   -- exactly what we want for the second half of the dilated value.
                                   -- But we need to flatten the tuple structure, because that's our current dilation convention.
                                   flat pos (PackedTy _ l) = [(pos,l)]
                                   flat pos (ProdTy ts) = concat [ flat (ix:pos) t
                                                                 | (ix,t) <- zip [0..] ts ]
                                   flat _  _ = []

                                   -- This ASSUMES that the end witnesses will be only cursors, and those cursors will
                                   -- not be reflected in the dilated type (just like, say Ints):
                                   allEnds = [ buildProjE ps (mkProjE2 numNewOut (1+numNewOut) (VarE tmp))
                                             | (ps,_lc) <- flat [] coreT ]
                                   
                                   restoreEndWits e = L1.mkProd $
                                                       [ mkProjE2 ix (1+numNewOut) (VarE tmp)
                                                       | ix <- [0..numNewOut-1] ]
                                                      ++ [e]
                               in
                                dbgTrace minChatLvl
                                     ("\n>>>>>> [cursorDirect] doapp/mkapp: building APP: "
                                      ++show (prjstk,isMain,mcurs, f, argE)++"\n  final type: "++show nat
                                      ++"\n  fulldests: "++show fullDests++"\n endVars: "++show endVs
                                      ++"\n argPoss: "++show (argPoss)
                                      ++"\n augmented arg: "++sdoc aarg
                                      ++"\n final type projected: "++show finalTy
                                      ++"\n core ret before prjstk / without witnesses: "++sdoc flpd
                                      ++"\n"
                                      )$
                                 -- buildProjE prjstk $
                                   L1.mkProd [ restoreEndWits flpd
                                             , L1.mkProd allEnds ]

          ------------------ Argument handling ----------------------
          -- Here we handle the evaluation of the *original* input arguments.
          new <- case argTy of
                  PackedTy{}
                      | allocFree argE -> mkapp =<< exp _tenv isMain argE
                      | otherwise -> do cr <- gensym "argbuf"
                                        LetE (cr, CursorTy (), chooseBuffer isMain) <$> do
                                          e' <- exp2 _tenv isMain (Cursor cr) argE
                                          mkapp (undilate e')
                                          -- withDilated arg e' $ \e'' -> mkapp e''

                  ty | L1.hasPacked ty -> error $
                          "cursorDirect: need to handle function argument of tupled packed types: "++show ty
                     | otherwise -> mkapp =<< exp _tenv isMain argE
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

       add v 0 = VarE v
       add v n = AddCursor v n

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
                    (v2,_):_ -> LetE (binderWitness v2, CursorTy (), end_this) e
             go2 = -- Do the type-specific reading of the fields:
              case ty of
                -- TODO: Generalize to other scalar types:
                IntTy ->
                  -- Warning: this is not a dilated type per se, it's a specific record for this prim:
                  LetE (tmp, snocCursor IntTy, ReadInt c) <$>
                    LetE (toEndVar vr, CursorTy (), cdrCursor (VarE tmp)) <$>
                     LetE (vr, IntTy, carVal (VarE tmp)) <$>
                       let gorst = go (toEndVar vr) (liftA2 (+) (L1.sizeOf IntTy) offset) rs
                       in if offset == Nothing
                          then witNext (cdrCursor (VarE tmp)) <$> gorst
                          else gorst -- If offset is still static, don't need to to help our dowstream find themselves.
                ty | isPacked ty -> 
                 -- Strategy: ALLOW unbound witness variables. A later traversal will reorder.
                 witNext (VarE (toEndVar vr)) <$> 
                   go (toEndVar vr) Nothing rs

                oth -> error$ "Cursorize: don't yet know how to read this type from buffer: "++show oth -- FINISHME
                   
         -- No matter what type of field is next, we always prefer a static witness:
         case offset of
            -- Statically sized, we know right where it is:
            Just n  -> LetE (binderWitness vr, CursorTy (), add cur0 n) <$> go2              
            -- Dynamically sized, we can still chain things
            -- together, but we don't know the answer straight out.
            Nothing -> go2


  -- | Take a destination cursor set.  Here we are in a context that
  --   flows to Packed data, we follow a convention of returning an
  --   expression that generates a dilated value.  See `DiExp` below.
  --  
  exp2 :: TEnv -> Bool -> Dests -> Exp -> SyM DiExp
  exp2 tenv isMain NoCursor ex = dilateTrivial <$> exp tenv isMain ex
  exp2 tenv isMain destC ex0 =
    dbgTrace (lvl+1) (" 2. Processing expr in packed context, cursor "++show destC++", exp:\n  "++sdoc ex0) $ 
    let go tenv = exp2 tenv isMain destC
        -- | Projections in the packed case.  RETURNS DILATED.
        doproj :: ProjStack -> Int -> Exp -> SyM DiExp
        doproj stk ix ex          
          | (MkProdE ls)         <- ex  = case stk of
                                            []     -> go tenv (ls !! ix)
                                            (i:is) -> doproj is i (ls !! ix)
          | (ProjE i2 e2)        <- ex  = doproj (ix:stk) i2 e2
          -- | (NamedVal vr ty val) <- ex  = onDi (\e -> LetE (vr,ty,e) (VarE vr)) <$>
          --                                  (doproj stk ix val)

          | (NamedVal vr ty val) <- ex = do Di val' <- doproj stk ix val
                                            tmp     <- gensym "dila2_"
                                            return $ Di $ LetE (tmp, dilateTy ty, val') $ 
                                                          LetE (vr, ty, undilate (Di (VarE tmp)))
                                                           (VarE tmp)

          | L2.isExtendedPattern ex     = error$ "cursorDirect/exp2: Unhandled extended L2 pattern: "++ndoc ex
          | AppE f arg           <- ex  =
             -- A function call plus projecting one of its results.  Here, WHICH result we project
             -- determines which tree output we consume, and thus which output cursor we connect.
             -- Because of our projection, our current cursor argument corresponds to the type of the
             -- projected component, not the type of the whole function result.
             do Right di <- doapp (ix:stk) tenv isMain (Just destC) f arg
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
       let Cursor curs = destC in 
       do dest' <- gensym "cursplus1_"
          let thetype = PackedTy (getTyOfDataCon ddefs k) ()
          -- This stands for the  "WriteTag" operation:
          Di . LetE (dest', thetype,
                     MkPackedE k [VarE curs]) <$>
             let go2 d [] = return $ MkProdE [VarE curs, VarE d]
                    -- ^ The final return value lives at the position of the out cursor
                 go2 d ((rnd,IntTy):rst) | L1.isTriv rnd = do
                     d'    <- gensym "curstmp"
                     LetE (d', CursorTy (), WriteInt d rnd ) <$>
                       (go2 d' rst)
                 -- Here we recursively transfer control
                 go2 d ((rnd,ty@PackedTy{}):rst) = do
                     tup  <- gensym "fldtup"
                     d'   <- gensym "flddst"
                     Di rnd' <- exp2 (M.insert dest' thetype tenv) isMain (Cursor d) rnd
                     LetE (tup, dilateTy thetype, rnd') <$>
                      LetE (d', CursorTy (), projCur (Di (VarE tup)) ) <$>
                       (go2 d' rst)
             in go2 dest' (zip ls (lookupDataCon ddefs k))

      -- Eliminate this form, while leaving bindings around.
      NamedVal nm ty val -> do Di val' <- go tenv val
                               -- return $ Di $ LetE (nm, dilateTy ty, val') (VarE nm)
                                       
                               -- Here we bind to the UNDILATED value, as elsewhere:
                               tmp <- gensym ("dila_"++nm)
                               return $ Di $ LetE (tmp, dilateTy ty, val') $
                                              LetE (nm, ty, undilate (Di (VarE tmp))) $
                                               (VarE tmp) -- We RETURN dilated, which is a postcondition
                
      -- This is already a witness binding, we leave it alone.
      LetE (v,ty,rhs) bod | L2.isCursorTy ty -> do
         -- We do NOT dilate the cursor types:
         if isWitnessExpr rhs
         then onDi (LetE (v,ty,rhs)) <$> go (M.insert v ty tenv) bod
         else error$ "Cursorize: broken assumptions about what a witness binding should look like:\n  "
                                    ++sdoc ex0           
                                      
      -- For the most part, we just dive under the let and address its body.
      LetE (v,ty, tr) bod | L1.isTriv tr -> onDi (LetE (v,ty,tr))            <$> go (M.insert v ty tenv) bod

      -- The only primitive that returns packed data is ReadPackedFile:
      LetE (v,ty, PrimAppE (L1.ReadPackedFile _ _) []) bod ->
         error "[Cursorize] FINISH ReadPackedFile"

      LetE (v,ty, PrimAppE p ls) bod     -> onDi (LetE (v,ty,PrimAppE p ls)) <$> go (M.insert v ty tenv) bod
              
      -- LetE (v1,t1, LetE (v2,t2, rhs2) rhs1) bod ->
      --    go $ LetE (v2,t2,rhs2) $ LetE (v1,t1,rhs1) bod
              
      LetE bnd _ -> error$ "cursorDirect: finish let binding cases in packed context:\n "++sdoc bnd

      -- An application that returns packed values is treated just like a 
      -- MkPackedE constructor: cursors are routed to it, and returned from it.
      AppE v e ->  -- To appear here, the function must have at least one Packed result.
        do Right e <- doapp [] tenv isMain (Just destC) v e
           return e

      -- This should not be possible.  Types don't work out:
      PrimAppE _ _ -> error$ "cursorDirect: unexpected PrimAppE in packed context: "++sdoc ex0

      -- Here we route the dest cursor to both braches.  We switch
      -- back to the other mode for the (non-packed) test condition.
      IfE a b c  -> do Di b' <- go tenv b
                       Di c' <- go tenv c
                       a'    <- exp tenv isMain a
                       return $ Di $ IfE a' b' c'

      -- An allocating case is just like an allocating If: 
      CaseE scrtE ls ->
          do Right de <- docase tenv isMain (Just destC) (scrtE, tyOfCaseScrut ddefs ex0) ls
             return de
      -- CaseE <$> (projVal <$> go e) <*>
      --                mapM (\(k,vrs,e) -> (k,vrs,) <$> go e) ls

      -- In order to divide-and-conquer, we need to navigate our bundle
      -- of output cursors and also recombine the end-cursors returned
      -- from our dilated results.
      MkProdE ls -> do 
        case destC of
          TupOut ds -> do
            -- First, we compute all the individual, dilated results:
            es <- mapM (\(dst,e) -> exp2 tenv isMain dst e)
                       (fragileZip ds ls)
            -- Next, we regroup and combine the front and end values:
            combineDilated (recoverType tenv) (zip ds es)

          _ -> error$ "cursorDirect:\n  MkProdE, cursor argument "++show destC
                      ++ "\n  does not match tuple: "++show ls

                         
      -- We should probably run the unariser before this pass:
      ProjE i ex -> doproj [] i ex
                        
      TimeIt e t b -> do Di e' <- go tenv e
                         return $ Di $ TimeIt e' t b

      MapE{}  -> error$ "cursorDirect: packed case needs finishing:\n  "++sdoc ex0
      FoldE{} -> error$ "cursorDirect: packed case needs finishing:\n  "++sdoc ex0
-- --        MapE (v,t,rhs) bod -> __
-- --        FoldE (v1,t1,r1) (v2,t2,r2) bod -> __

-- | Takes ALREADY-PROCESSED, dilated expressions and munges them.
combineDilated :: (Exp -> L1.Ty) -> [(Dests, DiExp)] -> SyM DiExp
combineDilated getType ls =
  dbgTrace 5 ("combineDilated:\n "++show ls) $ 
 do
  -- First, let-bind non-trivials:
  let (ds,es) = unzip ls
  ls <- forM es $ \ (Di e) ->
          if L1.isTriv e -- These can still be triv, even with tuples.
          then return ([],Di e)
          else do tmp <- gensym "combdil"
                  return ([(tmp, getType e, e)], Di (VarE tmp))
  let (bnds,es') = unzip ls

  -- Make one big, flattened product of cursors:
  bigpkg <- concatProds ds (L.map projCur es')
  return $ 
    Di $ mkLets (concat bnds) $
         MkProdE [ L1.mkProd (L.map projVal es')
                 , bigpkg ]

-- | Concatenation for tuple exprs in the output language of this pass.
--   Assumes TRIVIAL expressions as input.
concatProds :: [Dests] -> [Exp] -> SyM Exp
concatProds dests prods =
  dbgTrace 5 ("concatProds:\n "++sdoc (dests,prods)) $ 
 do
  let lens = L.map countCursors dests
  -- flats <- sequence [ gensym "flat" | _ <- prods ]
  -- We let-bind each tuple to avoid code duplication:
  let result = 
        -- mkLets [ (flat, mkCursorProd len, pexp)
        --        | (len,pexp,flat) <- zip3 lens prods flats ]
         -- Then we can build one big tuple expression combining everything:
         (L1.mkProd
          [ mkProjE2 ix len x -- (VarE x)
          | (len,x) <- zip lens prods -- flats
          , ix <- [0..(len-1)] ])
  dbgTrace 5 ("concatProds/result:\n  "++sdoc result) $
   return result
         
-- | The type of end-cursor witness packages is very simple in this representation:
mkCursorProd :: Int -> L1.Ty
mkCursorProd len = ProdTy $ replicate len (CursorTy ())

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
 deriving (Eq,Show,Ord,Read, Generic)

instance Out Dests
             
                



-- Pairs of (<something>,Cursor) which may not be proper dilated type:
----------------------------------------------------------------------
-- | Utility function: blindly add one cursor to the end.
snocCursor :: Ty1 () -> Ty1 ()
snocCursor ty = ProdTy[ty, CursorTy ()]

cdrCursor :: Exp -> Exp
cdrCursor = mkProjE 1

carVal :: Exp -> Exp
carVal  = mkProjE 0
----------------------------------------                


-- ================================================================================
--                         Dilation Conventions
-- ================================================================================
-- Everything to do with dilation.  It should be possible to change
-- the dilated format by changing only this section.


-- | If an expression `e` returns type `T`, then a dilated version of
-- `e` returns a tuple (T,Cursors), where cursors contains a flat
-- record of end-cursors corresponding exactly to all the components
-- of T which are PackedTy.
-- 
newtype DiExp = Di Exp
  deriving (Generic, Out, Show, Read, Eq, Ord)
--type DiExp = Exp

onDi :: (Exp -> Exp) -> DiExp -> DiExp
onDi f (Di x) = Di (f x)

fromDi :: DiExp -> Exp
fromDi (Di x) = x


-- | Project the cursor package from a dilated expression.             
projCur :: DiExp -> Exp
projCur (Di e) = mkProjE 1 e

-- | Project the original value from a dilated expression.
projVal :: DiExp -> Exp
projVal (Di e) = mkProjE 0 e
            

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
   tls = L.map (const (CursorTy ())) $ allPackedTys ty0

   -- | Provide a flat list of all the packed types in a type (preorder traversal).
   --   Does ** NOT ** include cursor types.
   allPackedTys :: Show a => Ty1 a -> [a]
   allPackedTys ty = 
       case ty of
         IntTy  -> []
         BoolTy -> []
         (ProdTy x) -> concatMap allPackedTys x
         (SymDictTy x) | L1.hasPacked x -> error $ "cursorize: dictionaries containing packed types not allowed yet: "++ show ty
                       | otherwise -> []
         (CursorTy l) -> [] -- NOT COUNTING.
         (PackedTy _ l) -> [l]
   --      (ListTy x) -> ListTy $ dilate x
         

-- | Drop the extra record of cursors on the floor.
undilate :: DiExp -> Exp
undilate = projVal -- redundant, we can get rid of this.

-- | For non-cursor types, dilation is very simple:
dilateTrivial :: Exp -> DiExp
dilateTrivial e = Di $ MkProdE [e, MkProdE []]

           
-- | Undilate throws away the end-cursors, but when we return from a
-- function, the calling convention demands something different, that
-- we return updated output cursors in place of each of the original
-- packed-value outputs.
--
-- This takes (1) a trivial expression referring the DILATED core return value.
-- And (2), the type of the CORE return value.
--
-- This returns the UNDILATED core value, after the splice.
spliceUpdatedCursors :: DiExp -> L1.Ty -> SyM Exp
spliceUpdatedCursors (Di trv) ty0
    | not (L1.hasPacked ty0) = return trv                     
    | otherwise =
        dbgTrace 6 ("spliceUpdatedCursors, processing "++show(trv,ty0)
                   ++" val/ends refs: "++show(valStart,endsStart)) $ 
        return final
    -- If there are no packed types, there's nothing to splice.                            
  where
   final = go [] orig 0 
   ProdTy [orig, endCurs]  = dilateTy ty0 
   allEndCurs = toList endCurs
                             
   toList (ProdTy ls) = ls
   toList t = [t]

   valStart  = projVal (Di trv)
   endsStart = projCur (Di trv)   

   -- Reference a path containing a regular val within the original value.
   refVal [] = valStart
   refVal (ix:is) = ProjE ix (refVal is)
   -- Reference a path containing a end witness, based on its index.
   refEnds :: Int -> Exp
   refEnds ix = mkProjE2 ix (length allEndCurs) endsStart
   
   -- Takes the type of the expression under focus, starting with the
   -- core (undilated) type.  As we loop we track both the PATH in the
   -- nested type, and the FLAT INDEX in the flattened, dilated cursor set.
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

-- | Type-directed dissassembly/reassembly of a dilated value.
--   Take a function that wants to operate on the original portion of the value
--   (ignoring dilation/end-cursors).  Assume that the function does NOT change the type,
--   and reattach the end-cursors when it's done.
withDilated :: Show a => Ty1 a -> DiExp -> (Exp -> SyM Exp) -> SyM DiExp
withDilated ty (Di edi) fn =
  __


                   
                 
-- ================================================================================  
-- End dilation details
-- ================================================================================  


-- | Split a function result into (1) end witnesses, (2) core value.  Given a trivial expression
--   representing the function return value, return new trivial expressions representing these
--   subcomponents.
--   This should be applied to the function types BEFORE cursorize.
splitFunResult :: ArrowTy L2.Ty -> Exp -> ([(Exp,L2.Ty)], (Exp, L2.Ty))
splitFunResult arrT@(ArrowTy _ ef _) et =
    ( [ (ProjE ix et, ty)
      | (ix,ty) <- zip [0..] witTys ]
    , (ProjE (S.size ef) et, coreTy) )
  where    
    coreTy = getCoreOutTy arrT
    witTys = getEndWitTys arrT

-- | Strip off the end-witness returns and give the core return type.
--   Works only on the output types PRIOR to the cursorize transformation.
getCoreOutTy :: forall a. Show a => ArrowTy (Ty1 a) -> Ty1 a
getCoreOutTy at@(ArrowTy _inT ef outTFull) =
    assertNoCursors (show at) $ 
    if S.null ef
    then outTFull
    else let ProdTy ls = outTFull
         in mkProdTy (L.drop (S.size ef) ls)

assertNoCursors :: Show a => String -> Ty1 a -> Ty1 a
assertNoCursors s t =
  if L2.hasCursorTy t
  then error $ " expected no cursor types, found: "++show t++"\n Context: "++s
  else t
            
-- | Retrieve a list of the end-witnesses
getEndWitTys :: forall a. ArrowTy (Ty1 a) -> [Ty1 a]
getEndWitTys (ArrowTy _ ef outTFull) = 
    if S.null ef
    then []
    else let ProdTy ls = outTFull
             res = L.take (S.size ef) ls
         in
         assert (all L2.isCursorTy res)
             res
         
          
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
    (CursorTy _)   -> 0 -- Doesn't go into the count.
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

   -- The one primitive that allocates packed data!
   (PrimAppE (L1.ReadPackedFile _ _) _x2) -> False                        
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

   (AddCursor _ _)   -> True -- ^ For adding offsets.
   -- (PrimAppE L1.AddP _x2) -> True -- ^ For adding offsets.  

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

-- | Smart constructor that immediately destroys products if it can:
--   Does NOT avoid single-element tuples.
mkProjE :: Int -> Exp -> Exp
mkProjE ix (MkProdE ls) = ls !! ix
mkProjE ix e = ProjE ix e

-- | Safer verison of `mkProjE`.  Ensures that it does not try to
-- reference a unary "tuple".
mkProjE2 :: Int -> Int -> Exp -> Exp
mkProjE2 0 1 e = e
mkProjE2 i l e | i >= l = error $ "internal error, out-of bounds mkProj: "
                          ++show i++" length "++show l++" expression:\n  "++sdoc e
mkProjE2 ix _ (MkProdE ls) = ls !! ix
mkProjE2 ix _ e = ProjE ix e
               

               
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
                           
                                           
               
