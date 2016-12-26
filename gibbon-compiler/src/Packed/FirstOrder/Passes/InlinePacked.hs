-- | Aggressive inliner to put Packed-typed expressions syntactically
-- under constructors that they flow to.

-- WARNING: DUPLICATED code from InlineTriv.hs

module Packed.FirstOrder.Passes.InlinePacked
    ( inlinePacked
    , pattern NamedVal)
    where

import qualified Data.Map as M    
import Packed.FirstOrder.Common (SyM, Var, dbgTrace, sdoc, lookupDataCon, DDefs)
import qualified Packed.FirstOrder.L1_Source as L1
import Packed.FirstOrder.L2_Traverse as L2
import Prelude hiding (exp)

-- | This pass gets ready for cursorDirect by pushing tree-creating
-- expressions within the syntactic scope of data constructor
-- applications.
--
-- STARTING INVARIANTS:
-- 
--   (1) Variables used only as witnesses are marked, and we NEVER
--      inline Datacons into those locations.  We should already be
--      thinking of those just as pointers.
--  
-- ENDING INVARIANTS:
-- 
--   (1) unbound variables may occur in the RHS of (LetE (_,CursorTy,_))
--       bindings, but nowwhere else.
--
--   (2) "witness" marked vars no longer occur. These markings have
--       served their purpose and are stripped.
--
inlinePacked :: L2.Prog -> SyM L2.Prog
inlinePacked prg@L2.Prog{ddefs,fundefs,mainExp} = return $
  prg { fundefs = M.map fd fundefs 
      , mainExp = case mainExp of
                    Nothing      -> Nothing
                    (Just (e,t)) -> Just (inlinePackedExp ddefs [] e, t)
      }
 where
   fd f@FunDef{funarg, funty= ArrowTy inT _ _, funbod} =
       f { funbod = inlinePackedExp ddefs
                    [(funarg, (fmap (const ()) inT, Nothing))]
                    funbod }

-- | Keep a map of the entire lexical environment, but only part of it
-- is inlinable. (I.e. function arguments are not.)
--
-- The policy at the momoent is to inline ONLY `isConstructor`
-- bindings, and not to remove 
inlinePackedExp :: DDefs L1.Ty -> [(Var,(L1.Ty, Maybe L1.Exp))] -> L1.Exp -> L1.Exp
inlinePackedExp ddefs = exp True
  where

  -- After this pass, these markers have served their purpose and can go away:
  var :: Var -> Var
  var v | isWitnessVar v = let Just v' = fromWitnessVar v in v'
        | otherwise = v

  -- | THis takes a flag indicating how aggressively to inline.  We
  -- have a special case where for end-var witnesses we don't inline as much.
  --
  -- Here the environment contains code that has NOT yet been recursively processed.
  exp :: Bool -> [(Var,(L1.Ty,Maybe L1.Exp))] -> L1.Exp -> L1.Exp
  exp strong env e0 =
   let go = exp strong env in
   -- dbgTrace 5 ("Inline "++show strong++", processing with env:\n   "++
   --             show [v | (v,(_,Just _)) <- env]++"\n   EXP: "++show e0) $
   case e0 of
    -- Here the witness variable markers intentionally prevent inlining:
    (VarE v) -> case lookup v env of
                  Nothing -> dbgTrace 1 ("WARNING [inlinePacked] unbound variable: "++v)$
                             VarE (var v)
                  Just (_,Nothing)  -> VarE (var v) -- Bound, but non-inlinable binding.
                  -- Here we either inline just the copies, or we inline up to isConstructor
                  Just (ty,Just rhs)
                   | VarE v2 <- rhs   -> keepGoing
                     -- We allow a ProjE-of-AppE idiom:
                   | ProjE i e <- rhs -> keepGoing
                    -- IF we're in the RHS of an end-witness, don't duplicate code:
                   | not strong -> VarE v
                    -- Finally, we don't fully inline, but rather leave names visible:
                   | isConstructor rhs -> NamedVal v ty keepGoing
                   | CaseE{} <- rhs    -> NamedVal v ty keepGoing
                   | IfE{}   <- rhs    -> NamedVal v ty keepGoing
                   | otherwise -> error $ "inlinePacked: internal error, should not have "
                                        ++ "this is the inlining environment:\n "++sdoc rhs
                   where
                    keepGoing = exp strong env rhs
                                           
    (LetE (v,t,rhs) bod)
       | VarE v2    <- rhs  -> addAndGo  -- ^ We always do copy-prop.
       | not (L2.hasRealPacked t) -> LetE (var v,t, rhs') 
                                     (exp strong ((v,(t,Nothing)):env) bod)
       | TimeIt{}   <- rhs  -> LetE (var v,t, rhs') (exp strong ((v,(t,Nothing)):env) bod)
       | ProjE _ _  <- rhs  -> addAndGo

--      | L1.hasTimeIt rhs  -> __ -- AUDITME: is this still needed?
       | isConstructor rhs -> addAndGo

       -- Otherwise we have a case or an If.  We still inline those.
       | CaseE{} <- rhs  -> addAndGo
       | IfE{}  <- rhs   -> addAndGo
       | otherwise -> error $ " [inlinePacked] unexpected Let RHS:\n  "++sdoc e0
      where
        -- Don't reduce anything on the RHS yet, just add it:
        addAndGo = exp strong ((v,(t,Just rhs)):env) bod
        -- Subtlety: a binding for an end-witness should not have
        -- computational content.  We will not inline constructors into its RHS.
        rhs' | isEndVar v = exp False env rhs
             | otherwise  = go rhs

    ------ boilerplate -------
    (LitE i)    -> LitE i
    (AppE f e)  -> AppE f $ go e
    (PrimAppE p es) -> PrimAppE p $ map (go) es
    (IfE e1 e2 e3) ->
         IfE (go e1) (go e2) (go e3)
    (ProjE i e) -> ProjE i $ go e
    (MkProdE es) -> MkProdE $ map (go) es
    -- We don't rename field binders with to/from witness:
    (CaseE e mp) -> let mp' = map dorhs mp
                        dorhs (c,args,ae) =
                            let tys = lookupDataCon ddefs c
                                env' = [(v,(t,Nothing)) | (v,t) <- (zip args tys)] ++ env in
                            (c,args,exp strong env' ae)
                    in CaseE (go e) mp'
    (MkPackedE c es) -> MkPackedE c $ map (go) es
    (TimeIt e t b) -> TimeIt (go e) t b
    (MapE (v,t,e') e) -> let env' = (v,(t,Nothing)) : env in
                         MapE (var v,t,go e') (exp strong env' e)
    (FoldE (v1,t1,e1) (v2,t2,e2) e3) ->
         let env' = (v1,(t1,Nothing)) : (v2,(t2,Nothing)) : env in
         FoldE (var v1,t1,go e1) (var v2,t2,go e2)
               (exp strong env' e3)

pattern NamedVal vr ty e = LetE (vr,ty,e) (VarE "NAMED_VAL")
-- pattern NamedVal vr ty e <- LetE (vr,ty,e) (VarE "NAMED_VAL") where
--   NamedVal vr ty e = LetE (vr,ty,e) (VarE vr)
               
-- | Is it the call that actually allocates output data? 
isConstructor :: Exp -> Bool
isConstructor ex =
  case ex of
    AppE{} -> True
    MkPackedE{} -> True
    _ -> False
