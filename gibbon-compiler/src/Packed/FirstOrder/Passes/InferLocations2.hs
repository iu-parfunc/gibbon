
{-# LANGUAGE OverloadedStrings #-}

-- TEMP:
-- {-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
-- {-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- TEMP:
-- {-# OPTIONS_GHC -Wno-all #-}

-- | Convert from L1 to L2, adding region constructs.

module Packed.FirstOrder.Passes.InferLocations2
    (-- data types
     FullEnv, TiM, InferState, Result, UnifyLoc, Failure, Dest(..),
     -- functions for manipulating locations
     fresh, freshUnifyLoc, finalUnifyLoc, fixLoc, freshLocVar, finalLocVar, assocLoc, finishExp,
          prim, emptyEnv,
     -- main functions
     unify, inferLocs, inferExp, inferExp', convertFunTy)
    where
      
{-
  Basic Strategy
  --------------

The idea here is to compute a location-annotated type signature
directly from the L1 type signature.  This implies extra copying to
support a destination-passing style.  For exmaple, the identity
function's type transforms as follows:

  id  :: Tree -> Tree
  id' :: forall l1 in r1, l2 in r2 . Tree l1 -> Tree l2

With this type, inferExp will immediately fail on the body of 'id x = x', requiring
a copy-insertion tactic to repair the failure and proceed.

To avoid this copying, we will require existential types in the
future, and a stronger type-inference algorithm.

The initial type-inference prototype should be able to apply
unification much as regular Hindley Milner inference does.  We always
have a rigid destination type with which to unify the body of a
function.  We likewise unify branches of a conditional to force them
to use the same destination (and copy otherwise).

Wherever unification fails, we pop up and report that failure,
continuing type checking only after repair.

We still need a strategy for discharging LetLoc bindings (when &
where), and for wrapping LetRegion forms whenever we would otherwise
have an unconstrained, ambiguous fresh region variable -- i.e. induced
by copy insertion.


 The three failures
 ------------------

As type checking proceeds, there are three ways things get stuck:

 * impossible constraint: inequality + equality
 * cyclic constraint: locations and values form a cycle
 * scope violation: location depends on something not in scope at 
   the point where the location must be defined.


 Location variable generation
 ----------------------------

With type-decorations and implicit location arguments and returns, our
basic let bindings of interest look like:

  let y [l2*] : ty = f [l*] x in bod

where the RHS (post-flattening) is a single application, or a
primitive application.

These function applications will be the first point of introduction
for fresh location variables.  This is the point at which we record
the environment in scope at the generated location variable.  This
will ultimately become the scope at which we allocate the location
variable, which will follow 'y' and be constrained only via 'y's
variable references.

The references to the generated location will consist of (1) LocExp's
relating it to other locations falling "after" it, and (2) dependence
constraints created via 'y's variable references.

We COULD examine every use-point of a location variable to check that
it doesn't involve dependences on out-of-scope values, but it is
sufficient to check only against the lexical environment at the
introduction point of the fresh location variable.

-}

import Data.Loc
import qualified Data.Sequence as Seq
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Foldable as F
import Data.Maybe
-- import qualified Control.Monad.Trans.Writer.Strict as W
import qualified Control.Monad.Trans.State.Strict as St
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans (lift)
-- import qualified Control.Monad.Trans.Either
-- import qualified Control.Monad.Trans.Cont as CC
import Debug.Trace


import Packed.FirstOrder.GenericOps (gFreeVars)
import Packed.FirstOrder.Common as C hiding (extendVEnv) -- (l, LRM(..)) 
-- import qualified Packed.FirstOrder.Common as C
import Packed.FirstOrder.Common (Var, Env2, DDefs, LocVar, runSyM, SyM, gensym, toVar)
import qualified Packed.FirstOrder.L1.Syntax as L1
import Packed.FirstOrder.L2.Syntax as L2
import Packed.FirstOrder.L2.Typecheck as T
--     (ConstraintSet, LocConstraint(..), RegionSet(..), LocationTypeState(..))


-- Environments
----------------------------------------------------------------------------------------------------

-- | Combine the different kinds of contextual information in-scope.
data FullEnv = FullEnv
    { dataDefs :: (DDefs Ty2)           -- ^ Data type definitions
    , valEnv :: M.Map Var Ty2           -- ^ Type env for local bindings
    , funEnv :: M.Map Var (ArrowTy Ty2) -- ^ Top level fundef types
    }
-- TODO: Lenses would probably help a bit here.

extendVEnv :: Var -> Ty2 -> FullEnv -> FullEnv
extendVEnv v ty fe@FullEnv{valEnv} = fe { valEnv = M.insert v ty valEnv }

lookupVarLoc :: Var -> FullEnv -> LocVar
lookupVarLoc v env = case lookupVEnv v env of
                       PackedTy _ lv -> lv
                       _ -> err $ "Variable does not have location: " ++ (show v)
             
lookupVEnv :: Var -> FullEnv -> Ty2
lookupVEnv v FullEnv{valEnv} = valEnv # v

lookupFEnv :: Var -> FullEnv -> ArrowTy Ty2
lookupFEnv v FullEnv{funEnv} = funEnv # v

-- Types
--------------------------------------------------------------------------------

-- | This helper exemplifies the simplicity of our current approach.
-- If we assume output regions are disjoint from input ones, then we
-- can instantiate an L1 function type into a polymorphic L2 one,
-- mechanically.
convertFunTy :: (L1.Ty1,L1.Ty1) -> SyM (ArrowTy Ty2)
convertFunTy (from,to) = do
    from' <- convertTy from
    to'   <- convertTy to
    -- For this simple version, we assume every location is in a separate region:
    lrm1 <- toLRM from' Input
    lrm2 <- toLRM to'   Output
    return $ ArrowTy { locVars = lrm1 ++ lrm2
                     , arrIn   = from'
                     , arrEffs = S.empty
                     , arrOut  = to'
                     , locRets = [] }
 where
   toLRM ls md =
       mapM (\v -> do r <- freshLocVar "r"
                      return $ LRM v (VarR r) md)
            (F.toList ls)

convertTy :: L1.Ty1 -> SyM Ty2
convertTy ty = traverse (const (freshLocVar "loc")) ty

convertDDefs :: DDefs L1.Ty1 -> SyM (DDefs Ty2)
convertDDefs ddefs = traverse f ddefs
    where f (DDef n dcs) = do
            dcs' <- forM dcs $ \(dc,bnds) -> do
                             bnds' <- forM bnds $ \(isb,ty) -> do
                                               ty' <- convertTy ty
                                               return (isb, ty')
                             return (dc,bnds')
            return $ DDef n dcs'

-- Inference algorithm
--------------------------------------------------------------------------------

type TiM a = ExceptT Failure (St.StateT InferState SyM) a

type InferState = M.Map LocVar UnifyLoc

data UnifyLoc = FixedLoc Var
              | FreshLoc Var
                deriving (Show, Eq)

data Failure = FailUnify Ty2 Ty2
             | FailInfer L1.Exp1
               deriving (Show, Eq)

---------------------------------------

data Constraint = AfterConstantL LocVar Int LocVar
                | AfterVariableL LocVar Var LocVar
                | AfterTagL LocVar LocVar
                | StartRegionL LocVar Region
                  deriving (Show, Eq)

-- | The result type for this pass.  Return a new expression and its
-- type, which includes/implies its location.
type Result = (L Exp2, Ty2, [Constraint])

inferLocs :: L1.Prog -> SyM L2.Prog
inferLocs (L1.Prog dfs fds me) = do
  prg <- St.runStateT (runExceptT m) M.empty
  case fst prg of
    Right a -> return a
    Left a -> err $ show a
  where m = do
          dfs' <- lift $ lift $ convertDDefs dfs
          fenv <- forM fds $ \(L1.FunDef _ (_,inty) outty _) ->
                  lift $ lift $ convertFunTy (inty,outty)
          let fe = FullEnv dfs' M.empty fenv
          me' <- case me of
            Just me -> do
              l1 <- fresh
              u1 <- fixLoc l1
              (me',ty') <- inferExp' fe me $ SingleDest l1
              me' <- finishExp me'
              return $ Just (me',ty')
            Nothing -> return Nothing
          fds' <- forM fds $ \(L1.FunDef fn fa frt fbod) -> do
                                   frt' <- lift $ lift $ convertTy frt
                                   case frt' of
                                     PackedTy tc lv -> do
                                               -- TODO: finish this
                                               return undefined
                                     _ -> do let arrty = lookupFEnv fn fe
                                                 fe' = extendVEnv (fst fa) (arrIn arrty) fe
                                             (fbod',_) <- inferExp' fe' fbod NoDest
                                             fbod' <- finishExp fbod'
                                             return $ L2.FunDef fn arrty (fst fa) fbod'
          return $ L2.Prog dfs' fds' me'

    
-- | Destination can be a single location var, a tuple of destinations,
-- or nothing (for scalar values)
data Dest = SingleDest LocVar -- TODO: refactor to just be list of locations, or actually enforce invariants of non-empty list, etc
          | TupleDest [Dest]
          | NoDest

mkDest :: [LocVar] -> Dest
mkDest [lv] = SingleDest lv
mkDest [] = NoDest
mkDest lvs = TupleDest $ L.map (mkDest . (\lv -> [lv])) lvs

-- | Wrap the inferExp procedure, and consume all remaining constraints
inferExp' :: FullEnv -> (L L1.Exp1) -> Dest -> TiM (L L2.Exp2, L2.Ty2)
inferExp' env lex0@(L sl1 exp) dest =
  let lc = L sl1

      bindAllLocations :: Result -> TiM Result
      bindAllLocations (expr,ty,constrs) = return $ (expr',ty,[])
          where constrs' = L.nub constrs
                expr' = foldr addLetLoc expr constrs'
                addLetLoc i a =
                    case i of
                      AfterConstantL lv1 v lv2 -> lc$ Ext (LetLocE lv1 (AfterConstantLE v lv2) a)
                      AfterVariableL lv1 v lv2 -> lc$ Ext (LetLocE lv1 (AfterVariableLE v lv2) a)
                      StartRegionL lv r -> lc$ Ext (LetLocE lv (InRegionLE r) a)
                      AfterTagL lv1 lv2 -> lc$ Ext (LetLocE lv1 (AfterConstantLE 1 lv2) a)

  in inferExp env lex0 dest >>= \(a,b,_) -> return (a,b)
     -- inferExp env lex0 dest >>= bindAllLocations >>= \(a,b,_) -> return (a,b)
                    
-- | We proceed in a destination-passing style given the target region
-- into which we must produce the resulting value.
inferExp :: FullEnv -> (L L1.Exp1) -> Dest -> TiM Result
inferExp env@FullEnv{dataDefs}
         lex0@(L sl1 ex0) dest =
  let lc = L sl1

      tryBindReg :: Result -> TiM Result
      tryBindReg (e,ty,((StartRegionL lv r) : cs)) =
          do lv' <- finalLocVar lv
             (e',ty',cs') <- tryBindReg (e,ty,cs)
             b <- noAfterLoc lv' cs' cs' -- (traceShowId lv) (traceShowId cs')
             if b
             then do (e'',ty'',cs'') <- bindTrivialAfterLoc lv' (e',ty',cs')
                     return (l$ Ext (LetRegionE r (l$ Ext (LetLocE lv' (InRegionLE r) e''))), ty'', cs'')
             else return (e',ty',(StartRegionL lv r):cs')
      tryBindReg (e,ty,c:cs) =
          do (e',ty',cs') <- tryBindReg (e,ty,cs)
             return (e',ty',c:cs')
      tryBindReg (e,ty,[]) = return (e,ty,[])

      handleTrailingBindLoc :: Var -> Result -> TiM Result
      handleTrailingBindLoc v res =
          do (e,ty,cs) <- bindAfterLoc v res
             case e of
               (L _ (Ext (LetLocE lv1 (AfterVariableLE v lv2) e))) ->
                   do (e',ty',cs') <- bindTrivialAfterLoc lv1 (e,ty,cs)
                      return (l$ Ext (LetLocE lv1 (AfterVariableLE v lv2) e'), ty', cs')
               _ -> return (e,ty,cs)

      bindAfterLoc :: Var -> Result -> TiM Result
      bindAfterLoc v (e,ty,c:cs) =
          case c of
            AfterVariableL lv1 v' lv2 ->
                if v == v'
                then do lv1' <- finalLocVar lv1
                        lv2' <- finalLocVar lv2
                        return (lc$ Ext (LetLocE lv1' (AfterVariableLE v lv2) e), ty, cs)
                else do (e',ty',cs') <- bindAfterLoc v (e,ty,cs)
                        return (e',ty',c:cs')
            _ -> do (e',ty',cs') <- bindAfterLoc v (e,ty,cs)
                    return (e',ty',c:cs')
      bindAfterLoc _ (e,ty,[]) = return (e,ty,[])

      bindTrivialAfterLoc :: LocVar -> Result -> TiM Result
      bindTrivialAfterLoc lv (e,ty,c:cs) =
          case c of
            AfterTagL lv1 lv2 ->
                do lv1' <- finalLocVar lv1
                   lv2' <- finalLocVar lv2
                   lv' <- finalLocVar lv
                   if lv2' == lv'
                   then do (e',ty',cs') <- bindTrivialAfterLoc lv1 (e,ty,cs)
                           return (lc$ Ext (LetLocE lv1' (AfterConstantLE 1 lv2') e'), ty', cs')
                   else do (e',ty',cs') <- bindTrivialAfterLoc lv (e,ty,cs)
                           return (e,ty,c:cs')
            AfterConstantL lv1 v lv2 ->
                do lv1' <- finalLocVar lv1
                   lv2' <- finalLocVar lv2
                   lv' <- finalLocVar lv
                   if lv2' == lv'
                   then do (e',ty',cs') <- bindTrivialAfterLoc lv1 (e,ty,cs)
                           return (lc$ Ext (LetLocE lv1' (AfterConstantLE v lv2') e'), ty', cs')
                   else do (e',ty',cs') <- bindTrivialAfterLoc lv (e,ty,cs)
                           return (e,ty,c:cs')
            _ -> do (e',ty',cs') <- bindTrivialAfterLoc lv (e,ty,cs)
                    return (e,ty,c:cs')
      bindTrivialAfterLoc _ (e,ty,[]) = return (e,ty,[])

      -- | To handle a case expression, we need to bind locations
      -- appropriately for all the fields.
      doCase :: DDefs Ty2 -> FullEnv -> LocVar -> Dest
             -> (DataCon, [(Var,())],     L L1.Exp1) ->
             TiM ((DataCon, [(Var,LocVar)], L L2.Exp2), Ty2, [Constraint])
      doCase ddfs env src dst (con,vars,rhs) = do
        vars' <- forM vars $ \(v,_) -> do lv <- lift $ lift $ freshLocVar "case"
                                          _ <- fixLoc lv
                                          return (v,lv)
        let contys = lookupDataCon ddfs con
            newtys = L.map (\(ty,(_,lv)) -> fmap (const lv) ty) $ zip contys vars'
            env' = L.foldr (\(v,ty) a -> extendVEnv v ty a) env $ zip (L.map fst vars') newtys
        (rhs',ty',cs') <- inferExp env' rhs dst
        -- TODO: insert bindings from variables bound in case
        return ((con,vars',rhs'),ty',cs')


  in
  case ex0 of
    L1.VarE v ->
      let e' = lc$ VarE v in
      case dest of
        NoDest -> return (e', lookupVEnv v env, [])
        TupleDest ds -> err $ "TODO: handle tuple of destinations for VarE"
        SingleDest d  -> do
                  let ty  = lookupVEnv v env
                  loc <- case ty of
                           PackedTy _ lv -> return lv
                           -- TODO: refactor this so we never try to put a non-packed type
                           -- in a location
                           _ -> lift $ lift $ freshLocVar "imm"
                  let ty' = case ty of
                              PackedTy k lv -> PackedTy k loc
                              t -> t
                  unify d loc
                            (return (e',ty',[]))
                            (copy (e',ty,[]) d)

    L1.MkProdE ls ->
      case dest of
        NoDest -> err $ "Expected destination(s) for expression"
        SingleDest d -> case ls of
                          [e] -> do (e',ty,les) <- inferExp env e dest
                                    return (lc$ MkProdE [e'], ty, les)
                          _ -> err $ "Cannot match single destination to tuple"
        TupleDest ds -> do results <- mapM (\(e,d) -> inferExp env e d) $ zip ls ds
                           return (lc$ MkProdE ([a | (a,_,_) <- results]),
                                     ProdTy ([b | (_,b,_) <- results]),
                                     concat $ [c | (_,_,c) <- results])
          
    L1.LitE n -> return (lc$ LitE n, IntTy, [])

    L1.DataConE () k ls ->
      case dest of
        NoDest -> err $ "Expected single location destination for DataConE"
        TupleDest _ds -> err $ "Expected single location destination for DataConE"
        SingleDest d -> do
                  locs <- sequence $ replicate (length ls) fresh
                  ls' <- mapM (\(e,lv) -> (inferExp env e $ SingleDest lv)) $ zip ls locs
                  let afterVar :: (Maybe (L Exp2), Maybe LocVar, Maybe LocVar) -> Maybe Constraint
                      afterVar ((Just (L _ (VarE v))), (Just loc1), (Just loc2)) =
                          Just $ AfterVariableL loc1 v loc2
                      afterVar ((Just (L _ (LitE _))), (Just loc1), (Just loc2)) =
                          Just $ AfterConstantL loc1 8 loc2
                      afterVar _ = Nothing
                      constrs = concat $ [c | (_,_,c) <- ls']
                      constrs' = if null locs
                                 then constrs
                                 else let tmpconstrs = [AfterTagL (L.head locs) d] ++
                                                       (mapMaybe afterVar $ zip3
                                                         (map Just ([a | (a,_,_) <- ls' ]))
                                                         -- ((map Just $ L.tail ([a | (a,_,_) <- ls' ])) ++ [Nothing])
                                                         -- (map Just locs)
                                                         ((map Just $ L.tail locs) ++ [Nothing])
                                                         (map Just locs)) ++
                                                         -- ((map Just $ L.tail locs) ++ [Nothing])) ++
                                                       constrs
                                          -- firstty = (\(_,b,_) -> b) (head ls')
                                      in tmpconstrs -- if L2.isPackedTy' firstty then tmpconstrs else tail tmpconstrs
                  return (lc$ DataConE d k [ e' | (e',_,_)  <- ls'],
                            PackedTy (getTyOfDataCon dataDefs k) d,
                            constrs')
    
    L1.IfE a b c@(L _ ce) -> do
       -- Here we blithely assume BoolTy because L1 typechecking has already passed:
       (a',bty,acs) <- inferExp env a NoDest
       assumeEq bty BoolTy
       -- Here BOTH branches are unified into the destination, so
       -- there is no need to unify with eachother.
       (b',tyb,csb)    <- inferExp env b dest
       (c',tyc,csc)    <- inferExp env c dest
       return (lc$ IfE a' b' c', tyc, L.nub $ acs ++ csb ++ csc)

--   | CaseE EXP [(DataCon, [(Var,loc)], EXP)]

    L1.CaseE ex ls -> do 
      -- Case expressions introduce fresh destinations for the scrutinee:
      loc <- lift $ lift $ freshLocVar "scrut"
      (ex',ty2,cs) <- inferExp env ex (SingleDest loc)
      let src = locOfTy ty2
      pairs <- mapM (doCase dataDefs env src dest) ls
      return (lc$ CaseE ex' ([a | (a,_,_) <- pairs]),
              (\(_,b,_)->b) (L.head pairs),
              (concat $ [c | (_,_,c) <- pairs]))

    L1.LetE (vr,locs,bty,L sl2 rhs) bod | [] <- locs ->
      case rhs of
        L1.AppE f [] arg -> L1.assertTriv arg $ do
            err $ "TODO: Support recursive functions: "++show rhs
        L1.LetE{} -> err $ "Expected let spine, encountered nested lets: " ++ (show lex0)
        L1.LitE i -> do
          (bod',ty',cs') <- inferExp (extendVEnv vr IntTy env) bod dest
          (bod'',ty'',cs'') <- handleTrailingBindLoc vr (bod', ty', cs')
          tryBindReg (lc$ L2.LetE (vr,[],IntTy,L sl2 $ L2.LitE i) bod'', ty'', cs'')

        -- Literals have no location, as they are scalars/value types.
--        L1.LitE n -> _finLit
                     
        PrimAppE p ls -> do
          lsrec <- mapM (\e -> inferExp env e NoDest) ls
          ty <- lift $ lift $ convertTy bty
          (bod',ty',cs') <- inferExp (extendVEnv vr ty env) bod dest
          let ls' = L.map (\(a,_,_)->a) lsrec
              cs'' = concat $ [c | (_,_,c) <- lsrec]
          (bod'',ty'',cs''') <- handleTrailingBindLoc vr (bod', ty', L.nub $ cs' ++ cs'')
          tryBindReg (lc$ L2.LetE (vr,[],ty,L sl2 $ L2.PrimAppE (prim p) ls') bod'',
                    ty'', cs''')
        DataConE _loc k ls  -> do
          loc <- lift $ lift $ freshLocVar "datacon"
          (rhs',rty,rcs) <- inferExp env (L sl2 $ DataConE () k ls) $ SingleDest loc
          (bod',ty',cs') <- inferExp (extendVEnv vr (PackedTy (getTyOfDataCon dataDefs k) loc) env) bod dest
          b <- containsLoc loc ty' cs'
          cs'' <- if b
                  then return $ L.nub $ cs' ++ rcs
                  else do r <- lift $ lift $ freshRegVar
                          return $ (StartRegionL loc r) : (L.nub $ cs' ++ rcs)
          (bod'',ty'',cs''') <- handleTrailingBindLoc vr (bod', ty', cs'')
          tryBindReg (lc$ L2.LetE (vr,[loc],PackedTy k loc,rhs') bod'',
                    ty', cs'')
        LitSymE x     -> do
          (bod',ty',cs') <- inferExp (extendVEnv vr IntTy env) bod dest
          (bod'',ty'',cs'') <- handleTrailingBindLoc vr (bod', ty', cs')
          tryBindReg (lc$ L2.LetE (vr,[],IntTy,L sl2 $ L2.LitSymE x) bod'', ty'', cs'')
        ProjE i e     -> do
          (e,ProdTy tys,cs) <- inferExp env e NoDest
          (bod',ty',cs') <- inferExp (extendVEnv vr (tys !! i) env) bod dest
          (bod'',ty'',cs'') <- handleTrailingBindLoc vr (bod', ty', L.nub $ cs ++ cs')
          tryBindReg (lc$ L2.LetE (vr,[],ProdTy tys,L sl2 $ L2.ProjE i e) bod'',
                             ty'', cs'')
        CaseE e ls    -> _case
        MkProdE ls    -> _mkprod
        TimeIt e t b       -> do
          lv <- lift $ lift $ freshLocVar "timeit"
          let subdest = case bty of
                          PackedTy _ _ -> SingleDest lv
                          _ -> NoDest
          (e',ty,cs) <- inferExp env e subdest
          (bod',ty',cs') <- inferExp (extendVEnv vr ty env) bod dest
          (bod'',ty'',cs'') <- handleTrailingBindLoc vr (bod', ty', L.nub $ cs ++ cs')
          tryBindReg (lc$ L2.LetE (vr,[],ty,L sl2 $ TimeIt e' ty b) bod'',
                    ty'', cs'')
          

        _oth -> err $ "Unhandled case in lhs of let: " ++ (show lex0)

    _oth -> err $ "Unhandled case: " ++ (show lex0)



-- TODO: Should eventually allow src and dest regions to be the same
-- for in-place updates packed data with linear types.
  

finishExp :: L Exp2 -> TiM (L Exp2)
finishExp (L i e) =
    let l e = L i e
    in
    case e of
      VarE v -> return $ l$ VarE v
      LitE i -> return $ l$ LitE i
      LitSymE v -> return $ l$ LitSymE v
      AppE v ls e1 -> do
             e1' <- finishExp e1
             ls' <- mapM finalLocVar ls
             return $ l$ AppE v ls e1'
      PrimAppE pr es -> do
             es' <- mapM finishExp es
             return $ l$ PrimAppE pr es'
      LetE (v,ls,t,e1) e2 -> do
             e1' <- finishExp e1
             e2' <- finishExp e2
             ls' <- mapM finalLocVar ls
             return $ l$ LetE (v,ls',t,e1') e2'
      IfE e1 e2 e3 -> do
             e1' <- finishExp e1
             e2' <- finishExp e2
             e3' <- finishExp e3
             return $ l$ IfE e1' e2' e3'
      MkProdE es -> do
             es' <- mapM finishExp es
             return $ l$ MkProdE es'
      ProjE i e1 -> do
             e1' <- finishExp e1
             return $ l$ ProjE i e1'
      CaseE e1 prs -> do
             e1' <- finishExp e1
             prs' <- forM prs $ \(dc, lvs, e2) -> do
                         e2' <- finishExp e2
                         lvs' <- forM lvs $ \(v,lv) -> do
                                                    lv' <- finalLocVar lv
                                                    return (v,lv')
                         return (dc,lvs',e2')
             return $ l$ CaseE e1' prs'
      DataConE lv dc es -> do
             es' <- mapM finishExp es
             lv' <- finalLocVar lv
             return $ l$ DataConE lv' dc es'
      TimeIt e1 d b -> do
             e1' <- finishExp e1
             return $ l$ TimeIt e1' d b
      _ -> err $ "Unhandled case: " ++ (show e)

containsLoc :: LocVar -> L2.Ty2 -> [Constraint] -> TiM Bool
containsLoc lv1 ty cs =
    case ty of
      PackedTy _ lv2 ->
          do lv1' <- finalLocVar lv1
             lv2' <- finalLocVar lv2
             if lv1' == lv2'
             then return True
             else do lvs <- associatedLocs lv2 cs
                     return $ elem lv1' lvs
      _ -> return False

associatedLocs :: LocVar -> [Constraint] -> TiM [LocVar]
associatedLocs lv (c:cs) =
    do lv' <- finalLocVar lv
       lvs <- associatedLocs lv cs
       case c of
         AfterConstantL lv1 _v lv2 ->
             do lv1' <- finalLocVar lv1
                lv2' <- finalLocVar lv2   
                if lv' == lv2'
                then do lvs' <- associatedLocs lv1' cs
                        return $ L.nub $ lv1' : (lvs ++ lvs')
                else return lvs
         AfterVariableL lv1 _v lv2 ->
             do lv1' <- finalLocVar lv1
                lv2' <- finalLocVar lv2   
                if lv' == lv2'
                then do lvs' <- associatedLocs lv1' cs
                        return $ L.nub $ lv1' : (lvs ++ lvs')
                else return lvs
         _ -> return lvs
associatedLocs lv [] = return []

noAfterLoc :: LocVar -> [Constraint] -> [Constraint] -> TiM Bool
noAfterLoc lv fcs (c:cs) =
    case c of
      AfterVariableL lv1 v lv2 ->
          do lv2' <- finalLocVar lv2
             lv' <- finalLocVar lv
             if lv' == lv2' then return False else noAfterLoc lv fcs cs
      AfterTagL lv1 lv2 ->
          do lv2' <- finalLocVar lv2
             lv' <- finalLocVar lv
             if lv' == lv2'
             then do b1 <- noAfterLoc lv fcs cs
                     b2 <- noAfterLoc lv1 fcs fcs
                     return (b1 && b2)
             else noAfterLoc lv fcs cs
      AfterConstantL lv1 v lv2 -> 
          do lv2' <- finalLocVar lv2
             lv' <- finalLocVar lv
             if lv' == lv2' then return False else noAfterLoc lv fcs cs
      _ -> noAfterLoc lv fcs cs
noAfterLoc _ _ [] = return True

-- | Unify is a conditional form that takes a "success branch" and
-- "failure branch".  In the case of failure, it makes no change to
-- the store.  In the case of success, the new equalities are placed
-- in the store /before/ executing the success branch.
unify :: LocVar -> LocVar -> TiM a -> TiM a -> TiM a
unify v1 v2 success fail = do
  ut1 <- lookupUnifyLoc v1
  ut2 <- lookupUnifyLoc v2
  case (ut1,ut2) of
    (FixedLoc l1, FixedLoc l2) ->
        if l1 == l2 then success else fail
    (FreshLoc l1, FixedLoc l2) ->
        do assocLoc l1 (FixedLoc l2)
           success
    (FixedLoc l2, FreshLoc l1) -> 
        do assocLoc l1 (FixedLoc l2)
           success
    (FreshLoc l1, FreshLoc l2) ->
        do assocLoc l1 (FreshLoc l2)
           success

freshLocVar :: String -> SyM LocVar
freshLocVar m = gensym (toVar m)

freshRegVar :: SyM Region
freshRegVar = do rv <- gensym (toVar "r")
                 return $ VarR rv

finalUnifyLoc :: LocVar -> TiM UnifyLoc
finalUnifyLoc v = do
  m <- lift $ St.get
  case M.lookup v m of
    Nothing -> return (FreshLoc v)
    Just (FixedLoc v') -> return (FixedLoc v')
    Just (FreshLoc v') -> finalUnifyLoc v'

finalLocVar :: LocVar -> TiM LocVar
finalLocVar v = do
  u <- finalUnifyLoc v
  case u of
    FixedLoc v' -> return v'
    FreshLoc v' -> return v'

fresh :: TiM LocVar
fresh = do
  lift $ lift $ freshLocVar "loc"

freshUnifyLoc :: TiM UnifyLoc
freshUnifyLoc = do
  lv <- fresh
  return $ FreshLoc lv

lookupUnifyLoc :: LocVar -> TiM UnifyLoc
lookupUnifyLoc lv = do
  m <- lift $ St.get
  case M.lookup lv m of
    Nothing -> do
      l' <- fresh
      lift $ St.put $ M.insert lv (FreshLoc l') m
      return $ FreshLoc l'
    Just (FreshLoc l') -> finalUnifyLoc l'
    Just (FixedLoc l') -> return $ FixedLoc l'

fixLoc :: LocVar -> TiM UnifyLoc
fixLoc lv = do 
  -- l' <- fresh
  m <- lift $ St.get
  lift $ St.put $ M.insert lv (FixedLoc lv) m
  return $ FixedLoc lv

assocLoc :: LocVar -> UnifyLoc -> TiM ()
assocLoc lv ul = do
  m <- lift $ St.get
  lift $ St.put $ M.insert lv ul m

-- | The copy repair tactic:
copy :: Result -> LocVar -> TiM Result
copy = _
  -- TODO

-- | For a packed type, get its location.
locOfTy :: Ty2 -> LocVar
locOfTy (PackedTy _ lv) = lv
locOfTy ty2 = err $ "Expected packed type, got "++show ty2 
       
err :: String -> a
err m = error $ "InferLocations: " ++ m

assumeEq :: (Eq a, Show a) => a -> a -> TiM ()
assumeEq a1 a2 =
    if a1 == a2
    then return ()
    else err $ "Expected these to be equal: " ++ (show a1) ++ ", " ++ (show a2)

-- | Convert a prim from L1 to L2
prim :: L1.Prim L1.Ty1 -> L1.Prim Ty2
prim p = case p of
           L1.AddP -> L1.AddP
           L1.SubP -> L1.SubP
           L1.MulP -> L1.MulP
           L1.DivP -> L1.DivP
           L1.ModP -> L1.ModP
           L1.EqSymP -> L1.EqSymP
           L1.EqIntP -> L1.EqIntP
           L1.MkTrue -> L1.MkTrue
           L1.MkFalse -> L1.MkFalse
           _ -> err $ "Can't handle this primop yet in InferLocations:\n"++show p

       
-- Notes on program repair:
--------------------------------------------------------------------------------

-- Tactic 1: reorder definitions and retry
-- Tactic 2: replace a hole containing 'e' with '(copy e)'
-- Tactic 3: change field x to be a pointer, making size(x) constant
-- Tactic 4: inline a binding duplicating work (usually bad)

-- Notes on testing:
--------------------------------------------------------------------------------
-- Test on:
-- add1ProgLetLeft  : succeeds in one try
-- add1ProgLetRight : fails with CyclicDependence and needs reorder tactic
-- add1ProgChallenge : fails with CyclicDependence and needs copy tactic
-- add1ProgSharing : fails with FailedLocUnify: l_x2 cannot equal 'l_x2 + size(x2)'

               
emptyEnv :: FullEnv
emptyEnv = FullEnv { dataDefs = C.emptyDD
                   , valEnv   = M.empty
                   , funEnv   = M.empty }


t0 :: ArrowTy Ty2
t0 = fst$ runSyM 0 $
     convertFunTy (snd (L1.funArg fd), L1.funRetTy fd)
   where fd = L1.fundefs L1.add1Prog M.! "add1"
           
tester1 :: L L1.Exp1 -> L Exp2
tester1 e = case fst $ fst $ runSyM 0 $ St.runStateT (runExceptT (inferExp emptyEnv e NoDest)) M.empty of
              Right a -> (\(a,_,_)->a) a
              Left a -> err $ show a

t1 :: L Exp2
t1 = tester1 (l$ LitE 3)

--  id  :: Tree -> Tree
--  id' :: forall l1 in r1, l2 in r2 . Tree l1 -> Tree l2

t2 :: L Exp2
t2 = tester1 $
     l$ LetE ("x",[],IntTy,l$ LitE 1) $
     l$ LetE ("y",[],IntTy,l$ LitE 2) $
     l$ LetE ("z",[],IntTy,l$ PrimAppE L1.AddP [l$ VarE "x", l$ VarE "y"]) $
     l$ VarE "z"

ddtree :: DDefs Ty2
ddtree = fromListDD [DDef (toVar "Tree")
                      [ ("Leaf",[(False,IntTy)])
                      , ("Node",[ (False,PackedTy "Tree" "l")
                                , (False,PackedTy "Tree" "l")])
                      ]]

treeEnv :: FullEnv
treeEnv = FullEnv { dataDefs = ddtree
                  , valEnv   = M.empty
                  , funEnv   = M.empty }


tester2 :: L L1.Exp1 -> L Exp2
tester2 e = case fst $ fst $ runSyM 0 $ St.runStateT (runExceptT (inferExp' treeEnv e NoDest)) M.empty of
              Right a -> fst a
              Left a -> err $ show a

t3 :: L Exp2
t3 = tester2 $
     l$ LetE ("x",[],IntTy,l$ LitE 1) $
     l$ LetE ("y",[],IntTy,l$ LitE 2) $
     l$ LetE ("z",[],PackedTy "Tree" (), l$ DataConE () "Leaf" [l$ VarE "x", l$ VarE "y"]) $
     l$ LitE 0
