{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- TEMP:
-- {-# OPTIONS_GHC -Wno-all #-}

-- | Convert from L1 to L2.

module Packed.FirstOrder.Passes.InferLocations
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

To avoid this copying, we will have to have existential types in the
future, and a stronger type-inference algorithm.

This type-inference algorithm should be able to apply unification much
as regular Hindley Milner inference does.  We always have a rigid
destination type with which to unify the body of a function.  We
likewise unify branches of a conditional to force them to use the same
destination (and copy otherwise).

Wherever unification fails, we pop up and report that, continuing type
checking only after repair.

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
   its the point where the location must be defined.


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
import qualified Control.Monad.Trans.Writer.Strict as W
import Control.Monad.Trans.Except
import Control.Monad.Trans (lift)
-- import qualified Control.Monad.Trans.Either
-- import qualified Control.Monad.Trans.Cont as CC


import Packed.FirstOrder.Common as C hiding (extendVEnv) -- (l, LRM(..)) 
import qualified Packed.FirstOrder.Common as C
import Packed.FirstOrder.Common (Var, Env2, DDefs, LocVar, runSyM, SyM, gensym, toVar)
import qualified Packed.FirstOrder.L1.Syntax as L1
import Packed.FirstOrder.L2.Syntax as L2
import Packed.FirstOrder.L2.Typecheck as T
--     (ConstraintSet, LocConstraint(..), RegionSet(..), LocationTypeState(..))


-- Dependencies
----------------------------------------------------------------------------------------------------

-- | Edges that create information-flow dependencies, representing both data dependencies
-- evident in the current program, and ones which will appear later, when locations are
-- given operational meaning.
--
-- These dependencies model the connections between named values, as introduced by LetE.
-- Unnamed values resulting from operand expressions cannot be shared.
data Dependence =
    -- * Value/value dependence.  Regular dataflow.
    VV { dstVar :: Var,    srcVar :: Var }

    -- * Location/value dependence: an end-location depends on the completion of a
    --   logical value before its address can be computed.
  | LV { dstLoc :: LocVar, srcVar :: Var }

    -- * Value/location dependence: a serialized (packed) value depends on its destination
    --   location in order to begin emiting its output.
  | VL { dstVar :: Var,    srcLoc :: LocVar }

    -- * Location/location or location/region dependence.
  | LL LocConstraint
  deriving (Show, Read, Ord, Eq)

data CommonVar = LVar !LocVar | DatVar !Var
  deriving (Show, Read, Ord, Eq)

-- | Map a dependence to a (from,to) edge.
depToEdge :: Dependence -> (CommonVar,CommonVar)
depToEdge = undefined

-- | Organize a graph into a map from each variable to the set of edges for which it
-- serves as the destination.
type Graph = M.Map CommonVar (S.Set Dependence)

-- | Map each variable to a set of other variables it depends on.
type DepGraph = M.Map CommonVar (S.Set CommonVar)

-- | A cycle is represented as an edgelist where the destination of the first edge matches
-- the source of the final edge.
type EdgeList = Seq.Seq Dependence


-- Environments
----------------------------------------------------------------------------------------------------

-- | Combine the different kinds of contextual information in-scope.
data FullEnv = FullEnv
    { dataDefs :: (DDefs Ty2)
    , valEnv :: M.Map Var Ty2
    , funEnv :: M.Map Var (ArrowTy Ty2)
    , dag    :: DepGraph }
-- TODO: Lenses would probably help a bit here.

--- -> RegionSet -> LocationTypeState

extendVEnv :: Var -> Ty2 -> FullEnv -> FullEnv
extendVEnv = undefined


lookupVarLoc :: Var -> FullEnv -> LocVar
lookupVarLoc = undefined
             
lookupVEnv :: Var -> FullEnv -> Ty2
lookupVEnv = undefined

lookupFEnv :: Var -> FullEnv -> (Ty2,Ty2)
lookupFEnv = undefined

-- | Instantiate the type schema for a function.  Return the fresh
-- location variables that 
instantiateFun :: Var -> FullEnv -> TiM ([LocVar], Ty2,Ty2)
instantiateFun = undefined
                  
extendDepGraph :: Dependence -> FullEnv -> FullEnv
extendDepGraph = undefined

transitiveClosure :: DepGraph -> DepGraph
transitiveClosure = undefined

hasCycle :: DepGraph -> Bool
hasCycle = undefined

----------------------------------------------------------------------------------------------------

-- | As we typecheck ,we track a list of dependencies as well as the
-- freshly allocated locations, together with a snapshot of the
-- environment ...............
type TyCheckLog = (EdgeList, Seq.Seq (LocVar, Env2 Ty2))

-- | Type inference monad.
type TiM a = ExceptT Failure (W.WriterT TyCheckLog SyM) a

-- | The result type for this pass.  Return a new expression and its
-- type, which includes/implies its location.
type Result = (L Exp2, Ty2)

type Cont = Result -> TiM Result
-- ^ Instead of explicit CPS, we could use ContT..

-- | A hole that represents a paused type-inference process.
newtype TiHole = TiHole Cont
-- ^ We could use an explicit datatype or a generic zipper library for
-- this, but instead we just use functions for holes.

-- | A selected subexpression that has focus.  A hole filled by a given subexpr.
data Selection = Selection L1.Exp1 TiHole
-- TODO: should we know the type of the expression selected at this point?

-- TODO: Really we should have two continuationsin a selection: one
-- for the context of the hole, in case the repair strategy wants to
-- examine the whole program, and a second one to continue
-- typechecking.

instance Show Selection where
  show (Selection e2 _hole) =
    "(Selection of subexpr: "++show e2++")"
--    "Within: "++show (hole )


-- | Type inference proceeds and fails sequentially.  It cannot
-- continue until the problem is resolved.
data Failure =
-- TODO: actually this needs something like TiHole to continue type checking after fixing
-- one or the other of the holes.
    FailedLocUnify { expected :: LocExp
                   , received :: LocExp
                   , context :: Selection }
    -- ^ Two locations we need to be equal, to unify, but don't.

  | CyclicDependence { chain :: EdgeList
                     , context :: Selection }
    -- ^ A cycle in the graph of value dependence between sizes, locations, and regular
    -- logical values.
 deriving Show


-- TODO: flip around the control flow and pass the repair agent into
-- the infer-locations pass:

-- | A program-repair agent that fixes failures as they arise.
type RepairTactic = Failure -> TiM Result


-- The compiler pass
----------------------------------------------------------------------------------------------------

-- | The compiler pass that converts from L1 to L2, inferring and
-- inserting location variables into the program.
--
-- This complete compiler pass has already been instantiated with a
-- failure-recover strategy.  Use rawInferProg if you want to apply a
-- different strategy.
inferLocs :: L1.Prog -> SyM L2.Prog
inferLocs (L1.Prog defs funs main) =
    error "FINISHME"


withRepairTactic :: RepairTactic -> L1.Prog -> SyM L2.Prog
withRepairTactic = undefined

inferProg :: L1.Prog -> TiM L2.Prog
inferProg = undefined
             
-- | Trivial expressions never cause failures.
doTriv :: FullEnv -> L1.Exp1 -> Exp2
doTriv = _finishme

inferExps :: FullEnv -> [L L1.Exp1] -> Cont -> TiM Result
inferExps = _fin


type LsCont = [L Exp2] -> TiM Result

sizeOrLoc :: Ty2 -> Either Int LocVar
sizeOrLoc = undefined

addOffset f off = undefined
    
-- | Takes the location of the tag and processes all the operands.
doDataConFields :: FullEnv -> LocVar -> [L L1.Exp1] -> LsCont -> TiM Result
doDataConFields env lprv0 ls0 k0 =
    go (T.AfterConstantC 1 lprv0) ls0 []
  where
    go :: (LocVar -> LocConstraint) -> _ -> _ -> _
    go _ [] acc = -- All fields processed.
       k0 (reverse acc)
    go lprev (nxt:rst) acc =
       inferExp env nxt $ \(nxt',nxtTy) -> 
        case sizeOrLoc nxtTy of
          Left sz ->
            go (addOffset lprev sz) rst (nxt' : acc)

            -- do tellConstraint (LL (T.AfterConstantC sz lprev _))

          Right loc ->
            case nxt' of
              L _ (VarE vr) ->
                let lvar = lookupVarLoc vr env in
                do tellConstraint (LL (lprev lvar))                                  
                   go (T.AfterVariableC vr lvar) rst (nxt' : acc)


-- | inferExp, if it succeeds, discharges all fresh locations used with 'LetLoc' forms.
-- If it fails, it returns a failure object containing a continuation.
inferExp :: FullEnv -> (L L1.Exp1) -> Cont -> TiM Result
inferExp env (L sl1 ex0) k =
  let l = L sl1 in
  case ex0 of
    L1.VarE v -> k (l$ VarE v, lookupVEnv v env)
    L1.LitE n -> k (l$ LitE n, IntTy)
    L1.IfE a b c@(L _ ce) ->
        -- Here we blithely assume success because L1 typechecking has already passed:
        inferExp env a $ \ (a',BoolTy) ->
        inferExp env b $ \ (b',tyb) ->
        let k' (c',tyc) = k (l$ IfE a' b' c', tyc) 
            ctxt = Selection ce (TiHole k') in
        inferExp env c $ \  (c',tyc) ->
          unifyLocs tyb tyc ctxt $
            k' (c',tyc)

    DataConE () con ls -> do
      loc <- freshLocVar "lDC" -- Location of the tag itself.
      let tyc = getTyOfDataCon (dataDefs env) con 
             
      -- TODO, constrain each argument to have a location related to the previous:
      doDataConFields env loc ls $ \ ls' -> 
         k ( l$ L2.DataConE loc con ls'
           , PackedTy tyc loc )


    -- Lets are where we allocate fresh locations:
    L1.LetE (vr,locs,_rhsTy,L sl2 rhs) bod | [] <- locs ->
      case rhs of
        L1.AppE f [] arg -> L1.assertTriv arg $ do 
            (newLocs,formalTy,resTy) <- instantiateFun f env

            let k' (arg',argTy) =
                      let env' = extendVEnv vr resTy env in                                    
                      inferExp env' bod $ \ (bod',bodTy) ->
                        k ( L sl1 (LetE (vr,[],resTy, L sl2 (AppE f (newLocs) arg')) bod')
                          , bodTy)
            inferExp env arg $ \ (arg',argTy) ->
              unifyLocs formalTy argTy (Selection _ (TiHole k')) $
                k' (arg',argTy)

        L1.LetE{} -> _

        PrimAppE p ls -> _prim
        DataConE loc k ls  -> _datacon
        LitSymE x     -> _linsym
        ProjE i e     -> _proj
        CaseE e ls    -> _case
        MkProdE ls    -> _mkprod
        TimeIt e t b       -> err "finish TimeIt"
        MapE (v,t,rhs) bod -> err "finish MapE"
        FoldE (v1,t1,r1) (v2,t2,r2) bod -> err "finish FoldE"

        _oth -> 
         inferExp env (L sl2 rhs) $ \ (_,_) ->
          -- Construct a value-dependence to all the free vars in the RHS:
          do tellConstraint VV{}
             inferExp _env' bod $ \ (_,_) ->
              _

     | otherwise -> err "Invariant violated.  LetE had nonempty bound locations."

    L1.AppE{} -> err $ "Expected flatten to name all function application results:\n "++show ex0
    PrimAppE p ls -> err $ "Expected flatten to name all primapp results:\n "++show ex0
    LitSymE x     -> _linsym
    ProjE i e     -> _proj
    CaseE e ls    -> _case
    MkProdE ls    -> _mkprod
    TimeIt e t b       -> err "finish TimeIt"
    MapE (v,t,rhs) bod -> err "finish MapE"
    FoldE (v1,t1,r1) (v2,t2,r2) bod -> err "finish FoldE"


err :: String -> a
err m = error $ "InferLocations: " ++ m

-- Helpers:
--------------------------------------------------------------------------------

-- | Record a dedence between a location/value and a location/value.
tellConstraint :: Dependence -> TiM ()
tellConstraint x = lift (W.tell (Seq.singleton x, Seq.empty))

-- | Snapshot the current environment at the point we allocate a new
-- location.
tellNewLocVar :: LocVar -> Env2 Ty2 -> TiM ()
tellNewLocVar v e = lift (W.tell (Seq.empty, Seq.singleton (v,e)))



-- | This helper exemplifies the simplicity of our current approach.
-- If we assume output regions are disjoint from input ones, then we
-- can instantiate an L1 function type into a polymorphic L2 one,
-- mechanically.
convertFunTy :: (L1.Ty1,L1.Ty1) -> SyM (ArrowTy Ty2)
convertFunTy (from,to) = do 
    -- let lvs = allLocVars inT ++ allLocVars outT
    -- lvs' <- mapM freshenVar lvs
    -- let subst = M.fromList (zip lvs lvs')
    -- return $ ArrowTy (substTy subst inT)
    --                  (substEffs subst effs)
    --                  (substTy subst outT)

    
    return $ ArrowTy { locVars = _
                     , arrIn   = _
                     , arrEffs = S.empty
                     , arrOut  = _
                     , locRets = [] }

-- TODO: Instantiate a polymorphic type schema.


-- | Fresh locVar
freshLocVar :: String -> TiM LocVar
freshLocVar m = lift$ lift$ gensym (toVar m)
                
-- | Unify two types that differ only in their locations.
-- This generates additional constraints.
unifyLocs :: Ty2 -> Ty2 -> Selection -> TiM Result -> TiM Result 
unifyLocs t1 t2 sel tail =
  if _
  then _
  else throwE (FailedLocUnify{ expected=_
                             , received=_
                             , context= sel })


-- Our unify function should produce [LocConstraint]
--
-- Here's the old location semi-lattice used for InferEffects:
{-
-- | Abstract locations:
data Loc = Fixed Var -- ^ A rigid location, such as for an input or output field.
         | Fresh Var -- ^ Fresh location-variables as created by
                     -- calling functions that are polymorphic in
                     -- their output location.
         | TupLoc [Loc] -- ^ The locations for each part of a tuple.
         | Top    -- ^ Contradiction.  Locations couldn't unify.
         | Bottom -- ^ "don't know" or "don't care".  This is the
                  -- location for non-packed data.
  deriving (Read,Show,Eq,Ord, Generic, NFData)
instance Out Loc
-}

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


test :: L2.Prog
test = fst $ runSyM 0 $ inferLocs L1.add1Prog

emptyEnv :: FullEnv
emptyEnv = FullEnv { dataDefs = C.emptyDD
                   , valEnv   = M.empty
                   , funEnv   = M.empty
                   , dag      = M.empty }

-- (Moved to Common)
-- l :: a -> L a
-- l x = L NoLoc x

idCont :: Cont
idCont (e,ty) = return (e,ty)

go :: TiM Result -> Either Failure Result
go =  fst . fst . runSyM 0 . W.runWriterT . runExceptT 

t1 :: Either Failure Result
t1 = fst$ fst$ runSyM 0 $ W.runWriterT $ runExceptT $
     inferExp emptyEnv (l$ LitE 3) idCont


-- t2_ :: TiM Result
-- t2_ = inferExp emptyEnv (l$ L1.IfE (l$ L1.PrimAppE L1.MkTrue [])
--                            (l$ L1.LitE 3)
--                            (l$ L1.LitE 4))
--                idCont

t2_ :: TiM Result
t2_ = inferExp emptyEnv (l$ L1.LetE ("v",[], IntTy, l$ LitE 33)
                           (l$ L1.VarE "v"))
               idCont
               
t2 :: Either Failure Result
t2 = go t2_

t3_ :: TiM Result
t3_ = inferExp emptyEnv (l$ L1.LetE ("v",[], BoolTy, l$ L1.PrimAppE L1.MkTrue [])
                           (l$ L1.VarE "v"))
               idCont


-- | Here is a challenge case where we have a scalar field between two
-- packed-data fields.
dd1 :: DDefs (UrTy ())
dd1 = (fromListDD [DDef "Tree"
                   [ ("Leaf",[])
                   , ("Node",[ (False, PackedTy "Tree" ())
                             , (False, IntTy)
                             , (False, PackedTy "Tree" ())])]])

-- | Simply construct a small tree value.
t4_p :: L1.Prog
t4_p = L1.Prog dd1 M.empty $ Just $
       l$ LetE ("x",[],PackedTy "Tree" (), l$ DataConE () "Leaf" []) $
       l$ LetE ("y",[],PackedTy "Tree" (), l$ DataConE () "Leaf" []) $
       l$ L1.DataConE () "Node"
          [ l$ VarE "x" , l$ LitE 99, l$ VarE "y"]


t4 :: Prog
t4 = fst $ runSyM 100 (inferLocs t4_p)
