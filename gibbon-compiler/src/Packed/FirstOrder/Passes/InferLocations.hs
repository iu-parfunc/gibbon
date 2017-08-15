-- | Convert from L1 to L2.

module Packed.FirstOrder.Passes.InferLocations where

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

-}

import qualified Data.Map as M

import qualified Packed.FirstOrder.Common as C
import Packed.FirstOrder.Common (Var, Env2, DDefs, LocVar, runSyM, SyM, gensym, toVar)
import qualified Packed.FirstOrder.L1.Syntax as L1
import Packed.FirstOrder.L2.Syntax as L2
import Packed.FirstOrder.L2.Typecheck
    (ConstraintSet, LocConstraint(..), RegionSet(..), LocationTypeState(..))
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

data CommonVar = LVar !LocVar | DatVar !Var
  deriving (Show, Read, Ord, Eq)

-- | Map a dependence to a (from,to) edge.
depToEdge :: Dependence -> (CommonVar,CommonVar)
depToEdge = undefined
            
type DepGraph = M.Map CommonVar Dependence

-- | A cycle is represented as an edgelist where the destination of the first edge matches
-- the source of the final edge.
type EdgeList = [Dependence]

----------------------------------------------------------------------------------------------------

-- | Combine the different kinds of contextual information in-scope.
data FullEnv = FullEnv (DDefs Ty2) (Env2 Ty2) DepGraph
-- TODO: Lenses would probably help a bit here.
             
--- -> RegionSet -> LocationTypeState

extendVEnv :: Var -> Ty2 -> FullEnv -> FullEnv
extendVEnv = undefined

extendDepGraph :: Dependence -> FullEnv -> FullEnv
extendDepGraph = undefined
             


----------------------------------------------------------------------------------------------------

-- | The compiler pass that converts from L1 to L2, inferring and
-- inserting location variables into the program.
inferLocs :: L1.Prog -> SyM L2.Prog
inferLocs = error "FINISHME"


-- | Type ineference monad.
type TiM a = Either Failure a
-- TODO: move all the junk we keep with us into a reader monad.
-- data Context = Context (DDefs Ty2) (Env2 Ty2) NewFuns ConstraintSet RegionSet LocationTypeState

-- TODO: switch to CPS so that we can generate Holes:
type Cont = Exp2 -> TiM (Exp2,Ty2)
-- ^ Instead of explicit continuations with this type, we could use ContT..


-- | inferExp, if it succeeds, discharges all fresh locations used with 'LetLoc' forms.
-- If it fails, it returns a failure object containing one or more continuations.
inferExp :: FullEnv -> Exp2 -> TiM (Ty2, LocationTypeState)
inferExp env ex0 =
  case ex0 of
    VarE v -> undefined
    LitE n -> return (IntTy, _)

              
-- | Type inference proceeds and fails sequentially.  It cannot
-- continue until the problem is resolved.
data Failure =
-- TODO: actually this needs something like TiHole to continue type checking after fixing
-- one or the other of the holes.
    FailedLocUnify { expected :: LocExp
                   , received :: LocExp
                   , context :: Selection
                   }
    -- ^ Two locations we need to be equal, to unify, but don't.
                        
  | CyclicDependence -- ??
    -- ^ A cycle in the graph of value dependence between sizes, locations, and regular
    -- logical values.

    
-- | A hole that represents a paused type-inference process.
newtype TiHole = TiHole Cont

    
-- | A context within a surrounding expression.  We could use an explicit datatype or a
-- generic zipper library for this, but instead we just use functions for holes.
-- newtype Hole = Hole (Exp2 -> Exp2)


-- | A selected subexpression that has focus.  A hole filled by a given subexpr.
data Selection = Selection Exp2 TiHole

               
-- Helpers:               
--------------------------------------------------------------------------------


-- TODO: Compute an L2 type schema from an L1 type.


-- TODO: Instantiate a polymorphic type schema.


-- | Fresh locVar
freshLocVar :: String -> SyM LocVar
freshLocVar m = gensym (toVar m)



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
