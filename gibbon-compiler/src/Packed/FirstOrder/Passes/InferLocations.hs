-- | Convert from L1 to L2.

module Packed.FirstOrder.Passes.InferLocations where

import Packed.FirstOrder.Common
import qualified Packed.FirstOrder.L1.Syntax as L1
import Packed.FirstOrder.L2.Syntax as L2
import Packed.FirstOrder.L2.Typecheck
    (ConstraintSet, LocConstraint(..), RegionSet(..), LocationTypeState(..))

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

-- | inferExp, if it succeeds, discharges all fresh locations used with 'LetLoc' forms.
-- If it fails, it returns a failure object containing one or more continuations.
inferExp :: DDefs Ty2 -> Env2 Ty2 -> NewFuns
         -> ConstraintSet -> RegionSet -> LocationTypeState -> Exp2
         -> TiM (Ty2, LocationTypeState)
inferExp _ _ _ _ _ _ ex0 =
  case ex0 of
    VarE v -> undefined
    
-- | Type inference proceeds and fails sequentially.  It cannot
-- continue until the problem is resolved.
data Failure =
-- TODO: actually this needs something like TiHole to continue type checking after fixing
-- one or the other of the holes.
    FailedLocUnify      (LocExp,Selection) (LocExp,Selection)
    -- ^ Two locations we need to be equal, to unify, but don't.
                        
  | CyclicDependence -- ??
    -- ^ A cycle in the graph of value dependence between sizes, locations, and regular
    -- logical values.

-- | A hole that represents a paused type-inference process.
newtype TiHole = TiHole (Exp2 -> TiM (Exp2,Ty2))

-- | A context within a surrounding expression.  We could use an explicit datatype or a
-- generic zipper library for this, but instead we just use functions for holes.
newtype Hole = Hole (Exp2 -> Exp2)

-- | A selected subexpression that has focus.  A hole filled by a given subexpr.
data Selection = Selection Exp2 Hole

-- Helpers:               
--------------------------------------------------------------------------------
    
-- TODO: Compute an L2 type schema from an L1 type.


-- TODO: Instantiate a polymorphic type schema.


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
