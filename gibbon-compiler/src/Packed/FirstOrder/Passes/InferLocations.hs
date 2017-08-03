-- | Convert from L1 to L2.

module Packed.FirstOrder.Passes.InferLocations where

import Packed.FirstOrder.Common
import Packed.FirstOrder.L1.Syntax as L1
import qualified Packed.FirstOrder.L2.Syntax as L2

-- | The compiler pass that converts from L1 to L2, inferring and
-- inserting location variables into the program.
inferLocs :: L1.Prog -> SyM L2.Prog
inferLocs = error "FINISHME"

-- inferExp :: (DDefs L1.Ty, FunEnv) -> LocEnv -> L1.Exp -> SyM (Set Effect, Loc)

-- inferExp :: DDefs Ty2 -> Env2 Ty2 -> NewFuns
--          -> ConstraintSet -> RegionSet -> LocationTypeState -> Exp2
--          -> TcM (Ty2, LocationTypeState)


-- Compute an L2 type schema from an L1 type.


-- Instantiate a polymorphic type schema.




test :: L2.Prog
test = fst $ runSyM 0 $ inferLocs L1.add1Prog
