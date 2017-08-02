-- | Convert from L1 to L2.

module Packed.FirstOrder.Passes.InferLocations where

import Packed.FirstOrder.Common
import Packed.FirstOrder.L1.Syntax as L1
import qualified Packed.FirstOrder.L2.Syntax as L2

-- | The compiler pass that converts from L1 to L2, inferring and
-- inserting location variables into the program.
inferLocs :: L1.Prog -> SyM L2.Prog
inferLocs = error "FINISHME"

