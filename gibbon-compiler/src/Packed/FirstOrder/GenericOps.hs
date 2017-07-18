-- | Generic operations that are supported by the various intermediate
-- representations inside the compiler.

module Packed.FirstOrder.GenericOps
    (
       Interp(..)

     -- * Free Variables, generic interface
     , FreeVars(..)
    )
    where

import qualified Data.Set as S
import Packed.FirstOrder.Common

--------------------------------------------------------------------------------
-- Things which can be interpreted to yield a final, printed value. 
--------------------------------------------------------------------------------

-- | Pure Gibbon programs, at any stage of compilation, should always
-- be evaluatable to a unique value.  The only side effects are timing.
class Interp a where
  -- | Interpret while ignoring timing constructs, and dropping the
  -- corresponding output to stdout.
  interpNoLogs     :: RunConfig -> a -> String

  -- | Interpret and produce a "log" of output lines, as well as a
  -- final, printed result.  The output lines include timing information.
  interpWithStdout :: RunConfig -> a -> IO (String,[String])

----------------------------------------------------------------------------------------------------
-- Free Variables 
----------------------------------------------------------------------------------------------------

-- | Expression and program types which support a notion of free variables.
class FreeVars a where
    gFreeVars :: a -> S.Set Var

instance FreeVars () where
  gFreeVars () = S.empty 


----------------------------------------------------------------------------------------------------
-- Compiler passes used in multiple phases
----------------------------------------------------------------------------------------------------

{-
class (Show e, Out e) => Expression e where

-- IRs amenable to flattening
class Expression e => Flattenable e where
  gFlattenExp :: forall e . (Flattenable e) =>
                 DDefs (TyOf e) -> Env2 (TyOf e) -> e -> SyM e

instance (Flattenable e, Out l, Show l) => Flattenable (PreExp l e (UrTy l)) where

instance (Flattenable e, Out l, Show l) => Flattenable (Exp2 l (UrTy l)) where
    
-- IRs amenable to simplification/inlineTrivs
class Expression e => Simplifiable e where
  
-}
