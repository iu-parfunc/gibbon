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
class Expression e where

-- IRs amenable to flattening
class Flattenable e where
--  gFlatten :: DDefs (UrTy l) -> Env2 (UrTy l) -> PreExp l e d -> SyM (PreExp l e d)
    
-- IRs amenable to simplification/inlineTrivs
class Simplifiable e where
  
-}
