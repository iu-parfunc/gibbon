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

-- | Pure Gibbon programs, at any stage of compilation, should always
-- be evaluatable to a unique value.  The only side effects are timing
class Interp a where
  -- | Interpret while ignoring timing constructs, and dropping the
  -- corresponding output to stdout.
  interpNoLogs     :: RunConfig -> a -> String

  -- | Interpret and produce a "log" of output lines, as well as a
  -- final, printed result.
  interpWithStdout :: RunConfig -> a -> IO (String,[String])

----------------------------------------------------------------------------------------------------
-- Free Variables 
----------------------------------------------------------------------------------------------------

-- | Expression and program types which support a notion of free variables.
class FreeVars a where
    gFreeVars :: a -> S.Set Var

instance FreeVars () where
  gFreeVars () = S.empty 

