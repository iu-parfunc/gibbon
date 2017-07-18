-- | Generic operations that are supported by the various intermediate
-- representations inside the compiler.

module Packed.FirstOrder.GenericOps
    (
       Interp(..)

     -- * Free Variables, generic interface
     , FreeVars(..)


     -- * Genereric interface for expressions and select passes
     , Expression(..), Flattenable(..), Simplifiable(..)
    )
    where

import qualified Data.Set as S
import Packed.FirstOrder.Common
import Text.PrettyPrint.GenericPretty (Out)
    
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

-- | A generic interface to expressions found in different phases of
-- the compiler.
class (Show e, Out e) => Expression e where
  type TyOf e 
      
-- | IRs amenable to flattening
class Expression e => Flattenable e where
  gFlattenExp :: DDefs (TyOf e) -> Env2 (TyOf e) -> e -> SyM e
    
-- | IRs amenable to simplification/inlineTrivs
class Expression e => Simplifiable e where
  gInlineTrivExp :: DDefs (TyOf e) -> e -> e

-- | A dummy instance for "no-extension" extension point.
instance Expression () where
  type TyOf () = ()
                    
-- | A dummy instance for "no-extension" extension point.
instance Flattenable () where
  gFlattenExp _ _ () = return ()

-- | A dummy instance for "no-extension" extension point.
instance Simplifiable () where
  gInlineTrivExp _ () = ()
