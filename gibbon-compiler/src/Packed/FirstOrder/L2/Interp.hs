-- | Interpreter reducing L2 programs to values.

module Packed.FirstOrder.L2.Interp
    () -- Instances only.
    where

import           Packed.FirstOrder.GenericOps(Interp, interpNoLogs, interpWithStdout)   
import qualified Packed.FirstOrder.L2.Syntax as L2
import           Packed.FirstOrder.L2.Syntax (pattern NamedVal)
import           Packed.FirstOrder.L1.Interp ()
    
import           Blaze.ByteString.Builder (Builder, toLazyByteString)
import           Blaze.ByteString.Builder.Char.Utf8 (fromString)
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Writer
import           Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.List as L
import           Data.Map as M
import           Data.IntMap as IM
import           Data.Word
import Data.Char
import           GHC.Generics
import           GHC.Stack (errorWithStackTrace)
import           Packed.FirstOrder.Common
import           Packed.FirstOrder.GenericOps(Interp, interpNoLogs, interpWithStdout)
import           Packed.FirstOrder.L1.Syntax   as L1
import           System.Clock
import           System.IO.Unsafe (unsafePerformIO)
import           Text.PrettyPrint.GenericPretty


import           Data.Sequence (Seq, ViewL ((:<)), (|>))
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import           Packed.FirstOrder.L2.Syntax ( pattern WriteInt, pattern ReadInt, pattern NewBuffer
                                               , pattern ScopedBuffer, pattern AddCursor)
    
-- | HACK: we don't have a type-level distinction for when cursors are
-- allowed in the AST.  We use L2 as a proxy for this, allowing
-- cursors whenver executing L2, even though this is a bit premature
-- in the compiler pipeline.
instance Interp L2.Prog where
  interpNoLogs rc p2     = interpNoLogs     rc{rcCursors=True} (L2.revertToL1 p2)
  interpWithStdout rc p2 = interpWithStdout rc{rcCursors=True} (L2.revertToL1 p2)
