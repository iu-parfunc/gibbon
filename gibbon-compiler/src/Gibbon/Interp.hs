{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Gibbon.Interp
-- Description : Things related to interpreting Gibbon programs.
-----------------------------------------------------------------------------

module Gibbon.Interp
  ( Interp(..)

  , Store(..), insertIntoStore, lookupInStore
  , Buffer(..), emptyBuffer, insertAtBuffer
  , SerializedVal(..), Value(..), Log, ValEnv
  , LanguageValue (..), valueFromLanguageValue

  , execAndPrint, deserialize, strToInt, lookup3
  ) where

import Control.Monad.State
import Control.DeepSeq
import Data.ByteString.Builder (Builder)
import Data.Char (ord)
import Data.Foldable as F
import Data.List as L
import Data.Map as M
import Data.Sequence (Seq, ViewL(..))
import Data.Word (Word8)
import System.IO.Unsafe (unsafePerformIO)
import Text.PrettyPrint.GenericPretty
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Sequence as S

import Gibbon.L1.Syntax
import Gibbon.Common

--------------------------------------------------------------------------------
-- Things which can be interpreted to yield a final, printed value.
--------------------------------------------------------------------------------

-- | Pure Gibbon programs, at any stage of compilation, should always
-- be evaluatable to a unique value.  The only side effects are timing.
class Interp a where
  {-# MINIMAL interpProg #-}

  interpProg :: SourceLanguage -> RunConfig -> a -> IO (LanguageValue, B.ByteString)

  -- | Interpret while ignoring timing constructs, and dropping the
  -- corresponding output to stdout.
  interpNoLogs :: SourceLanguage -> RunConfig -> a -> String
  interpNoLogs src rc p = unsafePerformIO $ show . fst <$> interpProg  src rc p

  -- | Interpret and produce a "log" of output lines, as well as a
  -- final, printed result.  The output lines include timing information.
  interpWithStdout :: SourceLanguage -> RunConfig -> a -> IO (String,[String])
  interpWithStdout src rc p = do
   (v,logs) <- interpProg src rc p
   return (show v, lines (B.unpack logs))


-- Stores and buffers:
------------------------------------------------------------

-- | A store is an address space full of buffers.
data Store = Store (M.Map Var Buffer)
  deriving (Read,Eq,Ord,Generic, Show)

instance Out Store

insertIntoStore :: MonadState Store m => Var -> Buffer -> m ()
insertIntoStore v buf = modify (\(Store x) -> Store (M.insert v buf x))

lookupInStore :: MonadState Store m => Var -> m (Maybe Buffer)
lookupInStore v = do
  Store store <- get
  return $ M.lookup v store

data Buffer = Buffer (Seq SerializedVal)
  deriving (Read,Eq,Ord,Generic, Show)

instance Out Buffer

emptyBuffer :: Buffer
emptyBuffer = Buffer S.empty

-- | Insert a value at a particular index in the buffer
insertAtBuffer :: Int -> SerializedVal -> Buffer -> Buffer
insertAtBuffer i v (Buffer b) = Buffer (S.insertAt i v b)

data SerializedVal = SerTag Word8 DataCon | SerInt Int | SerBool Int
  deriving (Read,Eq,Ord,Generic, Show)

byteSize :: SerializedVal -> Int
byteSize (SerInt _) = 8 -- FIXME: get this constant from elsewhere.
byteSize (SerBool _) = 8
byteSize (SerTag _ _) = 1

instance Out SerializedVal
instance NFData SerializedVal

instance Out Word8 where
  doc w       = doc       (fromIntegral w :: Int)
  docPrec n w = docPrec n (fromIntegral w :: Int)

instance Out a => Out (Seq a) where
  doc s       = doc       (F.toList s)
  docPrec n s = docPrec n (F.toList s)

-- Values
-------------------------------------------------------------

-- | It's a first order language with simple values.
data Value = VInt Int
           | VSym String
           | VBool Bool
           | VDict (M.Map Value Value)
           | VProd [Value]
           | VPacked DataCon [Value]
           | VLoc { bufID :: Var, offset :: Int }
           | VCursor { bufID :: Var, offset :: Int }
             -- ^ Cursor are a pointer into the Store plus an offset into the Buffer.
  deriving (Read,Eq,Ord,Generic)

newtype LanguageValue =  LanguageValue (SourceLanguage, Value)

instance Out Value
instance NFData Value

instance Show LanguageValue where
  show (LanguageValue (src, v)) =
    case src of 
      Hskl ->
        case v of
          VInt n   -> show n
          VSym s   -> s
          VBool b  -> if b then truePrinted Hskl else falsePrinted Hskl
          VProd ls -> "("++ concat(intersperse ", " (L.map show (lls))) ++")"
            where lls = fmap (\vl -> LanguageValue (Hskl, vl)) ls
          VDict m      -> show tupv
            where tupv = fmap (\vl -> case vl of (v1, v2) -> (LanguageValue(Hskl, v1), LanguageValue(Hskl, v2))) (M.toList m)
          VPacked k ls -> k ++ show (LanguageValue(Hskl, VProd ls)) 
          VLoc buf off -> "<location "++show buf++", "++show off++">"
          VCursor idx off -> "<cursor "++show idx++", "++show off++">" 
      Gibbon ->
        case v of
          VInt n   -> show n
          VSym s   -> "'" ++ s
          VBool b  -> if b then truePrinted Gibbon else falsePrinted Gibbon
          VProd ls -> "'#("++ concat(intersperse " " (L.map show lls)) ++")"
            where lls = fmap (\vl -> LanguageValue (Gibbon, vl)) ls
          VDict m      -> show tupv
            where tupv = fmap (\vl -> case vl of (v1, v2) -> (LanguageValue(Gibbon, v1), LanguageValue(Gibbon, v2))) (M.toList m)
          VPacked k ls -> "(" ++ k ++ concat (L.map ((" "++) . show) lls) ++ ")"  
            where lls = fmap (\vl -> LanguageValue (Gibbon, vl)) ls
          VLoc buf off -> "<location "++show buf++", "++show off++">"
          VCursor idx off -> "<cursor "++show idx++", "++show off++">"

type Log = Builder
type ValEnv = M.Map Var Value

--------------------------------------------------------------------------------

-- | Code to read a final answer back out.
deserialize :: (Out ty) => SourceLanguage -> DDefs ty -> Seq SerializedVal -> LanguageValue
deserialize src ddefs seq0 = final
 where
  ([final],_) = readN 1 seq0

  readN 0 seq1 = ([],seq1)
  readN n seq1 =
     case S.viewl seq1 of
       S.EmptyL -> error $ "deserialize: unexpected end of sequence: "++ndoc seq0
       SerInt i :< rst ->
         let (more,rst') = readN (n-1) rst
         in ( LanguageValue(src, (VInt i)) : more, rst')

       SerBool i :< rst ->
         let (more,rst') = readN (n-1) rst
             -- 1 is True
             b = i /= 0
         in ( LanguageValue(src, (VBool b)) : more, rst')

       SerTag _ k :< rst ->
         let (args,rst')  = readN (length (lookupDataCon ddefs k)) rst
             (more,rst'') = readN (n-1) rst'
         in ( LanguageValue(src, (VPacked k (fmap valueFromLanguageValue args))) : more, rst'')


execAndPrint :: (Interp (Prog ex)) => SourceLanguage -> RunConfig -> Prog ex -> IO ()
execAndPrint src rc prg = do
  (val,logs) <- interpProg src rc prg
  B.putStr logs
  case val of
    -- Special case: don't print void return:
    LanguageValue (_, VProd []) -> return () -- FIXME: remove this.
    _ -> print val

strToInt :: String -> Int
strToInt = product . L.map ord

lookup3 :: (Eq k, Show k, Show a, Show b) => k -> [(k,a,b)] -> (k,a,b)
lookup3 k ls = go ls
  where
   go [] = error$ "lookup3: key "++show k++" not found in list:\n  "++L.take 80 (show ls)
   go ((k1,a1,b1):r)
      | k1 == k   = (k1,a1,b1)
      | otherwise = go r

valueFromLanguageValue :: LanguageValue -> Value 
valueFromLanguageValue lv = case lv of 
 LanguageValue (_, v) -> v


languageValueFromValue :: SourceLanguage -> Value -> LanguageValue
languageValueFromValue src v = LanguageValue (src, v)
