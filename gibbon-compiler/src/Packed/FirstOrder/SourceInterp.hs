{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Interpreter for the source language (L1) 
--
-- UNFINISHED / PLACEHOLDER

module Packed.FirstOrder.SourceInterp
    ( execAndPrint, interpProg
    , Value(..)
    , main
    ) where

import           Blaze.ByteString.Builder (Builder, toLazyByteString)
import           Blaze.ByteString.Builder.Char.Utf8 (fromString)
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Writer
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.List as L 
import           Data.Map as M
import           Data.Word
import           GHC.Generics    
import           Packed.FirstOrder.Common
import           Packed.FirstOrder.L1_Source
import qualified Packed.FirstOrder.L2_Traverse as L2
import           System.Clock
import           System.IO.Unsafe (unsafePerformIO)
import           Text.PrettyPrint.GenericPretty


import           Data.Sequence (Seq, ViewL ((:<)), (|>))
import qualified Data.Sequence as S 
import qualified Data.Foldable as F
import           Packed.FirstOrder.Passes.InlinePacked(pattern NamedVal)
import           Packed.FirstOrder.Passes.Cursorize( pattern WriteInt, pattern ReadInt, pattern NewBuffer
                                                   , pattern CursorTy, pattern ScopedBuffer)
    
-- TODO:
-- It's a SUPERSET, but use the Value type from TargetInterp anyway:
-- Actually, we should merge these into one type with a simple extension story.
-- import Packed.FirstOrder.TargetInterp (Val(..), applyPrim)

------------------------------------------------------------

instance Interp Prog where    
  interpNoLogs rc p = unsafePerformIO $ show . fst <$> interpProg rc p
  interpWithStdout rc p = do
   (v,logs) <- interpProg rc p
   return (show v, lines (B.unpack logs))

-- | HACK: we don't have a type-level distinction for when cursors are
-- allowed in the AST.  We use L2 as a proxy for this, allowing
-- cursors whenver executing L2, even though this is a bit premature
-- in the compiler pipeline.
instance Interp L2.Prog where
  interpNoLogs rc p2     = interpNoLogs     rc{rcCursors=True} (L2.revertToL1 p2)
  interpWithStdout rc p2 = interpWithStdout rc{rcCursors=True} (L2.revertToL1 p2)
          
------------------------------------------------------------

data SerializedVal = SerTag Word8 | SerInt Int
  deriving (Read,Eq,Ord,Generic, Show)

instance Out SerializedVal
instance NFData SerializedVal

instance Out Word8 where
  doc w       = doc       (fromIntegral w :: Int)
  docPrec n w = docPrec n (fromIntegral w :: Int)

instance Out a => Out (Seq a) where
  doc s       = doc       (F.toList s)
  docPrec n s = docPrec n (F.toList s)
                
-- | It's a first order language with simple values.
data Value = VInt Int
           | VBool Bool
           | VDict (M.Map Value Value)
-- FINISH:       | VList
           | VProd [Value]
           | VPacked Constr [Value]

           | VBuf (Seq SerializedVal)
             -- ^ We write values to the right end of this sequence.

  deriving (Read,Eq,Ord,Generic)

instance Out Value
instance NFData Value    
           
instance Show Value where                      
 show v =
  case v of
   VInt n   -> show n
   VBool b  -> if b then truePrinted else falsePrinted
   VProd ls -> "("++ concat(intersperse ", " (L.map show ls)) ++")"
   VPacked k ls -> k ++ show (VProd ls)
   VDict m      -> show (M.toList m)
                   
   VBuf s -> "<Buffer containing: "++show s++">"
                
type ValEnv = Map Var Value

------------------------------------------------------------
{-    
-- | Promote a value to a term that evaluates to it.
l1FromValue :: Value -> Exp
l1FromValue x =
  case x of
    (VInt y) -> __
    (VProd ls) -> __
    (VPacked y1 y2) -> __
-}

execAndPrint :: RunConfig -> Prog -> IO ()
execAndPrint rc prg = do
  (val,logs) <- interpProg rc prg
  B.putStr logs
  case val of
    -- Special case: don't print void return:
    VProd [] -> return () -- FIXME: remove this.
    _ -> print val   

type Log = Builder

-- TODO: add a flag for whether we support cursors:
    
-- | Interpret a program, including printing timings to the screen.
interpProg :: RunConfig -> Prog -> IO (Value, B.ByteString)
-- Print nothing, return "void"              :
interpProg _ Prog {mainExp=Nothing} = return $ (VProd [], B.empty)
interpProg rc Prog {ddefs,fundefs, mainExp=Just e} =
    do (x,logs) <- runWriterT (interp e)
       return (x, toLazyByteString logs)

 where
  applyPrim :: Prim -> [Value] -> Value
  applyPrim p ls =
   case (p,ls) of
     (MkTrue,[])             -> VBool True
     (MkFalse,[])            -> VBool False
     (AddP,[VInt x, VInt y]) -> VInt (x+y)
     (SubP,[VInt x, VInt y]) -> VInt (x-y)
     (MulP,[VInt x, VInt y]) -> VInt (x*y)
     (EqSymP,[VInt x, VInt y]) -> VBool (x==y)
     (EqIntP,[VInt x, VInt y]) -> VBool (x==y)
     ((DictInsertP _ty),[VDict mp, key, val]) -> VDict (M.insert key val mp)
     ((DictLookupP _),[VDict mp, key])        -> mp # key
     ((DictEmptyP _),[])                      -> VDict M.empty
     ((ErrorP msg _ty),[]) -> error msg
     (SizeParam,[]) -> VInt (rcSize rc)
     oth -> error $ "unhandled prim or wrong number of arguments: "++show oth

  interp :: Exp -> WriterT Log IO Value
  interp = go M.empty
    where
      {-# NOINLINE goWrapper #-}
      goWrapper !_ix env ex = go env ex
      
      go :: ValEnv -> Exp -> WriterT Log IO Value
      go env x0 =
          case x0 of
            LitE c         -> return $ VInt c
            VarE v         -> return $ env # v
            PrimAppE p ls  -> do args <- mapM (go env) ls
                                 return $ applyPrim p args            
            ProjE ix ex -> do VProd ls <- go env ex
                              return $ ls !! ix

            --- Pattern synonyms specific to post-cursorize ASTs:
            NewBuffer    -> return $ VBuf S.empty
            ScopedBuffer -> return $ VBuf S.empty
            WriteInt v ex -> do let VBuf s = env # v
                                VInt n <- go env ex
                                return $ VBuf $ s |> SerInt n
            ReadInt v -> do
              let VBuf s = env # v
              case S.viewl s of
                SerInt n :< rst -> return $ VProd [VInt n, VBuf rst]
                S.EmptyL        -> error "SourceInterp: ReadInt on empty cursor/buffer."
                oth :< _ ->
                 error $"SourceInterp: ReadInt expected Int in buffer, found: "++show oth
                                     
            AppE f b -> do rand <- go env b
                           let FunDef{funArg=(vr,_),funBody}  = fundefs # f
                           go (M.insert vr rand env) funBody

            (CaseE x1 ls1) -> do
                   v <- go env x1
                   case v of
                     VPacked k ls2 ->
                         let (_,vs,rhs) = lookup3 k ls1
                             env' = M.union (M.fromList (zip vs ls2))
                                    env
                         in go env' rhs
                     _ -> error$ "SourceInterp: type error, expected data constructor, got:\n "++sdoc v++
                                 "\nWhen evaluating scrutinee of case expression:\n "++sdoc x1

            NamedVal _ _ bd -> go env bd

            (LetE (v,_ty,rhs) bod) -> do
              rhs' <- go env rhs
              let env' = M.insert v rhs' env
              go env' bod

            (MkProdE ls) -> VProd <$> mapM (go env) ls
            -- TODO: Should check this against the ddefs.
            (MkPackedE k ls) -> do
                args <- mapM (go env) ls
                case args of
                -- Constructors are overloaded.  They have different behavior depending on
                -- whether we are AFTER Cursorize or not.
                  [ VBuf sq ] | rcCursors rc -> return $ VBuf $ sq |> SerTag (getTagOfDataCon ddefs k)
                  _ -> return $ VPacked k args


            TimeIt bod _ isIter -> do
                let iters = if isIter then rcIters rc else 1
                !_ <- return $! force env
                st <- lift $ getTime clk          
                val <- foldM (\ _ i -> goWrapper i env bod)
                              (error "Internal error: this should be unused.")
                           [1..iters]
                en <- lift $ getTime clk
                let tm = fromIntegral (toNanoSecs $ diffTimeSpec en st)
                          / 10e9 :: Double         
                if isIter
                 then do tell$ fromString $ "ITERS: "++show iters       ++"\n"
                         tell$ fromString $ "SIZE: " ++show (rcSize rc) ++"\n"
                         tell$ fromString $ "BATCHTIME: "++show tm      ++"\n"
                 else tell$ fromString $ "SELFTIMED: "++show tm ++"\n"
                return $! val
              
                                
            IfE a b c -> do v <- go env a
                            case v of
                             VBool flg -> if flg
                                          then go env b
                                          else go env c
                             oth -> error$ "interp: expected bool, got: "++show oth

            MapE _ _bod    -> error "SourceInterp: finish MapE"
            FoldE _ _ _bod -> error "SourceInterp: finish FoldE"
                              

clk :: Clock
clk = Monotonic

                                               
-- Misc Helpers
--------------------------------------------------------------------------------

lookup3 :: (Eq k, Show k, Show a, Show b) =>
           k -> [(k,a,b)] -> (k,a,b)
lookup3 k ls = go ls
  where
   go [] = error$ "lookup3: key "++show k++" not found in list:\n  "++take 80 (show ls)
   go ((k1,a1,b1):r)
      | k1 == k   = (k1,a1,b1)
      | otherwise = go r
                    
--------------------------------------------------------------------------------

p1 :: Prog
p1 = Prog emptyDD  M.empty
          (Just (LetE ("x", IntTy, LitE 3) (VarE "x")))
         -- IntTy

main :: IO ()
main = execAndPrint (RunConfig 1 1 dbgLvl False) p1



       
