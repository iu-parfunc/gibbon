{-# LANGUAGE BangPatterns #-}

module Main where

import Data.List (sort)
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Time.Clock
import GHC.Generics
import System.Environment

-- import System.Clock
    
-- Lazy version
--------------------------------------------------------------------------------

data Tree = Leaf {-# UNPACK #-} !Int
          | Node Tree Tree
-- deriving Generic

instance NFData Tree where
  rnf (Leaf _) = ()
  rnf (Node x y) = rnf x `seq` rnf y

-- | Build a fully-evaluated tree
buildTree :: Int -> IO Tree
buildTree n = evaluate $ force $ go 1 n
  where
  go root 0 = Leaf root
  go root n = Node (go root (n-1))
                   (go (root + 2^(n-1)) (n-1))

add1Tree :: Tree -> Tree
add1Tree (Leaf n)   = Leaf (n+1)
add1Tree (Node x y) = Node (add1Tree x) (add1Tree y)

sumtree :: Tree -> Int
sumtree (Leaf n)   = n
sumtree (Node x y) = (sumtree x) + (sumtree y)
                      
leftmost (Leaf n) = n
leftmost (Node x _) = leftmost x

--------------------------------------------------------------------------------

{-# NOINLINE benchAdd1 #-}
benchAdd1 :: Tree -> IO Tree
benchAdd1 tr = evaluate $ force (add1Tree tr)

{-# NOINLINE benchBuild #-}
benchBuild :: Int -> IO Tree
benchBuild n = buildTree n

{-# NOINLINE benchSum #-}
benchSum :: Tree -> IO Int
benchSum tr = evaluate $ sumtree tr
               
benchOnInt :: Int -> (Int -> IO ()) -> IO ()
benchOnInt iters act = do
    putStrLn $ "ITERS: "++show iters
    t1  <- getCurrentTime  
    for_ 1 iters act 
    t2  <- getCurrentTime
    let diffT = diffUTCTime t2 t1
    putStrLn $ "BATCHTIME: " ++ show (fromRational (toRational diffT) :: Double)

-- TODO: switch to Monotonic
    -- en <- return $! unsafePerformIO $ getTime clk
    -- let TimeVal st = env # begin
    --     -- tm   = diffUTCTime en st
    --     tm = fromIntegral (toNanoSecs $ diffTimeSpec en st)

benchOnTree :: (Int,Int) -> (Tree -> IO ()) -> IO ()
benchOnTree (iters,sz) act = do
    tr0 <- buildTree sz
    putStrLn $ "ITERS: "++show iters
    t1  <- getCurrentTime  
    for_ 1 iters $ \_ -> act tr0
    t2  <- getCurrentTime
    let diffT = diffUTCTime t2 t1
    putStrLn $ "BATCHTIME:" ++ show (fromRational (toRational diffT) :: Double)

             
main =
 do args <- getArgs
    let (mode,power,iters) =
            case args of
              [m,p,i] -> (m,read p, read i)
              _   -> error $ "Bad command line args.  Expected <sum|build|add1> <depth> <iters>: " ++show args
    putStrLn $ "Benchmarking depth "++show power++", iters "++show iters
    putStrLn $ "SIZE: "++show power
    case mode of
      "build" -> benchOnInt   iters        (void . benchBuild)
      "sum"   -> benchOnTree (iters,power) (void . benchSum)
      "add1"  -> benchOnTree (iters,power) (void . benchAdd1)

    -- times <- forM [1 .. iters] $ \ix -> do
    --   t1  <- getCurrentTime
    --   tr2 <- bench ix tr
    --   t2  <- getCurrentTime
    --   -- putStrLn $ "Test, leftmost leaf in output: " ++ show (leftmost tr2)
    --   -- putStrLn $ "Took "++ show (diffUTCTime t2 t1)
    --   putStr "."
    --   return (diffUTCTime t2 t1)
    -- let sorted = sort times
    -- putStrLn $ "\nAll times: " ++ show sorted
    -- putStrLn $ "SELFTIMED: "++ show (sorted !! 4)


--------------------------------------------------------------------------------

----------------------------------------
-- Inclusive/Inclusive
for_ :: Monad m => Int -> Int -> (Int -> m a) -> m ()
for_ start end fn = loop start
   where loop !i | i > end = return ()
                 | otherwise = fn i >> loop (i+1)
{-# INLINE for_ #-}
             
{-# INLINE rep #-}
rep :: Monad m => m a -> Int -> m ()
rep m n = for_ 1 n $ \_ -> m
----------------------------------------

