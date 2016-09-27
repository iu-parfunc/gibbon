-- |

module Main where

import Data.List (sort)
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Time.Clock
import GHC.Generics
import System.Environment

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

leftmost (Leaf n) = n
leftmost (Node x _) = leftmost x

--------------------------------------------------------------------------------

bench :: Tree -> IO Tree
bench tr = evaluate $ force (add1Tree tr)

main =
 do args <- getArgs
    let power = case args of
                  [p] -> read p
                  _   -> error $ "Bad command line args.  Expected one number (exponent): " ++show args
    times <- forM [1..9] $ \_ -> do
      tr  <- buildTree power
      t1  <- getCurrentTime
      tr2 <- bench tr
      t2  <- getCurrentTime
      -- putStrLn $ "Test, leftmost leaf in output: " ++ show (leftmost tr2)
      -- putStrLn $ "Took "++ show (diffUTCTime t2 t1)
      putStr "."
      return (diffUTCTime t2 t1)
    let sorted = sort times
    putStrLn $ "\nAll times: " ++ show sorted
    putStrLn $ "SELFTIMED: "++ show (sorted !! 4)
