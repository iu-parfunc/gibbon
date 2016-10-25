-- |

module Main where

import Data.List (sort)
import Control.Exception
import Control.Monad
import Data.Time.Clock
import System.Environment

import GHC.Conc (par, pseq)
    
-- Strict version
--------------------------------------------------------------------------------

data Tree = Leaf {-# UNPACK #-} !Int
          | Node !Tree !Tree
  deriving Show
            
-- | Build a fully-evaluated tree
buildTree :: Int -> IO Tree
buildTree n = evaluate $ go 1 n
  where
  go root 0 = Leaf root
  go root n = Node (go root (n-1))
                   (go (root + 2^(n-1)) (n-1))

add1Tree :: Tree -> Tree
add1Tree (Leaf n)   = Leaf (n+1)
add1Tree (Node x y) = Node (add1Tree x) (add1Tree y)

add1Par :: Tree -> Int -> Tree
add1Par x          0 = add1Tree x
add1Par (Leaf n)   i = Leaf (n+1)
add1Par (Node x y) i =
    let x' = add1Par x (i-1)
        y' = add1Par y (i-1)
    in x' `par` y' `pseq`
       Node x' y'
                     
leftmost (Leaf n) = n
leftmost (Node x _) = leftmost x

--------------------------------------------------------------------------------

timeit act =
    do tm1 <- getCurrentTime
       x <- act
       tm2 <- getCurrentTime
       return (tm1,tm2,x)

{-# NOINLINE bench #-}
bench :: Int -> Tree -> IO Tree
bench _ tr = evaluate (add1Tree tr)

{-# NOINLINE benchPar #-}
benchPar :: Int -> Tree -> IO Tree
benchPar _ tr = evaluate (add1Par tr 6)

             
main :: IO ()
main =
 do args <- getArgs
    let (mode,power,iters) =
                case args of
                  [md,p,i] -> (md,read p,read i)
                  _   -> error $ "Bad command line args." ++
                                 "  Expected <mode>=par|seq <depth> <iters> got: " ++
                                 show args
    tr0  <- buildTree power
    times <- forM [1 .. iters :: Int] $ \ix -> do      
      (st,en,tr') <- case mode of
                     "par" -> timeit (benchPar ix tr0)
                     "seq" -> timeit (bench    ix tr0)
      putStr "."
      evaluate (leftmost tr')
      return (diffUTCTime en st)
    let sorted = sort times
    putStrLn $ "\nAll times: " ++ show sorted
    putStrLn $ "MEDIANTIME: "++ show (sorted !! (iters `quot` 2))
