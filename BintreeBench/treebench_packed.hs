
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Data.Word
import Data.Int
import Data.Bits
import Data.Time.Clock
import System.Environment
import Control.Monad
import Data.List (sort)

-- import System.Clock

    
type TreeTag = Word8

leafTag, nodeTag, indirectTag :: TreeTag
leafTag = 0
nodeTag = 1
indirectTag = 2

type TreeRef = Ptr Word8
type TreeNum = Int

fillTree :: TreeRef -> Int -> Int64 -> IO TreeRef
fillTree cursor n root =
    if (n == 0)
    then do poke (castPtr cursor :: Ptr TreeTag) leafTag
            let cursor' = advancePtr cursor 1
            poke (castPtr cursor' :: Ptr Int64) root
            return $ castPtr $ advancePtr (castPtr cursor' :: Ptr Int64) 1
    else do poke (castPtr cursor :: Ptr TreeTag) nodeTag
            cur2 <- fillTree (advancePtr cursor 1) (n-1) root
            fillTree cur2 (n-1) (root + (1 `shiftL` (n-1)))

treeSize :: Int -> Int
treeSize n =
    let leaves = 1 `shiftL` n
        nodes = leaves - 1
    in 8 * leaves + 1 * (nodes+leaves)

buildTree :: Int -> Int64 -> IO TreeRef
buildTree n root = do
  let bytes = treeSize n
  buf <- mallocBytes bytes
  fillTree buf n root
  return buf

printTree :: TreeRef -> IO TreeRef
printTree t =
    do t' <- peek (castPtr t) :: IO TreeTag
       case t' of
         0 -> do val <- peek $ castPtr $ advancePtr t 1 :: IO Int64
                 putStr $ show val
                 let t'' = castPtr $ advancePtr t 1 :: Ptr Int64
                     t''' = advancePtr t'' 1
                 return $ castPtr t'''
         1 -> do let t'' = advancePtr t 1
                 putStr "("
                 t''' <- printTree t''
                 putStr ","
                 t'''' <- printTree t'''
                 putStr ")"
                 return t''''
         2 -> undefined

type RefPair = (TreeRef, TreeRef)

add1Tree :: TreeRef -> TreeRef -> IO TreeRef
add1Tree t tout =
    do t' <- peek (castPtr t) :: IO TreeTag
       case t' of
         0 -> do poke tout leafTag
                 let t'' = advancePtr t 1
                     tout'' = advancePtr tout 1
                 v <- peek $ castPtr t'' :: IO Int64
                 poke (castPtr tout'') (v+1)
                 let t''' = advancePtr (castPtr t'' :: Ptr Int64) 1
                 return $ castPtr t'''
         1 -> do poke tout nodeTag
                 let t'' = advancePtr t 1
                     tout'' = advancePtr tout 1
                 t2 <- add1Tree t'' tout''
                 let tout''' = plusPtr tout'' (minusPtr t2 t'')
                 add1Tree t2 tout'''
         2 -> undefined
              


main :: IO ()
main = do
  ta <- buildTree 3 1
  tb <- mallocBytes (treeSize 3) :: IO TreeRef
  printTree ta
  putStrLn ""
  add1Tree ta tb
  printTree tb
  putStrLn ""

  args <- getArgs
  let (power,iters) =
        case args of
          [p,i] -> (read p, read i)
          _   -> error $ "Bad command line args.  Expected <depth> <iters>: " ++show args
  putStrLn $ "Benchmarking depth "++show power++", iters "++show iters
  ta  <- buildTree power 1
  tb  <- mallocBytes (treeSize power) :: IO TreeRef
  times <- forM [1 .. iters] $ \_ -> do
             t1  <- getCurrentTime
             tr2 <- add1Tree ta tb
             t2  <- getCurrentTime
             putStr "."
             return (diffUTCTime t2 t1)
  let sorted = sort times
  putStrLn $ "\nAll times: " ++ show sorted
  putStrLn $ "SELFTIMED: "++ show (sorted !! 4)


  return ()
