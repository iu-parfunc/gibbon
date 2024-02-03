module MapOnVector where 

import Gibbon.Vector

add1 :: Int -> Int 
add1 x = x + 1


gibbon_main =
  let n = sizeParam
      ls :: Vector Int
      ls = generate sizeParam (\i -> n-i)
      -- generate_loop is the recursive function to look at, if it is tail_recursive or not.
      ls' = iterate (map add1 ls)
      --_ = printVec (\i -> printint i) ls'
    in (ls,ls')