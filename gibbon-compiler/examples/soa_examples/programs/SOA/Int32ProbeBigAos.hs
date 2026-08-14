-- Int32 audit probe (overflow discipline): identical program, Linear layout.
data BList = BCons Int BList | BNil
{-# ANN type BList "Linear" #-}


mkBList :: Int -> BList
mkBList n =
  if n <= 0
  then BNil
  else let rst = mkBList (n - 1)
       in BCons (n * 1000000) rst

{-# ANN add1B "OPT:CanVectorize" #-}
add1B :: BList -> BList
add1B xs =
  case xs of
    BNil -> BNil
    BCons a rst -> BCons (a + 7000000) (add1B rst)

sumB :: BList -> Int
sumB xs =
  case xs of
    BNil -> 0
    BCons a rst -> a + sumB rst

gibbon_main =
  let xs = mkBList 2000
      ys = add1B xs
  in sumB ys
