data MultiList = MCons Int Int Int Int Float MultiList | MNil
{-# ANN type MultiList "Factored" #-}

{-# ANN mkMultiList "OPT:StoreScalarCounts" #-}
mkMultiList :: Int -> MultiList
mkMultiList len =
  if len <= 0
  then MNil
  else let rst = mkMultiList (len - 1)
           a = len
           b = len + 1
           c = len + 2
           d = len + 3
       in MCons a b c d 1.0 rst

sumMultiList :: MultiList -> Int
sumMultiList xs =
  case xs of
    MNil -> 0
    MCons a b c d f rst -> a + b + c + d + sumMultiList rst

{-# ANN add1MultiList "OPT:CanVectorize" #-}
add1MultiList :: MultiList -> MultiList
add1MultiList xs =
  case xs of
    MNil -> MNil
    MCons a b c d f rst ->
      MCons (a + 1) (b + 1) (c + 1) (d + 1) f (add1MultiList rst)

gibbon_main =
  let xs = mkMultiList 1000000
      xs' = iterate (add1MultiList xs)
  in sumMultiList xs'
