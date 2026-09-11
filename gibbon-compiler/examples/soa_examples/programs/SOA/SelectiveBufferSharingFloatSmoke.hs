data List = Cons Int Float List | Nil
{-# ANN type List "Factored" #-}

{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int -> List
mkList len =
  if len <= 0
  then Nil
  else let rst = mkList (len - 1)
       in Cons len 1.0 rst

{-# ANN add1KeepFloat "OPT:MayVectorize" #-}
add1KeepFloat :: List -> List
add1KeepFloat xs =
  case xs of
    Nil -> Nil
    Cons i f rst -> Cons (i + 1) f (add1KeepFloat rst)

sumList :: List -> Int
sumList xs =
  case xs of
    Nil -> 0
    Cons i _ rst -> i + sumList rst

sumFloatInt :: List -> Int
sumFloatInt xs =
  case xs of
    Nil -> 0
    Cons _ f rst -> let _ = f
                    in 0 + sumFloatInt rst

gibbon_main =
  let xs = mkList 100
      ys = add1KeepFloat xs
  in sumList ys + sumFloatInt ys
