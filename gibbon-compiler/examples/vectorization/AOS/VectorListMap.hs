data List = Cons Int List | Nil
{-# ANN type List "Linear" #-}

mkList :: Int -> List
mkList n =
  if n <= 0
  then Nil
  else Cons n (mkList (n - 1))

{-# ANN add3 "OPT:MayVectorize" #-}
add3 :: List -> List
add3 xs =
  case xs of
    Nil -> Nil
    Cons i rst -> Cons (i + 3) (add3 rst)

sumList :: List -> Int
sumList xs =
  case xs of
    Nil -> 0
    Cons i rst -> i + sumList rst

gibbon_main =
  let xs = mkList 64
      ys = add3 xs
  in sumList ys
