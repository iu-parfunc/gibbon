-- VectorInt32Boundary: List (Factored).
-- Functions: mkList, classify, sumList.
-- Annotated: MayVectorize on classify; StoreScalarCounts on mkList.
data List = Cons Int32 List | Nil
{-# ANN type List "Factored" #-}

{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int32 -> List
mkList n =
  if n <= 0
  then Nil
  else Cons (if mod n 5 == 0 then -2147483648
             else if mod n 5 == 1 then 2147483647
             else if mod n 5 == 2 then (0 - n)
             else if mod n 5 == 3 then 0
             else n)
            (mkList (n - 1))

-- Equality against INT32_MIN drives _mm_cmpeq_epi32 + the bitwise select.
{-# ANN classify "OPT:MayVectorize" #-}
classify :: List -> List
classify xs =
  case xs of
    Nil -> Nil
    Cons i rst -> Cons (if i == -2147483648 then 7 else 1) (classify rst)

sumList :: List -> Int32
sumList xs =
  case xs of
    Nil -> 0
    Cons i rst -> i + sumList rst

gibbon_main =
  let xs = mkList 14
      ys = classify xs
  in sumList ys
