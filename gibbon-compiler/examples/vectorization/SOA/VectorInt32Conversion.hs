-- VectorInt32Conversion: List (Factored).
-- Functions: mkList, narrowRoundTrip, sumList.
-- Annotated: MayVectorize on narrowRoundTrip; StoreScalarCounts on mkList.
data List = Cons Int32 List | Nil
{-# ANN type List "Factored" #-}

{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int32 -> List
mkList n = if n <= 0 then Nil else Cons n (mkList (n - 1))

-- Round-trips through Int8, which truncates: values above 127 wrap.
{-# ANN narrowRoundTrip "OPT:MayVectorize" #-}
narrowRoundTrip :: List -> List
narrowRoundTrip xs =
  case xs of
    Nil -> Nil
    Cons i rst -> Cons (toInt32 (toInt8 i)) (narrowRoundTrip rst)

sumList :: List -> Int32
sumList xs = case xs of
               Nil -> 0
               Cons i rst -> i + sumList rst

gibbon_main =
  let xs = mkList 13
      ys = narrowRoundTrip xs
  in sumList ys
