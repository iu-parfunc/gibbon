-- VectorInt8Select: List (Factored).
-- Functions: mkList, classify, sumList.
-- Annotated: MayVectorize on classify; StoreScalarCounts on mkList.
data List = Cons Int8 List | Nil
{-# ANN type List "Factored" #-}
{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int8 -> List
mkList n =
  if n <= 0
  then Nil
  else Cons (if mod n 5 == 0 then -128
             else if mod n 5 == 1 then 127
             else if mod n 5 == 2 then (0 - n)
             else if mod n 5 == 3 then 0
             else n)
            (mkList (n - 1))
{-# ANN classify "OPT:MayVectorize" #-}
classify :: List -> List
classify xs = case xs of
                Nil -> Nil
                Cons i rst -> Cons (if i == -128 then 7 else 1) (classify rst)
sumList :: List -> Int64
sumList xs = case xs of
               Nil -> 0
               Cons i rst -> toInt64 i + sumList rst
gibbon_main = let xs = mkList 37
                  ys = classify xs
              in sumList ys
