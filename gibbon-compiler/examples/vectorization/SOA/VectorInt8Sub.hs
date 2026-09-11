-- VectorInt8Sub: List (Factored).
-- Functions: mkList, sub3, sumList.
-- Annotated: MayVectorize on sub3; StoreScalarCounts on mkList.
data List = Cons Int8 List | Nil
{-# ANN type List "Factored" #-}
{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int8 -> List
mkList n = if n <= 0 then Nil else Cons n (mkList (n - 1))
{-# ANN sub3 "OPT:MayVectorize" #-}
sub3 :: List -> List
sub3 xs = case xs of
            Nil -> Nil
            Cons i rst -> Cons (i - 3) (sub3 rst)
sumList :: List -> Int64
sumList xs = case xs of
               Nil -> 0
               Cons i rst -> toInt64 i + sumList rst
gibbon_main = let xs = mkList 37
                  ys = sub3 xs
              in sumList ys
