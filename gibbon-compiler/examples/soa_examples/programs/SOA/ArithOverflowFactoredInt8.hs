-- ArithOverflowFactoredInt8: List (Factored).
-- Functions: mkList, bump, sumList.
-- Annotated: MayVectorize on bump; StoreScalarCounts on mkList.

data List = Cons Int8 List | Nil
{-# ANN type List "Factored" #-}

{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int64 -> List
mkList n = if n <= 0 then Nil else Cons (toInt8 (n * 7)) (mkList (n - 1))

{-# ANN bump "OPT:MayVectorize" #-}
bump :: List -> List
bump xs = case xs of
            Nil -> Nil
            Cons i rst -> Cons (i * 3 + 100) (bump rst)

sumList :: List -> Int64
sumList xs = case xs of
               Nil -> 0
               Cons i rst -> toInt64 i + sumList rst

gibbon_main = sumList (bump (mkList 37))
