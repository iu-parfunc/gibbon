-- GuardDivElseInt64: List (Factored).
-- Functions: mkList, xform, sumList.
-- Annotated: MayVectorize on xform; StoreScalarCounts on mkList.
data List = Cons Int64 List | Nil
{-# ANN type List "Factored" #-}

{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int64 -> List
mkList n = if n <= 0 then Nil else Cons (toInt64 (n - 5)) (mkList (n - 1))

{-# ANN xform "OPT:MayVectorize" #-}
xform :: List -> List
xform xs = case xs of
             Nil -> Nil
             Cons d rst -> Cons (if d == 0 then 7 else 100 / d) (xform rst)

sumList :: List -> Int64
sumList xs = case xs of
               Nil -> 0
               Cons i rst -> toInt64 i + sumList rst

gibbon_main = sumList (xform (mkList 10))
