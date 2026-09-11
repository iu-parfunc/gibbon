-- GuardNestedBoolInt32: List (Factored).
-- Functions: mkList, xform, sumList.
-- Annotated: MayVectorize on xform; StoreScalarCounts on mkList.
data List = Cons Bool Bool Int32 List | Nil
{-# ANN type List "Factored" #-}

{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int64 -> List
mkList n =
  if n <= 0
  then Nil
  else Cons (n == 5) (n == 4) (toInt32 (n - 5)) (mkList (n - 1))

{-# ANN xform "OPT:MayVectorize" #-}
xform :: List -> List
xform xs = case xs of
             Nil -> Nil
             Cons b1 b2 d rst ->
               Cons b1 b2 (if b1 then 7 else (if b2 then 100 / d else 60 / d)) (xform rst)

sumList :: List -> Int64
sumList xs = case xs of
               Nil -> 0
               Cons b1 b2 i rst -> toInt64 i + sumList rst

gibbon_main = sumList (xform (mkList 10))
