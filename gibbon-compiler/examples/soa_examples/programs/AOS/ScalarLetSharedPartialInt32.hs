-- ScalarLetSharedPartialInt32: List (Linear).
-- Functions: mkList, xform, sumList.
-- Annotated: MayVectorize on xform; StoreScalarCounts on mkList.
data List = Cons Int32 Int32 List | Nil
{-# ANN type List "Linear" #-}

{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int64 -> List
mkList n = if n <= 0 then Nil else Cons (toInt32 n) (toInt32 (n + 100)) (mkList (n - 1))

{-# ANN xform "OPT:MayVectorize" #-}
xform :: List -> List
xform xs = case xs of
             Nil -> Nil
             Cons a b rst ->
               let q = mod a b
                in Cons (a + q) (if q == 0 then b else b + q) (xform rst)

sumList :: List -> Int64
sumList xs = case xs of
               Nil -> 0
               Cons a b rst -> toInt64 a + toInt64 b + sumList rst

gibbon_main = sumList (xform (mkList 20))
