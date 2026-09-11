-- Case C: a LET-BOUND condition and a dependent temporary SHARED by both arms.
-- `y` is referenced three times, so this is the sharing case: inlining it would
-- evaluate `x + 1` three times.
data List = Cons Bool Int32 List | Nil
{-# ANN type List "Factored" #-}

{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int64 -> List
mkList n = if n <= 0 then Nil else Cons (n == 3) (toInt32 (n - 5)) (mkList (n - 1))

{-# ANN xform "OPT:MayVectorize" #-}
xform :: List -> List
xform xs = case xs of
             Nil -> Nil
             Cons p x rst -> Cons p (let y = x + 1 in let q = y < 4 in if q then y * 2 else y - 3) (xform rst)

sumList :: List -> Int64
sumList xs = case xs of
               Nil -> 0
               Cons p i rst -> toInt64 i + sumList rst

gibbon_main = sumList (xform (mkList 10))
