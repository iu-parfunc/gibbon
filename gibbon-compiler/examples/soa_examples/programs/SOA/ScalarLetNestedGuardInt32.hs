-- Case B: NESTED guards written naturally.  The inner condition becomes a LetE
-- inside the outer else arm.
data List = Cons Bool Int32 List | Nil
{-# ANN type List "Factored" #-}

{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int64 -> List
mkList n = if n <= 0 then Nil else Cons (n == 3) (toInt32 (n - 5)) (mkList (n - 1))

{-# ANN xform "OPT:MayVectorize" #-}
xform :: List -> List
xform xs = case xs of
             Nil -> Nil
             Cons p x rst -> Cons p (if p then x + 1 else (if x == 0 then 7 else x - 3)) (xform rst)

sumList :: List -> Int64
sumList xs = case xs of
               Nil -> 0
               Cons p i rst -> toInt64 i + sumList rst

gibbon_main = sumList (xform (mkList 10))
