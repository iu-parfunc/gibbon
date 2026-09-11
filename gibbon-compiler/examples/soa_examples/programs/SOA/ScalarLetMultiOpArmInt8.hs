-- Case A: a MULTI-OPERATION arm.  `(x * 3) + 2` is flattened into a LetE inside
-- the else arm, which the loopifier's scalar-expression whitelist rejects.
data List = Cons Bool Int8 List | Nil
{-# ANN type List "Factored" #-}

{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int64 -> List
mkList n = if n <= 0 then Nil else Cons (n == 3) (toInt8 (n - 5)) (mkList (n - 1))

{-# ANN xform "OPT:MayVectorize" #-}
xform :: List -> List
xform xs = case xs of
             Nil -> Nil
             Cons p x rst -> Cons p (if p then x + 1 else (x * 3) + 2) (xform rst)

sumList :: List -> Int64
sumList xs = case xs of
               Nil -> 0
               Cons p i rst -> toInt64 i + sumList rst

gibbon_main = sumList (xform (mkList 10))
