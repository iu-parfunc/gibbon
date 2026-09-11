-- W8 ordered comparisons over a list long enough to cross several packed-region
-- chunks.  The generator counts in Int64 and narrows, because an Int8 loop
-- counter could not exceed 127 elements.
data List = Cons Int8 List | Nil
{-# ANN type List "Factored" #-}
{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int64 -> List
mkList n = if n <= 0 then Nil else Cons (toInt8 (mod n 50 - 25)) (mkList (n - 1))
{-# ANN cmpLt "OPT:MayVectorize" #-}
cmpLt :: List -> List
cmpLt xs = case xs of
             Nil -> Nil
             Cons i rst -> Cons (if i < 0 then 1 else 2) (cmpLt rst)
{-# ANN cmpGe "OPT:MayVectorize" #-}
cmpGe :: List -> List
cmpGe xs = case xs of
             Nil -> Nil
             Cons i rst -> Cons (if i >= 0 then 3 else 4) (cmpGe rst)
sumList :: List -> Int64
sumList xs = case xs of
               Nil -> 0
               Cons i rst -> toInt64 i + sumList rst
gibbon_main = let xs = mkList 300
                  a = sumList (cmpLt xs)
                  b = sumList (cmpGe xs)
              in a + 1000000 * b
