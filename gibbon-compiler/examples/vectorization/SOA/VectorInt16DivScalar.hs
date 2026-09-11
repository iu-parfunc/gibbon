-- CONTROL: SSE2 has no packed signed 16-bit divide or modulus, so this loop
-- must stay entirely scalar.
data List = Cons Int16 List | Nil
{-# ANN type List "Factored" #-}
{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int16 -> List
mkList n = if n <= 0 then Nil else Cons n (mkList (n - 1))
{-# ANN divmod3 "OPT:MayVectorize" #-}
divmod3 :: List -> List
divmod3 xs = case xs of
               Nil -> Nil
               Cons i rst -> Cons ((i / 3) + (mod i 3)) (divmod3 rst)
sumList :: List -> Int16
sumList xs = case xs of
               Nil -> 0
               Cons i rst -> i + sumList rst
gibbon_main = let xs = mkList 21
                  ys = divmod3 xs
              in sumList ys
