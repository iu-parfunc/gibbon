-- CONTROL: SSE2 has no packed signed 32-bit divide or modulus, so both of
-- these must stay entirely scalar.
data List = Cons Int32 List | Nil
{-# ANN type List "Factored" #-}
{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int32 -> List
mkList n = if n <= 0 then Nil else Cons n (mkList (n - 1))
{-# ANN divmod3 "OPT:MayVectorize" #-}
divmod3 :: List -> List
divmod3 xs = case xs of
               Nil -> Nil
               Cons i rst -> Cons ((i / 3) + (mod i 3)) (divmod3 rst)
sumList :: List -> Int32
sumList xs = case xs of
               Nil -> 0
               Cons i rst -> i + sumList rst
gibbon_main = let xs = mkList 13
                  ys = divmod3 xs
              in sumList ys
