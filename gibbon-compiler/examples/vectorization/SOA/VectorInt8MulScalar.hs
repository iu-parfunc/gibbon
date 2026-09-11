-- CONTROL: SSE2 has no packed 8-bit multiply at all, so this stays scalar.
data List = Cons Int8 List | Nil
{-# ANN type List "Factored" #-}
{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int8 -> List
mkList n = if n <= 0 then Nil else Cons n (mkList (n - 1))
{-# ANN mul3 "OPT:MayVectorize" #-}
mul3 :: List -> List
mul3 xs = case xs of
            Nil -> Nil
            Cons i rst -> Cons (i * 2) (mul3 rst)
sumList :: List -> Int64
sumList xs = case xs of
               Nil -> 0
               Cons i rst -> toInt64 i + sumList rst
gibbon_main = let xs = mkList 37
                  ys = mul3 xs
              in sumList ys
