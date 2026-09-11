-- W8 packed multiply on baseline SSE2: unpack to 16-bit lanes,
-- _mm_mullo_epi16, mask to the low 8 bits, repack.  Max |value| is 21 x 3
-- = 63, well inside Int8, so no result depends on signed-overflow policy.
data List = Cons Int8 List | Nil
{-# ANN type List "Factored" #-}
{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int8 -> List
mkList n = if n <= 0 then Nil else Cons (if mod n 4 == 0 then (0 - n) else n) (mkList (n - 1))
{-# ANN mul3 "OPT:MayVectorize" #-}
mul3 :: List -> List
mul3 xs = case xs of
            Nil -> Nil
            Cons i rst -> Cons (i * 3) (mul3 rst)
sumList :: List -> Int64
sumList xs = case xs of
               Nil -> 0
               Cons i rst -> toInt64 i + sumList rst
gibbon_main = let xs = mkList 21
                  ys = mul3 xs
              in sumList ys
