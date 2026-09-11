-- W32 packed multiply on baseline SSE2 (two _mm_mul_epu32 plus shuffles).
-- Values stay well inside Int32: max element 21, times 3, is 63, so no result
-- depends on the still-unspecified signed-overflow policy.
data List = Cons Int32 List | Nil
{-# ANN type List "Factored" #-}
{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int32 -> List
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
