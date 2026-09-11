-- W16 multiply: _mm_mullo_epi16 IS baseline SSE2 (unlike _mm_mullo_epi32),
-- so unlike Int32 this genuinely vectorizes.  Inputs are chosen so every
-- scalar product stays inside the signed Int16 range [-32768, 32767]:
-- max element is 21, times 3, is 63.
data List = Cons Int16 List | Nil
{-# ANN type List "Factored" #-}
{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int16 -> List
mkList n = if n <= 0 then Nil else Cons n (mkList (n - 1))
{-# ANN mul3 "OPT:MayVectorize" #-}
mul3 :: List -> List
mul3 xs = case xs of
            Nil -> Nil
            Cons i rst -> Cons (i * 3) (mul3 rst)
sumList :: List -> Int16
sumList xs = case xs of
               Nil -> 0
               Cons i rst -> i + sumList rst
gibbon_main = let xs = mkList 21
                  ys = mul3 xs
              in sumList ys
