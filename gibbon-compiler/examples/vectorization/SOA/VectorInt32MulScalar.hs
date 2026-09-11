-- CONTROL: W32 multiplication has no packed SSE2 implementation
-- (_mm_mullo_epi32 is SSE4.1), so this loop must stay entirely scalar.  If it
-- ever emits gib_vec_mul_int32x4, the milestone's honesty guarantee is broken.
data List = Cons Int32 List | Nil
{-# ANN type List "Factored" #-}
{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int32 -> List
mkList n = if n <= 0 then Nil else Cons n (mkList (n - 1))
{-# ANN mul3 "OPT:MayVectorize" #-}
mul3 :: List -> List
mul3 xs = case xs of
            Nil -> Nil
            Cons i rst -> Cons (i * 3) (mul3 rst)
sumList :: List -> Int32
sumList xs = case xs of
               Nil -> 0
               Cons i rst -> i + sumList rst
gibbon_main = let xs = mkList 13
                  ys = mul3 xs
              in sumList ys
