-- Genuine eight-lane Int16 SIMD over a fully factored (SoA) list.
-- Int16 is 2 bytes, so one 128-bit SSE2 register holds EIGHT elements: one
-- 16-byte load/store moves eight of them, versus four for Int32 and two for
-- Int64.  The element count is not a multiple of eight, so the scalar tail runs.
data List = Cons Int16 List | Nil
{-# ANN type List "Factored" #-}
{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int16 -> List
mkList n = if n <= 0 then Nil else Cons n (mkList (n - 1))
{-# ANN add3 "OPT:MayVectorize" #-}
add3 :: List -> List
add3 xs = case xs of
            Nil -> Nil
            Cons i rst -> Cons (i + 3) (add3 rst)
sumList :: List -> Int16
sumList xs = case xs of
               Nil -> 0
               Cons i rst -> i + sumList rst
gibbon_main = let xs = mkList 21
                  ys = add3 xs
              in sumList ys
