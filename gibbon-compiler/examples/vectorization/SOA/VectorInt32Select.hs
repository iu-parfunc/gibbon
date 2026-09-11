-- W32 equality mask + select: out = if x == sentinel then a else b.
-- Exercises _mm_cmpeq_epi32 and the bitwise mask select.
data List = Cons Int32 List | Nil
{-# ANN type List "Factored" #-}
{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int32 -> List
mkList n = if n <= 0 then Nil else Cons n (mkList (n - 1))
{-# ANN pick "OPT:MayVectorize" #-}
pick :: List -> List
pick xs = case xs of
            Nil -> Nil
            Cons i rst -> Cons (if i == 7 then 100 else 1) (pick rst)
sumList :: List -> Int32
sumList xs = case xs of
               Nil -> 0
               Cons i rst -> i + sumList rst
gibbon_main = let xs = mkList 13
                  ys = pick xs
              in sumList ys
