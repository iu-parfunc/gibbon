-- W32 subtract: out = x - invariant.  Four lanes per 128-bit register.
data List = Cons Int32 List | Nil
{-# ANN type List "Factored" #-}
{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int32 -> List
mkList n = if n <= 0 then Nil else Cons n (mkList (n - 1))
{-# ANN sub3 "OPT:MayVectorize" #-}
sub3 :: List -> List
sub3 xs = case xs of
            Nil -> Nil
            Cons i rst -> Cons (i - 3) (sub3 rst)
sumList :: List -> Int32
sumList xs = case xs of
               Nil -> 0
               Cons i rst -> i + sumList rst
gibbon_main = let xs = mkList 13
                  ys = sub3 xs
              in sumList ys
