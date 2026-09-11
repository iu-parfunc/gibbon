-- All five signed comparisons at Int32, each driving a bitwise select.
-- Signed vs unsigned matters: negative operands must compare BELOW positive
-- ones.  Values are chosen so results stay well inside Int32's range.
data List = Cons Int32 List | Nil
{-# ANN type List "Factored" #-}
{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int32 -> List
mkList n = if n <= 0 then Nil else Cons (if mod n 3 == 0 then (0 - n) else n) (mkList (n - 1))

{-# ANN cmpLt "OPT:MayVectorize" #-}
cmpLt :: List -> List
cmpLt xs = case xs of
             Nil -> Nil
             Cons i rst -> Cons (if i < 0 then 1 else 2) (cmpLt rst)

{-# ANN cmpGt "OPT:MayVectorize" #-}
cmpGt :: List -> List
cmpGt xs = case xs of
             Nil -> Nil
             Cons i rst -> Cons (if i > 0 then 3 else 4) (cmpGt rst)

{-# ANN cmpLe "OPT:MayVectorize" #-}
cmpLe :: List -> List
cmpLe xs = case xs of
             Nil -> Nil
             Cons i rst -> Cons (if i <= 0 then 5 else 6) (cmpLe rst)

{-# ANN cmpGe "OPT:MayVectorize" #-}
cmpGe :: List -> List
cmpGe xs = case xs of
             Nil -> Nil
             Cons i rst -> Cons (if i >= 0 then 7 else 8) (cmpGe rst)

{-# ANN cmpEq "OPT:MayVectorize" #-}
cmpEq :: List -> List
cmpEq xs = case xs of
             Nil -> Nil
             Cons i rst -> Cons (if i == 1 then 9 else 10) (cmpEq rst)

sumList :: List -> Int64
sumList xs = case xs of
               Nil -> 0
               Cons i rst -> toInt64 i + sumList rst

gibbon_main =
  let xs = mkList 21
      a = sumList (cmpLt xs)
      b = sumList (cmpGt xs)
      c = sumList (cmpLe xs)
      d = sumList (cmpGe xs)
      e = sumList (cmpEq xs)
  in a + 100 * b + 10000 * c + 1000000 * d + 100000000 * e
