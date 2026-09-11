-- LoopifyShrinkingProducerRepro: List (Factored).
-- Functions: mkList, dropMost, add1, sumList.
-- Annotated: MayVectorize on add1; StoreScalarCounts on mkList.
data List = Cons Int List | Nil
{-# ANN type List "Factored" #-}

{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int -> List
mkList n = if n <= 0
           then Nil
           else let rst = mkList (n - 1)
                 in Cons n rst

dropMost :: List -> Int -> List
dropMost lst k = case lst of
                   Nil -> Nil
                   Cons i rst -> if k == 0
                                 then Cons i (dropMost rst 49)
                                 else dropMost rst (k - 1)

{-# ANN add1 "OPT:MayVectorize" #-}
add1 :: List -> List
add1 lst = case lst of
             Nil -> Nil
             Cons i rst -> let i1 = i + 1
                            in Cons i1 (add1 rst)

sumList :: List -> Int
sumList lst = case lst of
                Nil -> 0
                Cons i rst -> let s = sumList rst
                               in i + s

gibbon_main =
  let l1 = mkList 300
      h  = dropMost l1 0
      m  = add1 h
   in sumList m
