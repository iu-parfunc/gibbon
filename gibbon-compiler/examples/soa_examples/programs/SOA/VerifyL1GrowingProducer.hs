-- VerifyL1GrowingProducer: List (Factored).
-- Functions: mkList, dupAll, add1, sumList.
-- Annotated: MayVectorize on add1; StoreScalarCounts on mkList.
data List = Cons Int List | Nil
{-# ANN type List "Factored" #-}

{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int -> List
mkList n = if n <= 0
           then Nil
           else let rst = mkList (n - 1)
                 in Cons n rst

-- Emits TWO output elements per input element.
dupAll :: List -> List
dupAll lst = case lst of
               Nil -> Nil
               Cons i rst -> let r = dupAll rst
                              in Cons i (Cons i r)

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
  let l1 = mkList 50
      d  = dupAll l1
      m  = add1 d
   in sumList m
