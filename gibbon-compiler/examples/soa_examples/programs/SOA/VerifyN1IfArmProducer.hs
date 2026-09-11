-- VerifyN1IfArmProducer: List (Factored).
-- Functions: mkList, mul2, add1, sumList.
-- Annotated: MayVectorize on add1; StoreScalarCounts on mkList.
data List = Cons Int List | Nil
{-# ANN type List "Factored" #-}

{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int -> List
mkList n = if n <= 0
           then Nil
           else let rst = mkList (n - 1)
                 in Cons n rst

mul2 :: List -> List
mul2 lst = case lst of
             Nil -> Nil
             Cons i rst -> let v = if i == 0
                                   then 0
                                   else (if i == 1 then 1 else i * 2)
                            in Cons v (mul2 rst)

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
  let l1 = mkList 100
      n  = sumList l1
      h  = if n == 0 then mul2 l1 else mul2 l1
      m  = add1 h
   in sumList m
