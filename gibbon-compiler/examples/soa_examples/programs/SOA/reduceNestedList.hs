-- @BENCH adt_fields=3
-- @BENCH adt_type=List
data ListA = ConsA Int ListA | NilA
data List = Cons Int ListA List | Nil

{-# ANN type ListA "Linear" #-}
{-# ANN type List "Factored" #-}

{-# ANN mkListA "OPT:StoreScalarCounts" #-}
mkListA :: Int -> ListA
mkListA len =
  if len <= 0
    then NilA
    else
      let rst = mkListA (len - 1)
      in ConsA len rst

{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int -> List
mkList len =
  if len <= 0
    then Nil
    else
      let lsta = mkListA 3000
          rst = mkList (len - 1)
      in Cons len lsta rst

reduce :: List -> Int
reduce lst =
  case lst of
    Nil -> 0
    Cons a lsta rst -> a + reduce rst

gibbon_main =
  let _ = printsym (quote "Running program reduceNestedList: ")
      _ = printsym (quote "NEWLINE")
      lst = mkList 1000000
      _ = printsym (quote "Running pass reduction (fold, uses=2): ")
      _ = printsym (quote "NEWLINE")
      total = iterate (reduce lst)
      _ = printsym (quote "End")
      _ = printsym (quote "NEWLINE")
  in total
