-- SelectiveSharingTwoInputConsumerBug: List (Factored).
-- Functions: mkList, add1KeepFloat, zipSum.
-- Annotated: MayVectorize on add1KeepFloat; StoreScalarCounts on mkList.

data List = Cons Int Float List | Nil
{-# ANN type List "Factored" #-}

{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int -> List
mkList len =
  if len <= 0
  then Nil
  else let rst = mkList (len - 1)
       in Cons len 1.0 rst

{-# ANN add1KeepFloat "OPT:MayVectorize" #-}
add1KeepFloat :: List -> List
add1KeepFloat xs =
  case xs of
    Nil -> Nil
    Cons i f rst -> Cons (i + 1) f (add1KeepFloat rst)

zipSum :: List -> List -> Int
zipSum xs ys =
  case xs of
    Nil -> 0
    Cons i _ r ->
      case ys of
        Nil -> i
        Cons j _ r2 -> i + j + zipSum r r2

gibbon_main =
  let xs = mkList 100
      ys = add1KeepFloat xs
  in zipSum ys xs
