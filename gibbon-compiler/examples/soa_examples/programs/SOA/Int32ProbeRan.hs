-- Int32 audit probe: forces random-access-node (RAN) insertion by pattern
-- matching a constructor and only using a field that lives after an untraversed
-- packed field.  Compile WITHOUT --no-ran to exercise AddRAN.
data Tree = Leaf Int | Node Int Tree Tree
{-# ANN type Tree "Linear" #-}

mkTree :: Int -> Tree
mkTree d =
  if d <= 0
  then Leaf d
  else Node d (mkTree (d - 1)) (mkTree (d - 1))

rightmost :: Tree -> Int
rightmost t =
  case t of
    Leaf i -> i
    Node i l r -> rightmost r

sumTree :: Tree -> Int
sumTree t =
  case t of
    Leaf i -> i
    Node i l r -> i + (sumTree l + sumTree r)

gibbon_main =
  let t = mkTree 5
      a = rightmost t
      b = sumTree t
  in (a, b)
