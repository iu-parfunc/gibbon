data Tree = Leaf Int | Node Tree Tree
{-# ANN type Tree "Linear" #-}

mkTree :: Int -> Int -> Tree
mkTree d seed =
  if d <= 0
  then Leaf seed
  else Node (mkTree (d - 1) (seed + d)) (mkTree (d - 1) (seed + d + 1))

{-# ANN scaleLeaves "OPT:MayVectorize" #-}
scaleLeaves :: Int -> Tree -> Tree
scaleLeaves k tr =
  case tr of
    Leaf x -> Leaf (x * k + 1)
    Node l r -> Node (scaleLeaves k l) (scaleLeaves k r)

sumTree :: Tree -> Int
sumTree tr =
  case tr of
    Leaf x -> x
    Node l r -> sumTree l + sumTree r

gibbon_main =
  let tr = mkTree 5 1
      tr2 = scaleLeaves 2 tr
  in sumTree tr2
