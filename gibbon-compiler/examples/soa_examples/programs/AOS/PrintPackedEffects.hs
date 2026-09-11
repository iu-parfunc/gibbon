-- PrintPackedEffects.
-- Functions: mkTree, sumTree, showThenSum.
data Tree = Leaf Int64 | Node Tree Tree

mkTree :: Int64 -> Tree
mkTree n = if n <= 0 then Leaf 7 else Node (mkTree (n-1)) (mkTree (n-1))

sumTree :: Tree -> Int64
sumTree t =
  case t of
    Leaf i -> i
    Node a b -> sumTree a + sumTree b

-- A discarded printPacked inside an ordinary (non-main) helper.
showThenSum :: Tree -> Int64
showThenSum t =
  let _ = printPacked t
      s = sumTree t
  in s

gibbon_main =
  let t = mkTree 1
      -- discarded binder
      _  = printPacked t
      -- a different effect in between, to pin ordering
      _u = printint 111
      -- named-but-unused binder
      unused = printPacked t
      s = showThenSum t
  in s
