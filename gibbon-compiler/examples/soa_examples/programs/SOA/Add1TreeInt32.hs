-- Add1TreeInt32: Tree (Factored).
-- Functions: mkTree, add1Tree, checksumTree.
-- Annotated: MayVectorize on add1Tree; StoreScalarCounts on mkTree.
data Tree = Leaf Int32
          | Node Tree Tree

{-# ANN type Tree "Factored" #-}

{-# ANN mkTree "OPT:StoreScalarCounts" #-}
mkTree :: Int -> Int -> Tree
mkTree d seed =
  if d <= 0
  then Leaf (toInt32 (mod seed 64 - 32))
  else Node (mkTree (d - 1) (seed * 2 + 1)) (mkTree (d - 2) (seed * 2 + 3))

{-# ANN add1Tree "OPT:MayVectorize" #-}
add1Tree :: Tree -> Tree
add1Tree t =
  case t of
    Leaf x -> Leaf (x + 1)
    Node l r -> Node (add1Tree l) (add1Tree r)

checksumTree :: Tree -> Int32 -> Int32
checksumTree t h =
  case t of
    Leaf x -> h * 31 + x
    Node l r ->
      let hl = checksumTree l h
      in checksumTree r hl

gibbon_main =
  let _ = printsym (quote "Running program Add1Tree Int32: ")
      _ = printsym (quote "NEWLINE")
      tree = mkTree 33 1

      _ = printsym (quote "Running pass add1Tree (map, uses=3, shared=0): ")
      _ = printsym (quote "NEWLINE")
      tree' = iterate (add1Tree tree)
      _ = printsym (quote "End")
      _ = printsym (quote "NEWLINE")
      _ = printsym (quote "Running pass checksumTree (fold, uses=3): ")
      _ = printsym (quote "NEWLINE")
      cksum = iterate (checksumTree tree' (toInt32 0))
      _ = printsym (quote "End")
      _ = printsym (quote "NEWLINE")
  in cksum
