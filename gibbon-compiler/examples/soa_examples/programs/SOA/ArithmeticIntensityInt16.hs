-- ArithmeticIntensityInt16: Tree (Factored).
-- Functions: mkTree, arithKernel, checksumTree.
-- Annotated: MayVectorize on arithKernel; StoreScalarCounts on mkTree.
data Tree = Leaf Int16
          | Node Tree Tree

{-# ANN type Tree "Factored" #-}

{-# ANN mkTree "OPT:StoreScalarCounts" #-}
mkTree :: Int -> Int -> Tree
mkTree d seed =
  if d <= 0
  then Leaf (toInt16 (mod seed 64 - 32))
  else Node (mkTree (d - 1) (seed * 2 + 1)) (mkTree (d - 2) (seed * 2 + 3))

{-# ANN arithKernel "OPT:MayVectorize" #-}
arithKernel :: Tree -> Tree
arithKernel t =
  case t of
    Leaf x ->
      let
          a0_1 = x + 29
          b0_1 = x + 31
          a1_1 = x + 37
          b1_1 = x + 41
          a2_1 = x + 43
          b2_1 = x + 47
          a3_1 = x + 53
          b3_1 = x + 59
          a4_1 = x + 29
          b4_1 = x + 31
          a5_1 = x + 37
          b5_1 = x + 41
          a6_1 = x + 43
          b6_1 = x + 47
          a7_1 = x + 53
          b7_1 = x + 59
          ta0_1 = b0_1 + b0_1 + 1
          tb0_1 = a0_1 + a0_1 + 1
          a0_2 = a0_1 * ta0_1 + 29
          b0_2 = b0_1 * tb0_1 + 41
          ta1_1 = b1_1 + b1_1 + 1
          tb1_1 = a1_1 + a1_1 + 1
          a1_2 = a1_1 * ta1_1 + 31
          b1_2 = b1_1 * tb1_1 + 43
          ta2_1 = b2_1 + b2_1 + 1
          tb2_1 = a2_1 + a2_1 + 1
          a2_2 = a2_1 * ta2_1 + 37
          b2_2 = b2_1 * tb2_1 + 47
          ta3_1 = b3_1 + b3_1 + 1
          tb3_1 = a3_1 + a3_1 + 1
          a3_2 = a3_1 * ta3_1 + 41
          b3_2 = b3_1 * tb3_1 + 53
          ta4_1 = b4_1 + b4_1 + 1
          tb4_1 = a4_1 + a4_1 + 1
          a4_2 = a4_1 * ta4_1 + 43
          b4_2 = b4_1 * tb4_1 + 59
          ta5_1 = b5_1 + b5_1 + 1
          tb5_1 = a5_1 + a5_1 + 1
          a5_2 = a5_1 * ta5_1 + 47
          b5_2 = b5_1 * tb5_1 + 29
          ta6_1 = b6_1 + b6_1 + 1
          tb6_1 = a6_1 + a6_1 + 1
          a6_2 = a6_1 * ta6_1 + 53
          b6_2 = b6_1 * tb6_1 + 31
          ta7_1 = b7_1 + b7_1 + 1
          tb7_1 = a7_1 + a7_1 + 1
          a7_2 = a7_1 * ta7_1 + 59
          b7_2 = b7_1 * tb7_1 + 37
          ta0_2 = b0_2 + b0_2 + 1
          tb0_2 = a0_2 + a0_2 + 1
          a0_3 = a0_2 * ta0_2 + 31
          b0_3 = b0_2 * tb0_2 + 43
          ta1_2 = b1_2 + b1_2 + 1
          tb1_2 = a1_2 + a1_2 + 1
          a1_3 = a1_2 * ta1_2 + 37
          b1_3 = b1_2 * tb1_2 + 47
          ta2_2 = b2_2 + b2_2 + 1
          tb2_2 = a2_2 + a2_2 + 1
          a2_3 = a2_2 * ta2_2 + 41
          b2_3 = b2_2 * tb2_2 + 53
          ta3_2 = b3_2 + b3_2 + 1
          tb3_2 = a3_2 + a3_2 + 1
          a3_3 = a3_2 * ta3_2 + 43
          b3_3 = b3_2 * tb3_2 + 59
          ta4_2 = b4_2 + b4_2 + 1
          tb4_2 = a4_2 + a4_2 + 1
          a4_3 = a4_2 * ta4_2 + 47
          b4_3 = b4_2 * tb4_2 + 29
          ta5_2 = b5_2 + b5_2 + 1
          tb5_2 = a5_2 + a5_2 + 1
          a5_3 = a5_2 * ta5_2 + 53
          b5_3 = b5_2 * tb5_2 + 31
          ta6_2 = b6_2 + b6_2 + 1
          tb6_2 = a6_2 + a6_2 + 1
          a6_3 = a6_2 * ta6_2 + 59
          b6_3 = b6_2 * tb6_2 + 37
          ta7_2 = b7_2 + b7_2 + 1
          tb7_2 = a7_2 + a7_2 + 1
          a7_3 = a7_2 * ta7_2 + 29
          b7_3 = b7_2 * tb7_2 + 41
          ta0_3 = b0_3 + b0_3 + 1
          tb0_3 = a0_3 + a0_3 + 1
          a0_4 = a0_3 * ta0_3 + 37
          b0_4 = b0_3 * tb0_3 + 47
          ta1_3 = b1_3 + b1_3 + 1
          tb1_3 = a1_3 + a1_3 + 1
          a1_4 = a1_3 * ta1_3 + 41
          b1_4 = b1_3 * tb1_3 + 53
          ta2_3 = b2_3 + b2_3 + 1
          tb2_3 = a2_3 + a2_3 + 1
          a2_4 = a2_3 * ta2_3 + 43
          b2_4 = b2_3 * tb2_3 + 59
          ta3_3 = b3_3 + b3_3 + 1
          tb3_3 = a3_3 + a3_3 + 1
          a3_4 = a3_3 * ta3_3 + 47
          b3_4 = b3_3 * tb3_3 + 29
          ta4_3 = b4_3 + b4_3 + 1
          tb4_3 = a4_3 + a4_3 + 1
          a4_4 = a4_3 * ta4_3 + 53
          b4_4 = b4_3 * tb4_3 + 31
          ta5_3 = b5_3 + b5_3 + 1
          tb5_3 = a5_3 + a5_3 + 1
          a5_4 = a5_3 * ta5_3 + 59
          b5_4 = b5_3 * tb5_3 + 37
          ta6_3 = b6_3 + b6_3 + 1
          tb6_3 = a6_3 + a6_3 + 1
          a6_4 = a6_3 * ta6_3 + 29
          b6_4 = b6_3 * tb6_3 + 41
          ta7_3 = b7_3 + b7_3 + 1
          tb7_3 = a7_3 + a7_3 + 1
          a7_4 = a7_3 * ta7_3 + 31
          b7_4 = b7_3 * tb7_3 + 43
          ta0_4 = b0_4 + b0_4 + 1
          tb0_4 = a0_4 + a0_4 + 1
          a0_5 = a0_4 * ta0_4 + 41
          b0_5 = b0_4 * tb0_4 + 53
          ta1_4 = b1_4 + b1_4 + 1
          tb1_4 = a1_4 + a1_4 + 1
          a1_5 = a1_4 * ta1_4 + 43
          b1_5 = b1_4 * tb1_4 + 59
          ta2_4 = b2_4 + b2_4 + 1
          tb2_4 = a2_4 + a2_4 + 1
          a2_5 = a2_4 * ta2_4 + 47
          b2_5 = b2_4 * tb2_4 + 29
          ta3_4 = b3_4 + b3_4 + 1
          tb3_4 = a3_4 + a3_4 + 1
          a3_5 = a3_4 * ta3_4 + 53
          b3_5 = b3_4 * tb3_4 + 31
          ta4_4 = b4_4 + b4_4 + 1
          tb4_4 = a4_4 + a4_4 + 1
          a4_5 = a4_4 * ta4_4 + 59
          b4_5 = b4_4 * tb4_4 + 37
          ta5_4 = b5_4 + b5_4 + 1
          tb5_4 = a5_4 + a5_4 + 1
          a5_5 = a5_4 * ta5_4 + 29
          b5_5 = b5_4 * tb5_4 + 41
          ta6_4 = b6_4 + b6_4 + 1
          tb6_4 = a6_4 + a6_4 + 1
          a6_5 = a6_4 * ta6_4 + 31
          b6_5 = b6_4 * tb6_4 + 43
          ta7_4 = b7_4 + b7_4 + 1
          tb7_4 = a7_4 + a7_4 + 1
          a7_5 = a7_4 * ta7_4 + 37
          b7_5 = b7_4 * tb7_4 + 47
          ta0_5 = b0_5 + b0_5 + 1
          tb0_5 = a0_5 + a0_5 + 1
          a0_6 = a0_5 * ta0_5 + 43
          b0_6 = b0_5 * tb0_5 + 59
          ta1_5 = b1_5 + b1_5 + 1
          tb1_5 = a1_5 + a1_5 + 1
          a1_6 = a1_5 * ta1_5 + 47
          b1_6 = b1_5 * tb1_5 + 29
          ta2_5 = b2_5 + b2_5 + 1
          tb2_5 = a2_5 + a2_5 + 1
          a2_6 = a2_5 * ta2_5 + 53
          b2_6 = b2_5 * tb2_5 + 31
          ta3_5 = b3_5 + b3_5 + 1
          tb3_5 = a3_5 + a3_5 + 1
          a3_6 = a3_5 * ta3_5 + 59
          b3_6 = b3_5 * tb3_5 + 37
          ta4_5 = b4_5 + b4_5 + 1
          tb4_5 = a4_5 + a4_5 + 1
          a4_6 = a4_5 * ta4_5 + 29
          b4_6 = b4_5 * tb4_5 + 41
          ta5_5 = b5_5 + b5_5 + 1
          tb5_5 = a5_5 + a5_5 + 1
          a5_6 = a5_5 * ta5_5 + 31
          b5_6 = b5_5 * tb5_5 + 43
          ta6_5 = b6_5 + b6_5 + 1
          tb6_5 = a6_5 + a6_5 + 1
          a6_6 = a6_5 * ta6_5 + 37
          b6_6 = b6_5 * tb6_5 + 47
          ta7_5 = b7_5 + b7_5 + 1
          tb7_5 = a7_5 + a7_5 + 1
          a7_6 = a7_5 * ta7_5 + 41
          b7_6 = b7_5 * tb7_5 + 53
          ta0_6 = b0_6 + b0_6 + 1
          tb0_6 = a0_6 + a0_6 + 1
          a0_7 = a0_6 * ta0_6 + 47
          b0_7 = b0_6 * tb0_6 + 29
          ta1_6 = b1_6 + b1_6 + 1
          tb1_6 = a1_6 + a1_6 + 1
          a1_7 = a1_6 * ta1_6 + 53
          b1_7 = b1_6 * tb1_6 + 31
          ta2_6 = b2_6 + b2_6 + 1
          tb2_6 = a2_6 + a2_6 + 1
          a2_7 = a2_6 * ta2_6 + 59
          b2_7 = b2_6 * tb2_6 + 37
          ta3_6 = b3_6 + b3_6 + 1
          tb3_6 = a3_6 + a3_6 + 1
          a3_7 = a3_6 * ta3_6 + 29
          b3_7 = b3_6 * tb3_6 + 41
          ta4_6 = b4_6 + b4_6 + 1
          tb4_6 = a4_6 + a4_6 + 1
          a4_7 = a4_6 * ta4_6 + 31
          b4_7 = b4_6 * tb4_6 + 43
          ta5_6 = b5_6 + b5_6 + 1
          tb5_6 = a5_6 + a5_6 + 1
          a5_7 = a5_6 * ta5_6 + 37
          b5_7 = b5_6 * tb5_6 + 47
          ta6_6 = b6_6 + b6_6 + 1
          tb6_6 = a6_6 + a6_6 + 1
          a6_7 = a6_6 * ta6_6 + 41
          b6_7 = b6_6 * tb6_6 + 53
          ta7_6 = b7_6 + b7_6 + 1
          tb7_6 = a7_6 + a7_6 + 1
          a7_7 = a7_6 * ta7_6 + 43
          b7_7 = b7_6 * tb7_6 + 59
      in Leaf (a0_7 + b0_7 + a1_7 + b1_7 + a2_7 + b2_7 + a3_7 + b3_7 + a4_7 + b4_7 + a5_7 + b5_7 + a6_7 + b6_7 + a7_7 + b7_7)
    Node l r -> Node (arithKernel l) (arithKernel r)

checksumTree :: Tree -> Int16 -> Int16
checksumTree t h =
  case t of
    Leaf x -> h * 31 + x
    Node l r ->
      let hl = checksumTree l h
      in checksumTree r hl

gibbon_main =
  let _ = printsym (quote "Running program ArithmeticIntensity Int16: ")
      _ = printsym (quote "NEWLINE")
      tree = mkTree 35 1

      _ = printsym (quote "Running pass arithKernel (map, uses=3, shared=0): ")
      _ = printsym (quote "NEWLINE")
      tree' = iterate (arithKernel tree)
      _ = printsym (quote "End")
      _ = printsym (quote "NEWLINE")
      _ = printsym (quote "Running pass checksumTree (fold, uses=3): ")
      _ = printsym (quote "NEWLINE")
      cksum = iterate (checksumTree tree' (toInt16 0))
      _ = printsym (quote "End")
      _ = printsym (quote "NEWLINE")
  in cksum
