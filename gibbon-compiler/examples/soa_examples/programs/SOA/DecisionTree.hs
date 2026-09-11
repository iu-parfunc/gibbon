-- DecisionTree: DTree (Factored).
-- Functions: buildTree, countNodes, countLeaves, treeDepth, sumImpurity,
-- sumSamples, countFeatureUses, countClass. ...
{-# LANGUAGE BangPatterns #-}

module DTreeFoldBench where


-- ===============================
-- Decision Tree Benchmark (DTree)
-- ===============================

data DTree
  = Leaf
      Int64        -- class label
      Int64        -- sample count
  | Node
      Int64        -- feature id
      Int64        -- threshold
      Int64        -- impurity (e.g. Gini * 1000)
      DTree
      DTree


{-# ANN type DTree "Factored" #-}

-- -------------------------------
-- Tree generator (benchmark input)
-- -------------------------------

buildTree :: Int -> DTree
buildTree d =
  if d <= 0
  then Leaf (mod d 3) (1 + (mod d 10))
  else
    let feature   = mod d 16
        threshold = mod d 100
        impurity  = 1000 - d
    in Node feature threshold impurity (buildTree (d - 1)) (buildTree (d - 2))

-- -------------------------------
-- Structural reductions
-- -------------------------------

countNodes :: DTree -> Int
countNodes t =
  case t of
    Leaf _ _ ->
      1
    Node _ _ _ l r ->
      1 + countNodes l + countNodes r

countLeaves :: DTree -> Int
countLeaves t =
  case t of
    Leaf _ _ ->
      1
    Node _ _ _ l r ->
      countLeaves l + countLeaves r

treeDepth :: DTree -> Int
treeDepth t =
  case t of
    Leaf _ _ ->
      1
    Node _ _ _ l r ->
      let dl = treeDepth l in
      let dr = treeDepth r in
      1 + max dl dr

-- -------------------------------
-- ML-style analysis reductions
-- -------------------------------

sumImpurity :: DTree -> Int
sumImpurity t =
  case t of
    Leaf _ _ ->
      0
    Node _ _ imp l r ->
      imp + sumImpurity l + sumImpurity r

sumSamples :: DTree -> Int
sumSamples t =
  case t of
    Leaf _ samples ->
      samples
    Node _ _ _ l r ->
      sumSamples l + sumSamples r

-- `fid` is Int64, matching the field it is compared against.
countFeatureUses :: Int -> DTree -> Int
countFeatureUses fid t =
  case t of
    Leaf _ _ ->
      0
    Node f _ _ l r ->
      let here = if f == fid then 1 else 0 in
      here + countFeatureUses fid l
           + countFeatureUses fid r

countClass :: Int -> DTree -> Int
countClass cls t =
  case t of
    Leaf label _ ->
      if label == cls then 1 else 0
    Node _ _ _ l r ->
      countClass cls l + countClass cls r

countSmallLeaves :: Int -> DTree -> Int
countSmallLeaves thresh t =
  case t of
    Leaf _ samples ->
      if samples < thresh then 1 else 0
    Node _ _ _ l r ->
      countSmallLeaves thresh l
      + countSmallLeaves thresh r

-- -------------------------------
-- Inference-related reductions
-- -------------------------------

max :: Int -> Int -> Int
max a b = if a > b
          then a
          else b

inferenceCost :: DTree -> Int
inferenceCost t =
  case t of
    Leaf _ _ ->
      0
    Node _ _ _ l r ->
      1 + max (inferenceCost l) (inferenceCost r)

sumPathLengths :: Int -> DTree -> Int
sumPathLengths depth t =
  case t of
    Leaf _ samples ->
      depth * samples
    Node _ _ _ l r ->
      let dl = sumPathLengths (depth + 1) l in
      let dr = sumPathLengths (depth + 1) r in
      dl + dr


-- -------------------------------
-- Benchmark entry point
-- -------------------------------

gibbon_main =

  let _ = printsym (quote "Running program Decision Tree: ") in
  let _ = printsym (quote "NEWLINE") in
  -- Depth 32: the nine folds below are O(nodes) and, at the
  -- classification file's depth 14, each ran 12-23 MICROseconds --
  -- unmeasurable. See this file's header for the sizing.
  let tree = buildTree (sizeParam + 32) in

  -- Structural analyses
  let _ = printsym (quote "Running pass countNodes (fold, uses=2): ") in
  let _ = printsym (quote "NEWLINE") in
  let nodes   = iterate (countNodes tree) in
  let _ = printsym (quote "End") in
  let _ = printsym (quote "NEWLINE") in
  let _ = printsym (quote "Running pass countLeaves (fold, uses=2): ") in
  let _ = printsym (quote "NEWLINE") in
  let leaves  = iterate (countLeaves tree) in
  let _ = printsym (quote "End") in
  let _ = printsym (quote "NEWLINE") in
  let _ = printsym (quote "Running pass treeDepth (fold, uses=2): ") in
  let _ = printsym (quote "NEWLINE") in
  let depth   = iterate (treeDepth tree) in
  let _ = printsym (quote "End") in
  let _ = printsym (quote "NEWLINE") in
  let _ = printsym (quote "Running pass sumImpurity (fold, uses=3): ") in
  let _ = printsym (quote "NEWLINE") in
  let imp     = iterate (sumImpurity tree) in
  let _ = printsym (quote "End") in
  let _ = printsym (quote "NEWLINE") in
  let _ = printsym (quote "Running pass sumSamples (fold, uses=3): ") in
  let _ = printsym (quote "NEWLINE") in
  let samples = iterate (sumSamples tree) in
  let _ = printsym (quote "End") in
  let _ = printsym (quote "NEWLINE") in
  let _ = printsym (quote "Running pass countFeatureUses (fold, uses=3): ") in
  let _ = printsym (quote "NEWLINE") in
  let feat0   = iterate (countFeatureUses 0 tree) in
  let _ = printsym (quote "End") in
  let _ = printsym (quote "NEWLINE") in
  let _ = printsym (quote "Running pass countSmallLeaves (fold, uses=3): ") in
  let _ = printsym (quote "NEWLINE") in
  let small   = iterate (countSmallLeaves 5 tree) in
  let _ = printsym (quote "End") in
  let _ = printsym (quote "NEWLINE") in
  let _ = printsym (quote "Running pass inferenceCost (fold, uses=2): ") in
  let _ = printsym (quote "NEWLINE") in
  let cost  = iterate (inferenceCost tree) in
  let _ = printsym (quote "End") in
  let _ = printsym (quote "NEWLINE") in
  let _ = printsym (quote "Running pass sumPathLengths (fold, uses=3): ") in
  let _ = printsym (quote "NEWLINE") in
  let paths = iterate (sumPathLengths 0 tree) in
  let _ = printsym (quote "End") in
  let _ = printsym (quote "NEWLINE") in
  (nodes, leaves, depth, imp, samples, feat0, small, cost, paths)
