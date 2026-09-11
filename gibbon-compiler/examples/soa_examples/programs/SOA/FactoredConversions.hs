-- FactoredConversions: Narrow (Factored).
-- Functions: build, total.
{-# ANN type Narrow "Factored" #-}

module FactoredConversions where

data Narrow = NNil | NCons Int8 Int32 Narrow

-- Build with values that only fit after truncation, so a saturating or
-- widening implementation changes the printed structure.
build :: Int64 -> Narrow
build n =
  if n == 0
  then NNil
  else NCons (toInt8 (n + 126)) (toInt32 (n * 1000000000)) (build (n - 1))

-- Widen every narrow field back to Int64 before reducing, so the sum is
-- computed homogeneously at one width.
total :: Narrow -> Int64
total ls =
  case ls of
    NNil -> 0
    NCons a b rest -> toInt64 a + toInt64 b + total rest

gibbon_main =
  let ls = build 3
      _ = printPacked ls
      _ = printint (total ls)
  in 0
