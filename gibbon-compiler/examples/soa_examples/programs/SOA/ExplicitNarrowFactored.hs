-- ExplicitNarrowFactored: Narrow (Factored).
-- Functions: mkNarrow, sumNarrow.
-- Annotated: StoreScalarCounts on mkNarrow.
data Narrow = NNil | NCons Int8 Int32 Narrow
{-# ANN type Narrow "Factored" #-}

{-# ANN mkNarrow "OPT:StoreScalarCounts" #-}
mkNarrow :: Int32 -> Narrow
mkNarrow n =
  if n < 1
  then NNil
  else NCons 2 n (mkNarrow (n - 1))

sumNarrow :: Narrow -> Int32
sumNarrow xs =
  case xs of
    NNil -> 0
    NCons _a b rst -> b + (sumNarrow rst)

gibbon_main =
  let xs = mkNarrow 5
      t = sumNarrow xs
      _u = printPacked xs
  in t
