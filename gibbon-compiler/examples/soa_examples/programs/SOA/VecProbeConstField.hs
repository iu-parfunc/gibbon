-- Probe: one field is computed from its own input, the other is set to a
-- loop-invariant constant (so its DAG has no reads).  Exercises
-- `inputRefsForDag`/`nearestInputRefBefore` in VectorizeTraversals.hs.
data VList = VCons Int Int VList | VNil
{-# ANN type VList "Factored" #-}

{-# ANN mkVList "OPT:StoreScalarCounts" #-}
mkVList :: Int -> VList
mkVList n =
  if n <= 0
  then VNil
  else let rst = mkVList (n - 1)
       in VCons n (n + 10) rst

{-# ANN mapConst "OPT:CanVectorize" #-}
mapConst :: VList -> Int -> VList
mapConst xs k =
  case xs of
    VNil -> VNil
    VCons i j rst -> VCons (i + k) 99 (mapConst rst k)

sumFirst :: VList -> Int
sumFirst xs =
  case xs of
    VNil -> 0
    VCons i _ rst -> i + sumFirst rst

sumSecond :: VList -> Int
sumSecond xs =
  case xs of
    VNil -> 0
    VCons _ j rst -> j + sumSecond rst

gibbon_main =
  let xs = mkVList 1003
      ys = mapConst xs 5
      s1 = sumFirst ys
      s2 = sumSecond ys
  in (s1, s2)
