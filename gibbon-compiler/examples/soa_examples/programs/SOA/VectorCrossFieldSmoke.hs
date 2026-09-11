-- Smoke test for vectorizing scalar expression DAGs with cross-field reads.
-- The map is shape preserving and has no parent-child dependency: each output
-- scalar field is computed from scalar fields in the same constructor instance.
data VList = VCons Int Int VList | VNil
{-# ANN type VList "Factored" #-}

{-# ANN mkVList "OPT:StoreScalarCounts" #-}
mkVList :: Int -> VList
mkVList n =
  if n <= 0
  then VNil
  else let rst = mkVList (n - 1)
       in VCons n (n + 10) rst

{-# ANN mapCross "OPT:MayVectorize" #-}
mapCross :: VList -> Int -> VList
mapCross xs k =
  case xs of
    VNil -> VNil
    VCons i j rst ->
      let i1 = i + (j + k)
          j1 = (j - i) + k
      in VCons i1 j1 (mapCross rst k)

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
  let xs = mkVList 1000
      ys = mapCross xs 7
      s1 = sumFirst ys
      s2 = sumSecond ys
  in (s1, s2)
