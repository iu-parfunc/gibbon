-- Smoke test for the SIMD vector-register expression DAG recognizer.
-- Field 0 uses the loaded scalar twice; field 1 uses nested add/sub.
data VList = VCons Int Int VList | VNil
{-# ANN type VList "Factored" #-}

{-# ANN mkVList "OPT:StoreScalarCounts" #-}
mkVList :: Int -> VList
mkVList n =
  if n <= 0
  then VNil
  else let rst = mkVList (n - 1)
       in VCons n (n + 10) rst

{-# ANN mapExpr "OPT:CanVectorize" #-}
mapExpr :: VList -> Int -> VList
mapExpr xs k =
  case xs of
    VNil -> VNil
    VCons i j rst ->
      let i1 = (i + k) + (i + 3)
          j1 = (j - k) + (j + 2)
      in VCons i1 j1 (mapExpr rst k)

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
      ys = mapExpr xs 5
      s1 = sumFirst ys
      s2 = sumSecond ys
  in (s1, s2)
