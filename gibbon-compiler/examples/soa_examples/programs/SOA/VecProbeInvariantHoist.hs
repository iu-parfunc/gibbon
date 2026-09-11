-- VecProbeInvariantHoist: VList (Factored).
-- Functions: mkVList, mapInv, sumFirst.
-- Annotated: MayVectorize on mapInv; StoreScalarCounts on mkVList.
data VList = VCons Int Int VList | VNil
{-# ANN type VList "Factored" #-}

{-# ANN mkVList "OPT:StoreScalarCounts" #-}
mkVList :: Int -> VList
mkVList n =
  if n <= 0
  then VNil
  else let rst = mkVList (n - 1)
       in VCons n (n + 10) rst

{-# ANN mapInv "OPT:MayVectorize" #-}
mapInv :: VList -> Int -> Int -> VList
mapInv xs k m =
  case xs of
    VNil -> VNil
    VCons i j rst ->
      let i1 = i + (k / m)
          j1 = j + (k / m)
      in VCons i1 j1 (mapInv rst k m)

sumFirst :: VList -> Int
sumFirst xs =
  case xs of
    VNil -> 0
    VCons i _ rst -> i + sumFirst rst

gibbon_main =
  let xs = mkVList 0
      ys = mapInv xs 10 0
  in sumFirst ys
