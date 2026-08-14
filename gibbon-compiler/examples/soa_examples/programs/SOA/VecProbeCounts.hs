-- Probe: exercise the vector/scalar-tail split for element counts that are
-- not multiples of the logical stride (4), counts smaller than the stride,
-- and the empty buffer.  The element count is the literal in `gibbon_main`.
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

lenList :: VList -> Int
lenList xs =
  case xs of
    VNil -> 0
    VCons _ _ rst -> 1 + lenList rst

gibbon_main =
  let xs = mkVList 1000
      ys = mapExpr xs 5
      s1 = sumFirst ys
      s2 = sumSecond ys
      n = lenList ys
  in (s1, s2, n)
