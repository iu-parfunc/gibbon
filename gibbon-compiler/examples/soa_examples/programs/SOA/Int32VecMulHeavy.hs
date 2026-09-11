-- Audit probe (int32 vectorizer): a multiply-dominated MayVectorize map, used
-- to measure the cost of the scalarized `gib_vec_mul_int32x4` helper against a
-- hand-patched `_mm_mullo_epi32` (SSE4.1) version of the same generated C.
data ML = MCons Int Int ML | MNil
{-# ANN type ML "Factored" #-}

{-# ANN mkML "OPT:StoreScalarCounts" #-}
mkML :: Int -> ML
mkML n =
  if n <= 0
  then MNil
  else let rst = mkML (n - 1)
       in MCons n (n + 1) rst

{-# ANN mapMul "OPT:MayVectorize" #-}
mapMul :: ML -> Int -> ML
mapMul xs k =
  case xs of
    MNil -> MNil
    MCons i j rst ->
      MCons (((((((((((((i + 0) * (i + 1)) * (i + 2)) * (i + 3)) * (i + 4)) * (i + 5)) * (i + 6)) * (i + 7)) * (i + 8)) * (i + 9)) * (i + 10)) * (i + 11)) * k)
            (((((((((((((j + 0) * (j + 1)) * (j + 2)) * (j + 3)) * (j + 4)) * (j + 5)) * (j + 6)) * (j + 7)) * (j + 8)) * (j + 9)) * (j + 10)) * (j + 11)) * k)
            (mapMul rst k)

sumML :: ML -> Int
sumML xs =
  case xs of
    MNil -> 0
    MCons i j rst -> (i + j) + sumML rst

gibbon_main =
  let n0 = sizeParam
      k0 = (mod n0 5) + 2
      x0 = mkML n0
      x1 = mapMul x0 k0
      x2 = mapMul x1 k0
      x3 = mapMul x2 k0
      x4 = mapMul x3 k0
      x5 = mapMul x4 k0
      x6 = mapMul x5 k0
      x7 = mapMul x6 k0
      x8 = mapMul x7 k0
  in sumML x8
