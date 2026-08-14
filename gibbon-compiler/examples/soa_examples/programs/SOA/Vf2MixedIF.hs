-- Variant reproducer: Int and Float scalar fields mutated by the same
-- CanVectorize map (2 int groups at 64-bit / 1 at int32, vs 1 float group).
data MF = MFCons Int Float Int Float MF | MFNil
{-# ANN type MF "Factored" #-}

{-# ANN mkMF "OPT:StoreScalarCounts" #-}
mkMF :: Int -> MF
mkMF n =
  if n <= 0
  then MFNil
  else let rst = mkMF (n - 1)
           fn = intToFloat n
       in MFCons n (fn .+. 0.5) (n * 3) (fn .*. 2.0) rst

{-# ANN mapMF "OPT:CanVectorize" #-}
mapMF :: MF -> Int -> MF
mapMF xs k =
  case xs of
    MFNil -> MFNil
    MFCons a x b y rst -> MFCons (a + k) (x .+. 1.5) (b - k) (y .*. 3.0) (mapMF rst k)

sumI :: MF -> Int
sumI xs = case xs of
            MFNil -> 0
            MFCons a _ b _ rst -> (a + b) + sumI rst

sumF :: MF -> Float
sumF xs = case xs of
            MFNil -> 0.0
            MFCons _ x _ y rst -> (x .+. y) .+. sumF rst

gibbon_main =
  let n0 = sizeParam
      xs = mkMF n0
      ys = mapMF xs 7
  in (sumI ys, sumF ys)
