-- Int32 audit probe: SoA (Factored) layout, mixed scalar widths.
data PList = PCons Int Float Int PList | PNil
{-# ANN type PList "Factored" #-}

{-# ANN mkPList "OPT:StoreScalarCounts" #-}
mkPList :: Int -> PList
mkPList n =
  if n <= 0
  then PNil
  else let rst = mkPList (n - 1)
       in PCons n 2.0 (n + 1000) rst

sumA :: PList -> Int
sumA xs =
  case xs of
    PNil -> 0
    PCons a f b rst -> a + sumA rst

sumB :: PList -> Int
sumB xs =
  case xs of
    PNil -> 0
    PCons a f b rst -> b + sumB rst

sumF :: PList -> Float
sumF xs =
  case xs of
    PNil -> 0.0
    PCons a f b rst -> f .+. sumF rst

lenP :: PList -> Int
lenP xs =
  case xs of
    PNil -> 0
    PCons a f b rst -> 1 + lenP rst

{-# ANN add1P "OPT:CanVectorize" #-}
add1P :: PList -> PList
add1P xs =
  case xs of
    PNil -> PNil
    PCons a f b rst -> PCons (a + 1) (f .+. 1.0) (b + 2) (add1P rst)

gibbon_main =
  let xs = mkPList 101
      ys = add1P xs
      a = sumA ys
      b = sumB ys
      c = lenP ys
      d = sumF ys
  in (a, b, c, d)
