-- Probe: an Int (64-bit -> two 2-lane groups) field and a Float (4-lane)
-- field mutated by the same MayVectorize map.  With --opt-loop-fusion the
-- two per-buffer loops are fused into a single ForE, so the vectorizer must
-- reconcile 2 int groups against 1 float group in one logical stride of 4.
data MList = MCons Int Float MList | MNil
{-# ANN type MList "Factored" #-}

{-# ANN mkMList "OPT:StoreScalarCounts" #-}
mkMList :: Int -> MList
mkMList n =
  if n <= 0
  then MNil
  else let rst = mkMList (n - 1)
       in MCons n 2.0 rst

{-# ANN mapMixed "OPT:MayVectorize" #-}
mapMixed :: MList -> Int -> MList
mapMixed xs k =
  case xs of
    MNil -> MNil
    MCons i f rst -> MCons (i + k) (f .*. 3.0) (mapMixed rst k)

sumInt :: MList -> Int
sumInt xs =
  case xs of
    MNil -> 0
    MCons i _ rst -> i + sumInt rst

sumFlt :: MList -> Float
sumFlt xs =
  case xs of
    MNil -> 0.0
    MCons _ f rst -> f .+. sumFlt rst

gibbon_main =
  let xs = mkMList 1003
      ys = mapMixed xs 7
      a = sumInt ys
      b = sumFlt ys
  in (a, b)
