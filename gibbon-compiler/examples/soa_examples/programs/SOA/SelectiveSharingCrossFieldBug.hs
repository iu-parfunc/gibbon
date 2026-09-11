-- SelectiveSharingCrossFieldBug: L (Factored).
-- Functions: mkL, copyAtoB, sumB.
-- Annotated: MayVectorize on copyAtoB; StoreScalarCounts on mkL.

data L = C Int Int L | N
{-# ANN type L "Factored" #-}

{-# ANN mkL "OPT:StoreScalarCounts" #-}
mkL :: Int -> L
mkL n =
  if n <= 0
  then N
  else let rst = mkL (n - 1)
       in C n (n * 1000) rst

-- Copy field a into field b; field a itself is unchanged.
{-# ANN copyAtoB "OPT:MayVectorize" #-}
copyAtoB :: L -> L
copyAtoB xs =
  case xs of
    N -> N
    C a b rst -> C a a (copyAtoB rst)

sumB :: L -> Int
sumB xs =
  case xs of
    N -> 0
    C a b rst -> b + sumB rst

gibbon_main =
  let xs = mkL 10
      ys = copyAtoB xs
  in sumB ys
