-- Characterizes deliberate loss D4: a CanVectorize producer whose cursorized
-- ABI is NOT the exact four-cursor-array shape (here: two packed SoA inputs and
-- one packed SoA output -> six cursor arrays).  `soaOutputCursorShape` now
-- returns Nothing, so `rewriteSelectiveFun` refuses to share anything.
data L = C Int Int L | N
{-# ANN type L "Factored" #-}

{-# ANN mkL "OPT:StoreScalarCounts" #-}
mkL :: Int -> L
mkL n = if n <= 0 then N else let r = mkL (n - 1) in C n (n * 2) r

-- Two packed inputs, one packed output; field b is a pure copy of xs's b.
{-# ANN zipAdd "OPT:CanVectorize" #-}
zipAdd :: L -> L -> L
zipAdd xs ys =
  case xs of
    N -> N
    C a b r ->
      case ys of
        N -> N
        C c d r2 -> C (a + c) b (zipAdd r r2)

sumA :: L -> Int
sumA xs = case xs of
            N -> 0
            C a b r -> let s = sumA r in a + s

sumB :: L -> Int
sumB xs = case xs of
            N -> 0
            C a b r -> let s = sumB r in b + s

gibbon_main =
  let xs = mkL 100
      ys = mkL 100
      zs = zipAdd xs ys
  in (sumA zs, sumB zs)
