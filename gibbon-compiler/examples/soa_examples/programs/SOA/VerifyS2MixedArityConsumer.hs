-- VARIANT of the S2 defect: a consumer of the selectively shared value that
-- takes two packed SoA inputs of DIFFERENT types, hence four cursor arrays of
-- UNEQUAL lengths (2 and 3).  The old positional pattern paired arg0 with arg3
-- and required equal lengths, so it produced NO pairs at all
-- (the `_ -> []` hole), leaving the wrapper un-normalized.
data L = C Int L | N
{-# ANN type L "Factored" #-}

data T = TC Int Float T | TN
{-# ANN type T "Factored" #-}

{-# ANN mkL "OPT:StoreScalarCounts" #-}
mkL :: Int -> L
mkL n = if n <= 0 then N else let r = mkL (n - 1) in C n r

{-# ANN mkT "OPT:StoreScalarCounts" #-}
mkT :: Int -> T
mkT n = if n <= 0 then TN else let r = mkT (n - 1) in TC n 2.0 r

{-# ANN bumpL "OPT:CanVectorize" #-}
bumpL :: L -> L
bumpL xs = case xs of
             N -> N
             C i rst -> C (i + 1) (bumpL rst)

-- Consumes the shared L value alongside an unrelated T value.
mixed :: L -> T -> Int
mixed xs ts =
  case xs of
    N -> 0
    C i r ->
      case ts of
        TN -> i
        TC j _ r2 -> let s = mixed r r2 in i + j + s

gibbon_main =
  let xs = mkL 100
      ts = mkT 100
      ys = bumpL xs
  in mixed ys ts
