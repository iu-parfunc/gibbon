-- VerifyS2MixedArityConsumer: L (Factored), T (Factored).
-- Functions: mkL, mkT, bumpL, mixed.
-- Annotated: MayVectorize on bumpL; StoreScalarCounts on mkL, mkT.
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

{-# ANN bumpL "OPT:MayVectorize" #-}
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
