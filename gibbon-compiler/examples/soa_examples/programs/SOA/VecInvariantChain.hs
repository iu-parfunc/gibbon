-- VecInvariantChain: L (Factored).
-- Functions: mkL, mp, sumFirst.
-- Annotated: MayVectorize on mp; StoreScalarCounts on mkL.
data L = C Int Int L | Nil
{-# ANN type L "Factored" #-}

{-# ANN mkL "OPT:StoreScalarCounts" #-}
mkL :: Int -> L
mkL n = if n <= 0 then Nil else let rst = mkL (n - 1) in C n (n + 7) rst

{-# ANN mp "OPT:MayVectorize" #-}
mp :: L -> Int -> L
mp xs k =
  case xs of
    Nil -> Nil
    C i j rst ->
      let t1 = k + 1        -- invariant, level 1
          m1 = t1 * 2       -- invariant, level 2 -- depends on t1  <-- TRIGGER
          x1 = i * m1 + 1
      in C x1 j (mp rst k)

sumFirst :: L -> Int
sumFirst xs = case xs of
    Nil -> 0
    C i _ rst -> i + sumFirst rst

gibbon_main =
  let n  = 64
      k  = sizeParam + 3
      xs = mkL n
      ys = mp xs k
  in sumFirst ys
