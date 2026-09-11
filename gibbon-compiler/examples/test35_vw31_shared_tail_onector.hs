-- VW-31 control: the same let-bound recursive tail, but only ONE constructor
-- alternative consumes it.  This shape always worked -- there is nothing to
-- hoist across a join -- and it is registered so a fix for the two-alternative
-- case cannot be "stop reordering at all" without this test noticing that the
-- single-constructor path still behaves.
data U = C Int U | UEnd
{-# ANN type U "Linear" #-}

mkU :: Int -> U
mkU n =
  if n <= 0
  then UEnd
  else let rst = mkU (n - 1)
        in C (n * 7) rst

lenU :: U -> Int
lenU u = case u of
           UEnd -> 0
           C x rst -> 1 + lenU rst

sumU :: U -> Int
sumU u = case u of
           UEnd -> 0
           C x rst -> x + sumU rst

gibbon_main = let u = mkU 11 in (lenU u, sumU u)
