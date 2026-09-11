-- VW-04 regression: valid vector indexing at the lengths that were reported as
-- corrupting the heap.  Correctness-only; NOT a benchmark.
--
-- For each length n, `generate n (\j -> j*j)` is built and its FIRST and LAST
-- elements are read.  A dropped write, an off-by-one in the generate loop, or a
-- one-past-the-end read all change the sum rather than crashing, so this is a
-- value check and not merely a "did not segfault" check.
--
-- Expected value, derived by hand (not from the compiler):
--   sum over n of (0^2 + (n-1)^2)
--     n=1  -> 0      n=8  -> 49     n=16 -> 225
--     n=17 -> 256    n=24 -> 529    n=32 -> 961
--   total = 0 + 49 + 225 + 256 + 529 + 961 = 2020
module VecIndexLengths where

import Gibbon.Vector

endpoints :: Int -> Int
endpoints n =
  let v = generate n (\j -> j * j)
  in (nth v 0) + (nth v (n - 1))

gibbon_main =
  endpoints 1 + endpoints 8 + endpoints 16
    + endpoints 17 + endpoints 24 + endpoints 32
