-- B22 adversarial probe: two DISTINCT constructors whose names differ only in
-- a character that LoopifyTraversals.sanitizeLoopName maps to '_'.
--   A'  -> "A_"      A_  -> "A_"
-- LoopifiedTraversalFusion re-derives its fusion key from that sanitized loop
-- NAME, so both constructors' buffers parse to the same key.  Their per-chunk
-- element counts are deliberately unequal (A' : A_ = 1 : 3).
data T = A' Int T | A_ Int T | End
{-# ANN type T "Factored" #-}

{-# ANN mkT "OPT:StoreScalarCounts" #-}
mkT :: Int -> T
mkT n =
  if n <= 0
  then End
  else if mod n 4 == 0
       then A' (n + 1000) (mkT (n - 1))
       else A_ (n + 2000) (mkT (n - 1))

{-# ANN bump "OPT:MayVectorize" #-}
bump :: T -> T
bump t =
  case t of
    End -> End
    A' x rst -> A' (x + 1) (bump rst)
    A_ y rst -> A_ (y + 7) (bump rst)

sumP :: T -> Int
sumP t = case t of
           End -> 0
           A' x rst -> x + sumP rst
           A_ y rst -> sumP rst

sumU :: T -> Int
sumU t = case t of
           End -> 0
           A' x rst -> sumU rst
           A_ y rst -> y + sumU rst

lenT :: T -> Int
lenT t = case t of
           End -> 0
           A' x rst -> 1 + lenT rst
           A_ y rst -> 1 + lenT rst

gibbon_main = let t = mkT 40
                  u = bump t
              in (lenT u, sumP u, sumU u)
