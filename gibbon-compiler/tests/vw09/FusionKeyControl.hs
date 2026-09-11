-- B22 adversarial probe: two DISTINCT constructors whose names differ only in
-- a character that LoopifyTraversals.sanitizeLoopName maps to '_'.
--   Ap  -> "Aq"      Aq  -> "Aq"
-- LoopifiedTraversalFusion re-derives its fusion key from that sanitized loop
-- NAME, so both constructors' buffers parse to the same key.  Their per-chunk
-- element counts are deliberately unequal (Ap : Aq = 1 : 3).
data T = Ap Int T | Aq Int T | End
{-# ANN type T "Factored" #-}

{-# ANN mkT "OPT:StoreScalarCounts" #-}
mkT :: Int -> T
mkT n =
  if n <= 0
  then End
  else if mod n 4 == 0
       then Ap (n + 1000) (mkT (n - 1))
       else Aq (n + 2000) (mkT (n - 1))

{-# ANN bump "OPT:MayVectorize" #-}
bump :: T -> T
bump t =
  case t of
    End -> End
    Ap x rst -> Ap (x + 1) (bump rst)
    Aq y rst -> Aq (y + 7) (bump rst)

sumP :: T -> Int
sumP t = case t of
           End -> 0
           Ap x rst -> x + sumP rst
           Aq y rst -> sumP rst

sumU :: T -> Int
sumU t = case t of
           End -> 0
           Ap x rst -> sumU rst
           Aq y rst -> y + sumU rst

lenT :: T -> Int
lenT t = case t of
           End -> 0
           Ap x rst -> 1 + lenT rst
           Aq y rst -> 1 + lenT rst

gibbon_main = let t = mkT 40
                  u = bump t
              in (lenT u, sumP u, sumU u)
