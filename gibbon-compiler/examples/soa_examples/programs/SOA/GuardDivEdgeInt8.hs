-- GuardDivEdgeInt8: L (Factored).
-- Functions: numOf, denOf, mkL, xform, sumL.
-- Annotated: MayVectorize on xform; StoreScalarCounts on numOf, mkL.
data L = Cons Int8 Int8 Int8 Int8 L | Nil
{-# ANN type L "Factored" #-}

{-# ANN numOf "OPT:StoreScalarCounts" #-}
numOf :: Int64 -> Int64
numOf k =
  if k == 1 then (0 - 127) - 1 else
  if k == 2 then (0 - 127) - 1 else
  if k == 3 then 7 else
  if k == 4 then 0 - 7 else
  if k == 5 then 7 else
  if k == 6 then 0 - 7 else
  if k == 7 then 127 else 5

denOf :: Int64 -> Int64
denOf k =
  if k == 1 then 0 - 1 else
  if k == 2 then 1 else
  if k == 3 then 3 else
  if k == 4 then 3 else
  if k == 5 then 0 - 3 else
  if k == 6 then 0 - 3 else
  if k == 7 then 0 - 1 else 0

{-# ANN mkL "OPT:StoreScalarCounts" #-}
mkL :: Int64 -> L
mkL k =
  if k <= 0
  then Nil
  else Cons (toInt8 (numOf k)) (toInt8 (denOf k)) 0 0 (mkL (k - 1))

{-# ANN xform "OPT:MayVectorize" #-}
xform :: L -> L
xform xs = case xs of
             Nil -> Nil
             Cons n d q r rst ->
               Cons n d (if d == 0 then 99 else n / d)
                        (if d == 0 then 99 else mod n d)
                        (xform rst)

sumL :: L -> Int64
sumL xs = case xs of
            Nil -> 0
            Cons n d q r rst -> toInt64 q + toInt64 r + sumL rst

gibbon_main = sumL (xform (mkL 8))
