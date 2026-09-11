-- VectorMixedFallback: Mixed (Factored).
-- Functions: mkMixed, xform, total.
-- Annotated: MayVectorize on xform; StoreScalarCounts on mkMixed.
data Mixed = MCons Int8 Int16 Int32 Int64 Mixed | MNil
{-# ANN type Mixed "Factored" #-}

{-# ANN mkMixed "OPT:StoreScalarCounts" #-}
mkMixed :: Int64 -> Mixed
mkMixed n =
  if n <= 0
  then MNil
  else MCons (toInt8 (mod n 40)) (toInt16 (mod n 100)) (toInt32 (mod n 1000)) (n * 7)
             (mkMixed (n - 1))

{-# ANN xform "OPT:MayVectorize" #-}
xform :: Mixed -> Mixed
xform xs =
  case xs of
    MNil -> MNil
    MCons a b c d rst -> MCons (a + 3) (b * 2) (c * 3) (d + 11) (xform rst)

total :: Mixed -> Int64
total xs =
  case xs of
    MNil -> 0
    MCons a b c d rst -> toInt64 a + toInt64 b + toInt64 c + d + total rst

gibbon_main =
  let xs = mkMixed 19
      ys = xform xs
  in total ys
