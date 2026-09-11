-- VectorMixedCmp: Mixed (Factored).
-- Functions: mkMixed, classify, total.
-- Annotated: MayVectorize on classify; StoreScalarCounts on mkMixed.
data Mixed = MCons Int8 Int16 Int32 Int64 Mixed | MNil
{-# ANN type Mixed "Factored" #-}

{-# ANN mkMixed "OPT:StoreScalarCounts" #-}
mkMixed :: Int64 -> Mixed
mkMixed n =
  if n <= 0
  then MNil
  else MCons (toInt8 (mod n 50 - 25)) (toInt16 (mod n 3000 - 1500))
             (toInt32 (mod n 100000 - 50000)) (n - 500)
             (mkMixed (n - 1))

{-# ANN classify "OPT:MayVectorize" #-}
classify :: Mixed -> Mixed
classify xs =
  case xs of
    MNil -> MNil
    MCons a b c d rst ->
      MCons (if a < 0 then 1 else 2)
            (if b >= 0 then 3 else 4)
            (if c > 0 then 5 else 6)
            (if d <= 0 then 7 else 8)
            (classify rst)

total :: Mixed -> Int64
total xs =
  case xs of
    MNil -> 0
    MCons a b c d rst -> toInt64 a + toInt64 b + toInt64 c + d + total rst

gibbon_main =
  let xs = mkMixed 37
      ys = classify xs
  in total ys
