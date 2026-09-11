-- VectorMixedWidths: Mixed (Factored).
-- Functions: mkMixed, bump, total.
-- Annotated: MayVectorize on bump; StoreScalarCounts on mkMixed.
data Mixed = MCons Int8 Int16 Int32 Int64 Mixed | MNil
{-# ANN type Mixed "Factored" #-}

{-# ANN mkMixed "OPT:StoreScalarCounts" #-}
mkMixed :: Int64 -> Mixed
mkMixed n =
  if n <= 0
  then MNil
  else MCons (toInt8 (mod n 50)) (toInt16 (mod n 3000)) (toInt32 (n * 1000)) (n * 100000)
             (mkMixed (n - 1))

-- Each field gets its own transformation: W8 add, W16 add, W32 add, W64 add.
{-# ANN bump "OPT:MayVectorize" #-}
bump :: Mixed -> Mixed
bump xs =
  case xs of
    MNil -> MNil
    MCons a b c d rst -> MCons (a + 3) (b + 5) (c + 7) (d + 11) (bump rst)

total :: Mixed -> Int64
total xs =
  case xs of
    MNil -> 0
    MCons a b c d rst -> toInt64 a + toInt64 b + toInt64 c + d + total rst

gibbon_main =
  let xs = mkMixed 17
      ys = bump xs
  in total ys
