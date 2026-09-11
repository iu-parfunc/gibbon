-- GuardMixedIsolation: Rec (Factored).
-- Functions: mkRec, xform, sumRec.
-- Annotated: MayVectorize on xform; StoreScalarCounts on mkRec.
data Rec = Rec Int8 Int16 Int32 Int64 Rec | Nil
{-# ANN type Rec "Factored" #-}

{-# ANN mkRec "OPT:StoreScalarCounts" #-}
mkRec :: Int64 -> Rec
mkRec n =
  if n <= 0
  then Nil
  else Rec (toInt8 n) (toInt16 n) (toInt32 (n - 5)) n (mkRec (n - 1))

{-# ANN xform "OPT:MayVectorize" #-}
xform :: Rec -> Rec
xform xs = case xs of
             Nil -> Nil
             Rec a b c d rst ->
               Rec (a + 3) (b * 2) (if c == 0 then 9 else 100 / c) (d + 1) (xform rst)

sumRec :: Rec -> Int64
sumRec xs = case xs of
              Nil -> 0
              Rec a b c d rst -> toInt64 a + toInt64 b + toInt64 c + d + sumRec rst

gibbon_main = sumRec (xform (mkRec 10))
