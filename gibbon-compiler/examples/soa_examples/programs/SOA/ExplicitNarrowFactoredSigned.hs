-- Genuine SoA (Factored) coverage for negative narrow values, boundary
-- values, and comparisons on fields extracted from a factored structure.
-- Int16 here (Int8/Int32 are covered by ExplicitNarrowFactored.hs), plus an
-- Int64 control field alongside it in the same constructor.
data Signed = SNil | SCons Int16 Int64 Signed
{-# ANN type Signed "Factored" #-}

{-# ANN mkSigned "OPT:StoreScalarCounts" #-}
mkSigned :: Int64 -> Signed
mkSigned n =
  if n < 1
  then SNil
  else
    let a :: Int16
        a = if n == 1 then -32768 else if n == 2 then 32767 else if n == 3 then -3 else -4
    in SCons a n (mkSigned (n - 1))

sumSigned16 :: Signed -> Int16
sumSigned16 xs =
  case xs of
    SNil -> 0
    SCons a _b rst -> a + (sumSigned16 rst)

allNonNegative64 :: Signed -> Bool
allNonNegative64 xs =
  case xs of
    SNil -> True
    SCons _a b rst -> if b < 0 then False else allNonNegative64 rst

gibbon_main =
  let xs = mkSigned 4
      s = sumSigned16 xs
      ok = allNonNegative64 xs
      _u1 = printint s
  in ok
