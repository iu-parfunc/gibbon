-- Int32 audit probe: exercises the `iterate` / TimeIt codegen path
-- (which prints "SIZE:" / "ITERS:" derived from GibInt-typed RTS getters).
data PList = PCons Int PList | PNil
{-# ANN type PList "Linear" #-}

mkPList :: Int -> PList
mkPList n =
  if n <= 0
  then PNil
  else let rst = mkPList (n - 1)
       in PCons n rst

sumP :: PList -> Int
sumP xs =
  case xs of
    PNil -> 0
    PCons a rst -> a + sumP rst

gibbon_main =
  let xs = mkPList 101
      s = iterate (sumP xs)
  in s
