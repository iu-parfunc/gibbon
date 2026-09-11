-- VerifyS1ManyBufferCrossField: W (Factored).
-- Functions: mkW, swap1and11, sumF1, sumF11, sumF12.
-- Annotated: MayVectorize on swap1and11; StoreScalarCounts on mkW.
data W = WC Int Int Int Int Int Int Int Int Int Int Int Int Int W | WN
{-# ANN type W "Factored" #-}

{-# ANN mkW "OPT:StoreScalarCounts" #-}
mkW :: Int -> W
mkW n =
  if n <= 0
  then WN
  else let rst = mkW (n - 1)
       in WC n (n*2) (n*3) (n*4) (n*5) (n*6) (n*7) (n*8) (n*9) (n*10) (n*11) (n*100000) (n*13) rst

-- f1  <- f11 (dep on a two-digit buffer index)
-- f11 <- f1  (dep on a one-digit buffer index)
-- everything else is a genuine self copy and MAY be shared.
{-# ANN swap1and11 "OPT:MayVectorize" #-}
swap1and11 :: W -> W
swap1and11 xs =
  case xs of
    WN -> WN
    WC a b c d e f g h i j k l m rst -> WC k b c d e f g h i j a l m (swap1and11 rst)

sumF1 :: W -> Int
sumF1 xs = case xs of
             WN -> 0
             WC a b c d e f g h i j k l m rst -> let s = sumF1 rst in a + s

sumF11 :: W -> Int
sumF11 xs = case xs of
              WN -> 0
              WC a b c d e f g h i j k l m rst -> let s = sumF11 rst in k + s

sumF12 :: W -> Int
sumF12 xs = case xs of
              WN -> 0
              WC a b c d e f g h i j k l m rst -> let s = sumF12 rst in l + s

gibbon_main =
  let xs = mkW 20
      ys = swap1and11 xs
      s1 = sumF1 ys
      s11 = sumF11 ys
      s12 = sumF12 ys
  in (s1, s11, s12)
