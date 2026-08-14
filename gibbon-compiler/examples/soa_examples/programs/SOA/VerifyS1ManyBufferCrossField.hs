-- VARIANT of the S1 defect, stressing `isOwnLoopCursor`'s exact-match claim at
-- buffer indices >= 10.  W has 13 Int fields, so loop buffers run 0..13 and the
-- cross-buffer dependency cursor names include `_buf1_dep11_read_cur` and
-- `_buf11_dep1_read_cur`.  A prefix/suffix mismatch (`_buf1_` matching inside
-- `_buf11_`) would misclassify one of the cross-field writes as a pure copy.
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
{-# ANN swap1and11 "OPT:CanVectorize" #-}
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
