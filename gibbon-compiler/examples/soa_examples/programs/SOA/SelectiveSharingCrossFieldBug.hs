-- REPRODUCER (audit artifact): selective buffer sharing miscompiles a
-- cross-field scalar copy.
--
-- `copyAtoB` writes field `a`'s value into field `b`.  Buffer 2 (field `b`) is
-- therefore NOT a pure copy of its own input, but
-- `Gibbon.Passes.SelectiveBufferSharing.isScalarCopyInner` only checks that the
-- single WriteScalar's value came from *some* ReadScalar in the loop body; it
-- never checks that the ReadScalar used buffer 2's own input cursor.  The
-- loopifier's cross-buffer dependency read (`..._buf2_dep1_read_val`) is bound
-- by `ProjE 0 (ReadScalar ...)` exactly like a same-buffer read, so buffer 2 is
-- classified as a pure copy and shared with the input.
--
-- Expected (== output without --enable-selective-buffer-sharing): 55
-- Observed with --enable-selective-buffer-sharing:               55000
--
-- Repro:
--   gibbon --use-mutable-cursors --no-ran --store-scalar-field-counts \
--          --enable-loopification --auto-loopification \
--          --packed --to-exe SelectiveSharingCrossFieldBug.hs      -- prints 55
--   gibbon ... --enable-selective-buffer-sharing ...               -- prints 55000

data L = C Int Int L | N
{-# ANN type L "Factored" #-}

{-# ANN mkL "OPT:StoreScalarCounts" #-}
mkL :: Int -> L
mkL n =
  if n <= 0
  then N
  else let rst = mkL (n - 1)
       in C n (n * 1000) rst

-- Copy field a into field b; field a itself is unchanged.
{-# ANN copyAtoB "OPT:CanVectorize" #-}
copyAtoB :: L -> L
copyAtoB xs =
  case xs of
    N -> N
    C a b rst -> C a a (copyAtoB rst)

sumB :: L -> Int
sumB xs =
  case xs of
    N -> 0
    C a b rst -> b + sumB rst

gibbon_main =
  let xs = mkL 10
      ys = copyAtoB xs
  in sumB ys
