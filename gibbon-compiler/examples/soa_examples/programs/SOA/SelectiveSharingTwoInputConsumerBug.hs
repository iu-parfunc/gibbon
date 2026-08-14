-- REPRODUCER (audit artifact): selective buffer sharing leaves the
-- selective-indirection wrapper un-normalized when the consumer of the shared
-- value takes two packed SoA arguments.
--
-- After cursorization `zipSum` has four CursorArray arguments,
-- (ends_xs, ends_ys, curs_xs, curs_ys).
-- `Gibbon.Passes.SelectiveBufferSharing.soaInputCursorShapes` matches its
-- "producer" pattern `(endIx,_,n1) : _ : _ : (curIx,_,n2) : _` and pairs
-- argument 0 with argument 3, i.e. one input's *ends* array with the other
-- input's *cursor* array.  That pair is never a marked selective pair, so no
-- `UnwrapSelectiveIndirections` is emitted and the consumer reads the raw
-- GIB_SELECTIVE_INDIRECTION_TAG (249) from the shared dcon buffer.
--
-- Expected (== output without --enable-selective-buffer-sharing): 10200
-- Observed with --enable-selective-buffer-sharing:
--   "Unknown tag in: tmpval_<n>"  and exit status 1.
--
-- Swapping the argument order to `zipSum xs ys` fails identically.
--
-- Repro:
--   gibbon --use-mutable-cursors --no-ran --store-scalar-field-counts \
--          --enable-loopification --auto-loopification \
--          --packed --to-exe SelectiveSharingTwoInputConsumerBug.hs  -- 10200
--   gibbon ... --enable-selective-buffer-sharing ...                 -- crashes

data List = Cons Int Float List | Nil
{-# ANN type List "Factored" #-}

{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int -> List
mkList len =
  if len <= 0
  then Nil
  else let rst = mkList (len - 1)
       in Cons len 1.0 rst

{-# ANN add1KeepFloat "OPT:CanVectorize" #-}
add1KeepFloat :: List -> List
add1KeepFloat xs =
  case xs of
    Nil -> Nil
    Cons i f rst -> Cons (i + 1) f (add1KeepFloat rst)

zipSum :: List -> List -> Int
zipSum xs ys =
  case xs of
    Nil -> 0
    Cons i _ r ->
      case ys of
        Nil -> i
        Cons j _ r2 -> i + j + zipSum r r2

gibbon_main =
  let xs = mkList 100
      ys = add1KeepFloat xs
  in zipSum ys xs
