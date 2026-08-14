-- CORRECTNESS reproducer (audit finding F1/F2).
--
-- Recursive (no flags)          : prints 1056
-- --store-scalar-field-counts --enable-loopification --auto-loopification
--                              : glibc malloc assertion failure / SIGABRT
--
-- Cause: `dropMost` matches the SoA producer ABI recognised by
-- Gibbon/Passes/ScalarCountPropagation.hs:144-153 (`producerShape`), which
-- performs NO shape-preservation check, so
-- `ScalarCountCopyAll` installs the *input*+ per-chunk element counts on the
-- much shorter output value.  Gibbon/Passes/LoopifyTraversals.hs:1167,1174
-- then uses that count as an unchecked `ForE` trip count with no bounds
-- check, so `add1` writes far past the end of its output chunk.
data List = Cons Int List | Nil
{-# ANN type List "Factored" #-}

{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int -> List
mkList n = if n <= 0
           then Nil
           else let rst = mkList (n - 1)
                 in Cons n rst

dropMost :: List -> Int -> List
dropMost lst k = case lst of
                   Nil -> Nil
                   Cons i rst -> if k == 0
                                 then Cons i (dropMost rst 49)
                                 else dropMost rst (k - 1)

{-# ANN add1 "OPT:CanVectorize" #-}
add1 :: List -> List
add1 lst = case lst of
             Nil -> Nil
             Cons i rst -> let i1 = i + 1
                            in Cons i1 (add1 rst)

sumList :: List -> Int
sumList lst = case lst of
                Nil -> 0
                Cons i rst -> let s = sumList rst
                               in i + s

gibbon_main =
  let l1 = mkList 300
      h  = dropMost l1 0
      m  = add1 h
   in sumList m
