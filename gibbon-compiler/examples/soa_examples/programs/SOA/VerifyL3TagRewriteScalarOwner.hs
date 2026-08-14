-- VARIANT of the L3 defect (tag rewrite dropped by the verbatim tag copy),
-- exercising a constructor that DOES own a scalar buffer.
-- CA and CB both carry an Int, so the branch has a non-empty scalar plan --
-- unlike the original nullary NilA->NilB reproducer.
data L = CA Int L | CB Int L | Nil
{-# ANN type L "Factored" #-}

{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int -> L
mkList n = if n <= 0
           then Nil
           else let rst = mkList (n - 1)
                 in CA n rst

-- Rewrites CA -> CB while also bumping the scalar field.
{-# ANN flipCon "OPT:CanVectorize" #-}
flipCon :: L -> L
flipCon lst = case lst of
                Nil -> Nil
                CA i rst -> let i1 = i + 1
                             in CB i1 (flipCon rst)
                CB i rst -> let i1 = i + 1
                             in CB i1 (flipCon rst)

countB :: L -> Int
countB lst = case lst of
               Nil -> 0
               CA i rst -> countB rst
               CB i rst -> let s = countB rst in 1 + s

sumList :: L -> Int
sumList lst = case lst of
                Nil -> 0
                CA i rst -> let s = sumList rst in i + s
                CB i rst -> let s = sumList rst in i + s

gibbon_main =
  let l1 = mkList 20
      m  = flipCon l1
      b  = countB m
      s  = sumList m
   in (b, s)
