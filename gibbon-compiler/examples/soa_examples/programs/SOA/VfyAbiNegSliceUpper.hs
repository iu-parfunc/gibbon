-- F4 variant: same ABI defect reached through the *upper* bounds check of
-- gib_vector_slice (a different code path in the same function) with a
-- negative length rather than a negative lower index.
gibbon_main =
  let v0 :: Vector Int
      v0 = valloc 8
      v1 = inplacevupdate v0 0 10
      negn = 0 - 2
      s = vslice 4 negn v1
  in vlength s
