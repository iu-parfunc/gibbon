-- Independent verification of the RTS ABI width defect (F4).
--
-- `gib_vector_slice` is called from generated code.  Pre-fix its prototype was
-- `GibVector *gib_vector_slice(GibInt, GibInt, GibVector *)`, and `GibInt` is
-- `int32_t` in the generated TU (which is built with -DGIBBON_INT32 under
-- --int32) but `int64_t` in the RTS TU (which is never rebuilt per width).  A
-- negative index therefore arrives in the callee with a garbage upper half.
gibbon_main =
  let v0 :: Vector Int
      v0 = valloc 8
      v1 = inplacevupdate v0 0 10
      v2 = inplacevupdate v1 1 20
      v3 = inplacevupdate v2 2 30
      v4 = inplacevupdate v3 3 40
      v5 = inplacevupdate v4 4 50
      v6 = inplacevupdate v5 5 60
      v7 = inplacevupdate v6 6 70
      v8 = inplacevupdate v7 7 80
      neg = 0 - 1
      s = vslice neg 2 v8
  in vlength s
