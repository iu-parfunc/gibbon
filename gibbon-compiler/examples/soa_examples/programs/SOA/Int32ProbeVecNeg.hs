-- Int32 audit probe: passes a negative GibInt across the generated-code/RTS
-- boundary.  The RTS is compiled without -DGIBBON_INT32, so gib_vector_slice's
-- first parameter is int64_t there while the caller passes a 32-bit value.
gibbon_main =
  let v0 :: Vector Int
      v0 = valloc 5
      v1 = inplacevupdate v0 0 10
      v2 = inplacevupdate v1 1 20
      v3 = inplacevupdate v2 2 30
      v4 = inplacevupdate v3 3 40
      v5 = inplacevupdate v4 4 50
      neg = 0 - 1
      s = vslice neg 2 v5
      m = vlength s
  in m
