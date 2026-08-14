-- Int32 audit probe: exercises RTS entry points whose C prototypes mention
-- GibInt (gib_vector_alloc / gib_vector_nth / gib_vector_length /
-- gib_vector_inplace_update / gib_vector_slice / gib_expll).  The RTS object is
-- built once WITHOUT -DGIBBON_INT32, so under --int32 the generated TU and the
-- linked RTS disagree on the width of every GibInt in these signatures.
gibbon_main =
  let v0 :: Vector Int
      v0 = valloc 5
      v1 = inplacevupdate v0 0 10
      v2 = inplacevupdate v1 1 20
      v3 = inplacevupdate v2 2 30
      v4 = inplacevupdate v3 3 40
      v5 = inplacevupdate v4 4 50
      n  = vlength v5
      a  = vnth v5 0
      b  = vnth v5 4
      s  = vslice 1 3 v5
      m  = vlength s
      c  = vnth s 0
      e  = 2 ^ 10
      f  = 3 ^ 4
  in (n, a, b, m, c, e, f)
