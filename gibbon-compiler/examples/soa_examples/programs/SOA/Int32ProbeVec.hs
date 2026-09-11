-- Int32ProbeVec.
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
