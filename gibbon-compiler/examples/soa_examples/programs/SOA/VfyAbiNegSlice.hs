-- VfyAbiNegSlice.
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
