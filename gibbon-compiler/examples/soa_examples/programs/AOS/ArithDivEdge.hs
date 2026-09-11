-- ArithDivEdge.
-- Functions: signMatrix8, signMatrix16, signMatrix32, signMatrix64, edge8,
-- edge16, edge32, edge64.

signMatrix8 :: Int8 -> ()
signMatrix8 z =
  let _a = printint ((z + 7) / (z + 3))
      _b = printint (mod (z + 7) (z + 3))
      _c = printint ((z - 7) / (z + 3))
      _d = printint (mod (z - 7) (z + 3))
      _e = printint ((z + 7) / (z - 3))
      _f = printint (mod (z + 7) (z - 3))
      _g = printint ((z - 7) / (z - 3))
      _h = printint (mod (z - 7) (z - 3))
  in ()

signMatrix16 :: Int16 -> ()
signMatrix16 z =
  let _a = printint ((z + 7) / (z + 3))
      _b = printint (mod (z + 7) (z + 3))
      _c = printint ((z - 7) / (z + 3))
      _d = printint (mod (z - 7) (z + 3))
      _e = printint ((z + 7) / (z - 3))
      _f = printint (mod (z + 7) (z - 3))
      _g = printint ((z - 7) / (z - 3))
      _h = printint (mod (z - 7) (z - 3))
  in ()

signMatrix32 :: Int32 -> ()
signMatrix32 z =
  let _a = printint ((z + 7) / (z + 3))
      _b = printint (mod (z + 7) (z + 3))
      _c = printint ((z - 7) / (z + 3))
      _d = printint (mod (z - 7) (z + 3))
      _e = printint ((z + 7) / (z - 3))
      _f = printint (mod (z + 7) (z - 3))
      _g = printint ((z - 7) / (z - 3))
      _h = printint (mod (z - 7) (z - 3))
  in ()

signMatrix64 :: Int64 -> ()
signMatrix64 z =
  let _a = printint ((z + 7) / (z + 3))
      _b = printint (mod (z + 7) (z + 3))
      _c = printint ((z - 7) / (z + 3))
      _d = printint (mod (z - 7) (z + 3))
      _e = printint ((z + 7) / (z - 3))
      _f = printint (mod (z + 7) (z - 3))
      _g = printint ((z - 7) / (z - 3))
      _h = printint (mod (z - 7) (z - 3))
  in ()

edge8 :: Int8 -> ()
edge8 z =
  let lo = z - 127 - 1
      m1 = z - 1
      _a = printint (lo / m1)
      _b = printint (mod lo m1)
  in ()

edge16 :: Int16 -> ()
edge16 z =
  let lo = z - 32767 - 1
      m1 = z - 1
      _a = printint (lo / m1)
      _b = printint (mod lo m1)
  in ()

edge32 :: Int32 -> ()
edge32 z =
  let lo = z - 2147483647 - 1
      m1 = z - 1
      _a = printint (lo / m1)
      _b = printint (mod lo m1)
  in ()

edge64 :: Int64 -> ()
edge64 z =
  let lo = z - 9223372036854775807 - 1
      m1 = z - 1
      _a = printint (lo / m1)
      _b = printint (mod lo m1)
  in ()

gibbon_main =
  let _a = signMatrix8 0
      _b = signMatrix16 0
      _c = signMatrix32 0
      _d = signMatrix64 0
      _e = edge8 0
      _f = edge16 0
      _g = edge32 0
      _h = edge64 0
  in 0
