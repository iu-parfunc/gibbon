-- ArithExpWidths.
-- Functions: pow8, pow16, pow32, pow64.

pow8 :: Int8 -> ()
pow8 z =
  let two = z + 2
      three = z + 3
      _a = printint (three ^ (z + 0))
      _b = printint ((z + 0) ^ (z + 0))
      _c = printint (three ^ (z + 3))
      _d = printint ((z - 3) ^ (z + 3))
      _e = printint ((z - 3) ^ (z + 2))
      _f = printint (two ^ (z + 7))
      _g = printint (two ^ (z + 8))
      _h = printint (two ^ (z + 15))
      _i = printint (two ^ (z + 16))
      _j = printint (two ^ (z + 31))
      _k = printint (three ^ (z + 20))
      _l = printint (three ^ (z - 1))
      _m = printint (two ^ (z - 1))
  in ()

pow16 :: Int16 -> ()
pow16 z =
  let two = z + 2
      three = z + 3
      _a = printint (three ^ (z + 0))
      _b = printint ((z + 0) ^ (z + 0))
      _c = printint (three ^ (z + 3))
      _d = printint ((z - 3) ^ (z + 3))
      _e = printint ((z - 3) ^ (z + 2))
      _f = printint (two ^ (z + 7))
      _g = printint (two ^ (z + 8))
      _h = printint (two ^ (z + 15))
      _i = printint (two ^ (z + 16))
      _j = printint (two ^ (z + 31))
      _k = printint (three ^ (z + 20))
      _l = printint (three ^ (z - 1))
      _m = printint (two ^ (z - 1))
  in ()

pow32 :: Int32 -> ()
pow32 z =
  let two = z + 2
      three = z + 3
      _a = printint (three ^ (z + 0))
      _b = printint ((z + 0) ^ (z + 0))
      _c = printint (three ^ (z + 3))
      _d = printint ((z - 3) ^ (z + 3))
      _e = printint ((z - 3) ^ (z + 2))
      _f = printint (two ^ (z + 7))
      _g = printint (two ^ (z + 8))
      _h = printint (two ^ (z + 15))
      _i = printint (two ^ (z + 16))
      _j = printint (two ^ (z + 31))
      _k = printint (three ^ (z + 20))
      _l = printint (three ^ (z - 1))
      _m = printint (two ^ (z - 1))
  in ()

pow64 :: Int64 -> ()
pow64 z =
  let two = z + 2
      three = z + 3
      _a = printint (three ^ (z + 0))
      _b = printint ((z + 0) ^ (z + 0))
      _c = printint (three ^ (z + 3))
      _d = printint ((z - 3) ^ (z + 3))
      _e = printint ((z - 3) ^ (z + 2))
      _f = printint (two ^ (z + 7))
      _g = printint (two ^ (z + 8))
      _h = printint (two ^ (z + 15))
      _i = printint (two ^ (z + 16))
      _j = printint (two ^ (z + 31))
      _k = printint (three ^ (z + 20))
      _l = printint (three ^ (z - 1))
      _m = printint (two ^ (z - 1))
  in ()

gibbon_main =
  let _a = pow8 0
      _b = pow16 0
      _c = pow32 0
      _d = pow64 0
  in 0
