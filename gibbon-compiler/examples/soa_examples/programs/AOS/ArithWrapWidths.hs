-- ArithWrapWidths.
-- Functions: wrap8, wrap16, wrap32, wrap64.

wrap8 :: Int8 -> ()
wrap8 z =
  let hi = z + 127
      lo = z - 127 - 1
      _a = printint (hi + 1)
      _b = printint (lo - 1)
      _c = printint (1 - lo)
      _d = printint (lo * (z - 1))
      _e = printint (hi * (z + 2))
      _f = printint (hi * hi)
      _g = printint (lo * lo)
      _h = printint (z - lo)
      _i = printint ((z - 3) * (z - 4))
      _j = printint ((z - 3) * (z + 4))
  in ()

wrap16 :: Int16 -> ()
wrap16 z =
  let hi = z + 32767
      lo = z - 32767 - 1
      _a = printint (hi + 1)
      _b = printint (lo - 1)
      _c = printint (1 - lo)
      _d = printint (lo * (z - 1))
      _e = printint (hi * (z + 2))
      _f = printint (hi * hi)
      _g = printint (lo * lo)
      _h = printint (z - lo)
      _i = printint ((z - 3) * (z - 4))
      _j = printint ((z - 3) * (z + 4))
  in ()

wrap32 :: Int32 -> ()
wrap32 z =
  let hi = z + 2147483647
      lo = z - 2147483647 - 1
      _a = printint (hi + 1)
      _b = printint (lo - 1)
      _c = printint (1 - lo)
      _d = printint (lo * (z - 1))
      _e = printint (hi * (z + 2))
      _f = printint (hi * hi)
      _g = printint (lo * lo)
      _h = printint (z - lo)
      _i = printint ((z - 3) * (z - 4))
      _j = printint ((z - 3) * (z + 4))
  in ()

wrap64 :: Int64 -> ()
wrap64 z =
  let hi = z + 9223372036854775807
      lo = z - 9223372036854775807 - 1
      _a = printint (hi + 1)
      _b = printint (lo - 1)
      _c = printint (1 - lo)
      _d = printint (lo * (z - 1))
      _e = printint (hi * (z + 2))
      _f = printint (hi * hi)
      _g = printint (lo * lo)
      _h = printint (z - lo)
      _i = printint ((z - 3) * (z - 4))
      _j = printint ((z - 3) * (z + 4))
  in ()

gibbon_main =
  let _a = wrap8 0
      _b = wrap16 0
      _c = wrap32 0
      _d = wrap64 0
  in 0
