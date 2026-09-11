-- ArithConstFold.
-- Functions: cAdd8, cSub8, cMul8, cDiv8, cMod8, cExp8, pair8, cAdd16. ...

cAdd8 :: Int8
cAdd8 = 127 + 1
cSub8 :: Int8
cSub8 = (-128) - 1
cMul8 :: Int8
cMul8 = (-128) * (0 - 1)
cDiv8 :: Int8
cDiv8 = (-128) / (0 - 1)
cMod8 :: Int8
cMod8 = mod (-128) (0 - 1)
cExp8 :: Int8
cExp8 = 2 ^ 31

pair8 :: Int8 -> ()
pair8 z =
  let hi = z + 127
      lo = z + (-128)
      one = z + 1
      m1 = z - 1
      two = z + 2
      e31 = z + 31
      _a1 = printint cAdd8
      _a2 = printint (hi + one)
      _b1 = printint cSub8
      _b2 = printint (lo - one)
      _c1 = printint cMul8
      _c2 = printint (lo * m1)
      _d1 = printint cDiv8
      _d2 = printint (lo / m1)
      _e1 = printint cMod8
      _e2 = printint (mod lo m1)
      _f1 = printint cExp8
      _f2 = printint (two ^ e31)
  in ()

cAdd16 :: Int16
cAdd16 = 32767 + 1
cSub16 :: Int16
cSub16 = (-32768) - 1
cMul16 :: Int16
cMul16 = (-32768) * (0 - 1)
cDiv16 :: Int16
cDiv16 = (-32768) / (0 - 1)
cMod16 :: Int16
cMod16 = mod (-32768) (0 - 1)
cExp16 :: Int16
cExp16 = 2 ^ 31

pair16 :: Int16 -> ()
pair16 z =
  let hi = z + 32767
      lo = z + (-32768)
      one = z + 1
      m1 = z - 1
      two = z + 2
      e31 = z + 31
      _a1 = printint cAdd16
      _a2 = printint (hi + one)
      _b1 = printint cSub16
      _b2 = printint (lo - one)
      _c1 = printint cMul16
      _c2 = printint (lo * m1)
      _d1 = printint cDiv16
      _d2 = printint (lo / m1)
      _e1 = printint cMod16
      _e2 = printint (mod lo m1)
      _f1 = printint cExp16
      _f2 = printint (two ^ e31)
  in ()

cAdd32 :: Int32
cAdd32 = 2147483647 + 1
cSub32 :: Int32
cSub32 = (-2147483648) - 1
cMul32 :: Int32
cMul32 = (-2147483648) * (0 - 1)
cDiv32 :: Int32
cDiv32 = (-2147483648) / (0 - 1)
cMod32 :: Int32
cMod32 = mod (-2147483648) (0 - 1)
cExp32 :: Int32
cExp32 = 2 ^ 31

pair32 :: Int32 -> ()
pair32 z =
  let hi = z + 2147483647
      lo = z + (-2147483648)
      one = z + 1
      m1 = z - 1
      two = z + 2
      e31 = z + 31
      _a1 = printint cAdd32
      _a2 = printint (hi + one)
      _b1 = printint cSub32
      _b2 = printint (lo - one)
      _c1 = printint cMul32
      _c2 = printint (lo * m1)
      _d1 = printint cDiv32
      _d2 = printint (lo / m1)
      _e1 = printint cMod32
      _e2 = printint (mod lo m1)
      _f1 = printint cExp32
      _f2 = printint (two ^ e31)
  in ()

cAdd64 :: Int64
cAdd64 = 9223372036854775807 + 1
cSub64 :: Int64
cSub64 = (-9223372036854775808) - 1
cMul64 :: Int64
cMul64 = (-9223372036854775808) * (0 - 1)
cDiv64 :: Int64
cDiv64 = (-9223372036854775808) / (0 - 1)
cMod64 :: Int64
cMod64 = mod (-9223372036854775808) (0 - 1)
cExp64 :: Int64
cExp64 = 2 ^ 31

pair64 :: Int64 -> ()
pair64 z =
  let hi = z + 9223372036854775807
      lo = z + (-9223372036854775808)
      one = z + 1
      m1 = z - 1
      two = z + 2
      e31 = z + 31
      _a1 = printint cAdd64
      _a2 = printint (hi + one)
      _b1 = printint cSub64
      _b2 = printint (lo - one)
      _c1 = printint cMul64
      _c2 = printint (lo * m1)
      _d1 = printint cDiv64
      _d2 = printint (lo / m1)
      _e1 = printint cMod64
      _e2 = printint (mod lo m1)
      _f1 = printint cExp64
      _f2 = printint (two ^ e31)
  in ()

gibbon_main =
  let _a = pair8 0
      _b = pair16 0
      _c = pair32 0
      _d = pair64 0
  in 0
