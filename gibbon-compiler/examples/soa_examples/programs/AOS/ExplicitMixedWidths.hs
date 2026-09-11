-- Explicit-width coverage (Int8/Int16/Int32/Int64 in one packed AoS
-- constructor): boundary literals, homogeneous arithmetic per width,
-- an Int8 comparison, manual per-field printing, and the generated
-- (printPacked) datatype printer, all on the same mixed-width value.
data Mixed = Mixed Int8 Int16 Int32 Int64

mkMixed :: Int8 -> Mixed
mkMixed a = Mixed a 300 70000 5000000000

bumpMixed :: Mixed -> Mixed
bumpMixed m =
  case m of
    Mixed a b c d -> Mixed (a + 1) (b + 2) (c + 3) (d + 4)

printMixedFields :: Mixed -> ()
printMixedFields m =
  case m of
    Mixed a b c d ->
      let _u1 = printint a
          _u2 = printint b
          _u3 = printint c
          _u4 = printint d
      in ()

printBoundary8 :: Int8 -> Int8 -> ()
printBoundary8 lo hi =
  let _u1 = printint lo
      _u2 = printint hi
  in ()

printBoundary32 :: Int32 -> Int32 -> ()
printBoundary32 lo hi =
  let _u1 = printint lo
      _u2 = printint hi
  in ()

cmp8 :: Int8 -> Int8 -> Bool
cmp8 a b = a < b

gibbon_main =
  let m = bumpMixed (mkMixed 3)
      _u1 = printMixedFields m
      _u2 = printPacked m
      _u3 = printBoundary8 (-128) 127
      _u4 = printBoundary32 (-2147483648) 2147483647
      c = cmp8 3 5
  in c
