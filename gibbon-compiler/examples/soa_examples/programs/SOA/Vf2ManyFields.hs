-- Variant reproducer: 12 scalar fields (buffer suffixes _buf0.._buf11) so any
-- name-suffix matching bug between e.g. _buf1_ and _buf11_ shows up as a
-- cross-field value corruption.  Distinct multipliers make every field's
-- contribution unique.
data W = WCons Int Int Int Int Int Int Int Int Int Int Int Int W | WNil
{-# ANN type W "Factored" #-}

{-# ANN mkW "OPT:StoreScalarCounts" #-}
mkW :: Int -> W
mkW n =
  if n <= 0
  then WNil
  else let rst = mkW (n - 1)
       in WCons n (n+1) (n+2) (n+3) (n+4) (n+5) (n+6) (n+7) (n+8) (n+9) (n+10) (n+11) rst

{-# ANN mapW "OPT:CanVectorize" #-}
mapW :: W -> Int -> W
mapW xs k =
  case xs of
    WNil -> WNil
    WCons a b c d e f g h i j l m rst ->
      WCons (a+(k*1)) (b+(k*2)) (c+(k*3)) (d+(k*4)) (e+(k*5)) (f+(k*6))
            (g+(k*7)) (h+(k*8)) (i+(k*9)) (j+(k*10)) (l+(k*11)) (m+(k*12))
            (mapW rst k)

digest :: W -> Int -> Int
digest xs p =
  case xs of
    WNil -> 0
    WCons a b c d e f g h i j l m rst ->
      ((a*1)+(b*3)+(c*7)+(d*13)+(e*23)+(f*41)+(g*67)+(h*101)+(i*151)+(j*211)+(l*281)+(m*367))*(p+1)
        + digest rst (p+1)

gibbon_main =
  let n0 = sizeParam
      xs = mkW n0
      ys = mapW xs 5
  in digest ys 0
