-- MapIntensity: L (Linear).
-- Functions: mkL, mapAI0, mapAI1, mapAI2, mapAI4, mapAI8, mapAI16, sumFirst.
-- ...
-- Annotated: MayVectorize on mapAI0, mapAI1, mapAI2, mapAI4, mapAI8, mapAI16.
data L = C Int Int L | Nil
{-# ANN type L "Linear" #-}

mkL :: Int -> L
mkL n =
  if n <= 0
  then Nil
  else let rst = mkL (n - 1)
       in C n (n + 7) rst

-- N = 0: pure copy.  Establishes the bandwidth floor -- the time to move the
-- bytes with no arithmetic at all.
{-# ANN mapAI0 "OPT:MayVectorize" #-}
mapAI0 :: L -> Int -> L
mapAI0 xs k =
  case xs of
    Nil -> Nil
    C i j rst -> C i j (mapAI0 rst k)

-- N = 1: one fused multiply-add.  This is the add1-class kernel.
{-# ANN mapAI1 "OPT:MayVectorize" #-}
mapAI1 :: L -> Int -> L
mapAI1 xs k =
  case xs of
    Nil -> Nil
    C i j rst ->
      let i1 = i * k + 1
      in C i1 j (mapAI1 rst k)

{-# ANN mapAI2 "OPT:MayVectorize" #-}
mapAI2 :: L -> Int -> L
mapAI2 xs k =
  case xs of
    Nil -> Nil
    C i j rst ->
      let a = i * k + 1
          b = a * 3 + 5
      in C b j (mapAI2 rst k)

{-# ANN mapAI4 "OPT:MayVectorize" #-}
mapAI4 :: L -> Int -> L
mapAI4 xs k =
  case xs of
    Nil -> Nil
    C i j rst ->
      let a = i * k + 1
          b = a * 3 + 5
          c = b * 7 + 9
          d = c * 11 + 13
      in C d j (mapAI4 rst k)

{-# ANN mapAI8 "OPT:MayVectorize" #-}
mapAI8 :: L -> Int -> L
mapAI8 xs k =
  case xs of
    Nil -> Nil
    C i j rst ->
      let a = i * k + 1
          b = a * 3 + 5
          c = b * 7 + 9
          d = c * 11 + 13
          e = d * 15 + 17
          f = e * 19 + 21
          g = f * 23 + 25
          h = g * 27 + 29
      in C h j (mapAI8 rst k)

{-# ANN mapAI16 "OPT:MayVectorize" #-}
mapAI16 :: L -> Int -> L
mapAI16 xs k =
  case xs of
    Nil -> Nil
    C i j rst ->
      let a = i * k + 1
          b = a * 3 + 5
          c = b * 7 + 9
          d = c * 11 + 13
          e = d * 15 + 17
          f = e * 19 + 21
          g = f * 23 + 25
          h = g * 27 + 29
          p = h * 31 + 33
          q = p * 35 + 37
          r = q * 39 + 41
          s = r * 43 + 45
          t = s * 47 + 49
          u = t * 51 + 53
          v = u * 55 + 57
          w = v * 59 + 61
      in C w j (mapAI16 rst k)

sumFirst :: L -> Int
sumFirst xs =
  case xs of
    Nil -> 0
    C i _ rst -> i + sumFirst rst

lenL :: L -> Int
lenL xs =
  case xs of
    Nil -> 0
    C _ _ rst -> 1 + lenL rst

gibbon_main =
  let _  = printsym (quote "Running program MapIntensity: ")
      _  = printsym (quote "NEWLINE")
      n  = 10000000
      k  = sizeParam + 3
      xs = mkL n

      _  = printsym (quote "Running pass mapAI0 (map, uses=1, shared=2): ")
      _  = printsym (quote "NEWLINE")
      a0 = iterate (mapAI0 xs k)
      _  = printsym (quote "End")
      _  = printsym (quote "NEWLINE")

      _  = printsym (quote "Running pass mapAI1 (map, uses=1, shared=1): ")
      _  = printsym (quote "NEWLINE")
      a1 = iterate (mapAI1 xs k)
      _  = printsym (quote "End")
      _  = printsym (quote "NEWLINE")

      _  = printsym (quote "Running pass mapAI2 (map, uses=1, shared=1): ")
      _  = printsym (quote "NEWLINE")
      a2 = iterate (mapAI2 xs k)
      _  = printsym (quote "End")
      _  = printsym (quote "NEWLINE")

      _  = printsym (quote "Running pass mapAI4 (map, uses=1, shared=1): ")
      _  = printsym (quote "NEWLINE")
      a4 = iterate (mapAI4 xs k)
      _  = printsym (quote "End")
      _  = printsym (quote "NEWLINE")

      _  = printsym (quote "Running pass mapAI8 (map, uses=1, shared=1): ")
      _  = printsym (quote "NEWLINE")
      a8 = iterate (mapAI8 xs k)
      _  = printsym (quote "End")
      _  = printsym (quote "NEWLINE")

      _  = printsym (quote "Running pass mapAI16 (map, uses=1, shared=1): ")
      _  = printsym (quote "NEWLINE")
      a16 = iterate (mapAI16 xs k)
      _  = printsym (quote "End")
      _  = printsym (quote "NEWLINE")

      -- Consume every result so no pass can be eliminated as dead.
      c0 = sumFirst a0
      c1 = sumFirst a1
      c2 = sumFirst a2
      c4 = sumFirst a4
      c8 = sumFirst a8
      c16 = sumFirst a16
      ln = lenL a16
  in (c0, c1, c2, c4, c8, c16, ln)
