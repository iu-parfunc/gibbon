-- MapIntensityV2: L (Linear).
-- Functions: mkL, mapSer1, mapSer2, mapSer4, mapSer8, mapSer16, mapChain1,
-- mapChain2. ...
-- Annotated: MayVectorize on mapSer1, mapSer2, mapSer4, mapSer8, mapSer16,
-- mapChain1, mapChain2, mapChain4, mapChain8, mapPar1, mapPar2, mapPar4,
-- mapPar8, mapPar16.
data L = C Int Int L | Nil
{-# ANN type L "Linear" #-}

mkL :: Int -> L
mkL n =
  if n <= 0
  then Nil
  else let rst = mkL (n - 1)
       in C n (n + 7) rst

{-# ANN mapSer1 "OPT:MayVectorize" #-}
-- 1 multiplies, one serial chain.  ILP 1 -- latency ceiling.
mapSer1 :: L -> Int -> L
mapSer1 xs m =
  case xs of
    Nil -> Nil
    C i j rst ->
      let
          x1 = i * m + 1
      in C x1 j (mapSer1 rst m)

{-# ANN mapSer2 "OPT:MayVectorize" #-}
-- 2 multiplies, one serial chain.  ILP 1 -- latency ceiling.
mapSer2 :: L -> Int -> L
mapSer2 xs m =
  case xs of
    Nil -> Nil
    C i j rst ->
      let
          x1 = i * m + 1
          x2 = x1 * m + 5
      in C x2 j (mapSer2 rst m)

{-# ANN mapSer4 "OPT:MayVectorize" #-}
-- 4 multiplies, one serial chain.  ILP 1 -- latency ceiling.
mapSer4 :: L -> Int -> L
mapSer4 xs m =
  case xs of
    Nil -> Nil
    C i j rst ->
      let
          x1 = i * m + 1
          x2 = x1 * m + 5
          x3 = x2 * m + 7
          x4 = x3 * m + 9
      in C x4 j (mapSer4 rst m)

{-# ANN mapSer8 "OPT:MayVectorize" #-}
-- 8 multiplies, one serial chain.  ILP 1 -- latency ceiling.
mapSer8 :: L -> Int -> L
mapSer8 xs m =
  case xs of
    Nil -> Nil
    C i j rst ->
      let
          x1 = i * m + 1
          x2 = x1 * m + 5
          x3 = x2 * m + 7
          x4 = x3 * m + 9
          x5 = x4 * m + 11
          x6 = x5 * m + 13
          x7 = x6 * m + 15
          x8 = x7 * m + 17
      in C x8 j (mapSer8 rst m)

{-# ANN mapSer16 "OPT:MayVectorize" #-}
-- 16 multiplies, one serial chain.  ILP 1 -- latency ceiling.
mapSer16 :: L -> Int -> L
mapSer16 xs m =
  case xs of
    Nil -> Nil
    C i j rst ->
      let
          x1 = i * m + 1
          x2 = x1 * m + 5
          x3 = x2 * m + 7
          x4 = x3 * m + 9
          x5 = x4 * m + 11
          x6 = x5 * m + 13
          x7 = x6 * m + 15
          x8 = x7 * m + 17
          x9 = x8 * m + 19
          x10 = x9 * m + 21
          x11 = x10 * m + 23
          x12 = x11 * m + 25
          x13 = x12 * m + 27
          x14 = x13 * m + 29
          x15 = x14 * m + 31
          x16 = x15 * m + 33
      in C x16 j (mapSer16 rst m)

{-# ANN mapChain1 "OPT:MayVectorize" #-}
-- 4 multiplies: 1 seed + 2 chains of depth 1 + 1 closing product.
-- ILP 2.  The `i * i` seed is non-linear, so chain b cannot be CSE'd
-- against chain a; the closing product keeps the combination quadratic.
mapChain1 :: L -> Int -> L
mapChain1 xs m =
  case xs of
    Nil -> Nil
    C i j rst ->
      let
          b0 = i * i
          a1 = i * m + 1
          b1 = b0 * m + 5
          r = a1 * b1
      in C r j (mapChain1 rst m)

{-# ANN mapChain2 "OPT:MayVectorize" #-}
-- 6 multiplies: 1 seed + 2 chains of depth 2 + 1 closing product.
-- ILP 2.  The `i * i` seed is non-linear, so chain b cannot be CSE'd
-- against chain a; the closing product keeps the combination quadratic.
mapChain2 :: L -> Int -> L
mapChain2 xs m =
  case xs of
    Nil -> Nil
    C i j rst ->
      let
          b0 = i * i
          a1 = i * m + 1
          b1 = b0 * m + 5
          a2 = a1 * m + 7
          b2 = b1 * m + 9
          r = a2 * b2
      in C r j (mapChain2 rst m)

{-# ANN mapChain4 "OPT:MayVectorize" #-}
-- 10 multiplies: 1 seed + 2 chains of depth 4 + 1 closing product.
-- ILP 2.  The `i * i` seed is non-linear, so chain b cannot be CSE'd
-- against chain a; the closing product keeps the combination quadratic.
mapChain4 :: L -> Int -> L
mapChain4 xs m =
  case xs of
    Nil -> Nil
    C i j rst ->
      let
          b0 = i * i
          a1 = i * m + 1
          b1 = b0 * m + 5
          a2 = a1 * m + 7
          b2 = b1 * m + 9
          a3 = a2 * m + 11
          b3 = b2 * m + 13
          a4 = a3 * m + 15
          b4 = b3 * m + 17
          r = a4 * b4
      in C r j (mapChain4 rst m)

{-# ANN mapChain8 "OPT:MayVectorize" #-}
-- 18 multiplies: 1 seed + 2 chains of depth 8 + 1 closing product.
-- ILP 2.  The `i * i` seed is non-linear, so chain b cannot be CSE'd
-- against chain a; the closing product keeps the combination quadratic.
mapChain8 :: L -> Int -> L
mapChain8 xs m =
  case xs of
    Nil -> Nil
    C i j rst ->
      let
          b0 = i * i
          a1 = i * m + 1
          b1 = b0 * m + 5
          a2 = a1 * m + 7
          b2 = b1 * m + 9
          a3 = a2 * m + 11
          b3 = b2 * m + 13
          a4 = a3 * m + 15
          b4 = b3 * m + 17
          a5 = a4 * m + 19
          b5 = b4 * m + 21
          a6 = a5 * m + 23
          b6 = b5 * m + 25
          a7 = a6 * m + 27
          b7 = b6 * m + 29
          a8 = a7 * m + 31
          b8 = b7 * m + 33
          r = a8 * b8
      in C r j (mapChain8 rst m)

{-# ANN mapPar1 "OPT:MayVectorize" #-}
-- CONTROL, NOT AN INTENSITY POINT.  sum_k (i + c_k) * m distributes to
-- m * (1*i + sum c_k); the scalar build emits ONE imul regardless of N.
mapPar1 :: L -> Int -> L
mapPar1 xs m =
  case xs of
    Nil -> Nil
    C i j rst ->
      let
          p1 = (i + 1) * m
      in C p1 j (mapPar1 rst m)

{-# ANN mapPar2 "OPT:MayVectorize" #-}
-- CONTROL, NOT AN INTENSITY POINT.  sum_k (i + c_k) * m distributes to
-- m * (2*i + sum c_k); the scalar build emits ONE imul regardless of N.
mapPar2 :: L -> Int -> L
mapPar2 xs m =
  case xs of
    Nil -> Nil
    C i j rst ->
      let
          p1 = (i + 1) * m
          p2 = (i + 3) * m
          s2 = p1 + p2
      in C s2 j (mapPar2 rst m)

{-# ANN mapPar4 "OPT:MayVectorize" #-}
-- CONTROL, NOT AN INTENSITY POINT.  sum_k (i + c_k) * m distributes to
-- m * (4*i + sum c_k); the scalar build emits ONE imul regardless of N.
mapPar4 :: L -> Int -> L
mapPar4 xs m =
  case xs of
    Nil -> Nil
    C i j rst ->
      let
          p1 = (i + 1) * m
          p2 = (i + 3) * m
          p3 = (i + 5) * m
          p4 = (i + 7) * m
          s2 = p1 + p2
          s3 = s2 + p3
          s4 = s3 + p4
      in C s4 j (mapPar4 rst m)

{-# ANN mapPar8 "OPT:MayVectorize" #-}
-- CONTROL, NOT AN INTENSITY POINT.  sum_k (i + c_k) * m distributes to
-- m * (8*i + sum c_k); the scalar build emits ONE imul regardless of N.
mapPar8 :: L -> Int -> L
mapPar8 xs m =
  case xs of
    Nil -> Nil
    C i j rst ->
      let
          p1 = (i + 1) * m
          p2 = (i + 3) * m
          p3 = (i + 5) * m
          p4 = (i + 7) * m
          p5 = (i + 9) * m
          p6 = (i + 11) * m
          p7 = (i + 13) * m
          p8 = (i + 15) * m
          s2 = p1 + p2
          s3 = s2 + p3
          s4 = s3 + p4
          s5 = s4 + p5
          s6 = s5 + p6
          s7 = s6 + p7
          s8 = s7 + p8
      in C s8 j (mapPar8 rst m)

{-# ANN mapPar16 "OPT:MayVectorize" #-}
-- CONTROL, NOT AN INTENSITY POINT.  sum_k (i + c_k) * m distributes to
-- m * (16*i + sum c_k); the scalar build emits ONE imul regardless of N.
mapPar16 :: L -> Int -> L
mapPar16 xs m =
  case xs of
    Nil -> Nil
    C i j rst ->
      let
          p1 = (i + 1) * m
          p2 = (i + 3) * m
          p3 = (i + 5) * m
          p4 = (i + 7) * m
          p5 = (i + 9) * m
          p6 = (i + 11) * m
          p7 = (i + 13) * m
          p8 = (i + 15) * m
          p9 = (i + 17) * m
          p10 = (i + 19) * m
          p11 = (i + 21) * m
          p12 = (i + 23) * m
          p13 = (i + 25) * m
          p14 = (i + 27) * m
          p15 = (i + 29) * m
          p16 = (i + 31) * m
          s2 = p1 + p2
          s3 = s2 + p3
          s4 = s3 + p4
          s5 = s4 + p5
          s6 = s5 + p6
          s7 = s6 + p7
          s8 = s7 + p8
          s9 = s8 + p9
          s10 = s9 + p10
          s11 = s10 + p11
          s12 = s11 + p12
          s13 = s12 + p13
          s14 = s13 + p14
          s15 = s14 + p15
          s16 = s15 + p16
      in C s16 j (mapPar16 rst m)

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
  let _  = printsym (quote "Running program MapIntensityV2: ")
      _  = printsym (quote "NEWLINE")
      n  = 10000000
      m  = 2 * sizeParam + 3
      xs = mkL n

      _  = printsym (quote "Running pass mapSer1 (map, uses=1, ilp=1, shared=1): ")
      _  = printsym (quote "NEWLINE")
      rser1 = iterate (mapSer1 xs m)
      _  = printsym (quote "End")
      _  = printsym (quote "NEWLINE")

      _  = printsym (quote "Running pass mapSer2 (map, uses=1, ilp=1, shared=1): ")
      _  = printsym (quote "NEWLINE")
      rser2 = iterate (mapSer2 xs m)
      _  = printsym (quote "End")
      _  = printsym (quote "NEWLINE")

      _  = printsym (quote "Running pass mapSer4 (map, uses=1, ilp=1, shared=1): ")
      _  = printsym (quote "NEWLINE")
      rser4 = iterate (mapSer4 xs m)
      _  = printsym (quote "End")
      _  = printsym (quote "NEWLINE")

      _  = printsym (quote "Running pass mapSer8 (map, uses=1, ilp=1, shared=1): ")
      _  = printsym (quote "NEWLINE")
      rser8 = iterate (mapSer8 xs m)
      _  = printsym (quote "End")
      _  = printsym (quote "NEWLINE")

      _  = printsym (quote "Running pass mapSer16 (map, uses=1, ilp=1, shared=1): ")
      _  = printsym (quote "NEWLINE")
      rser16 = iterate (mapSer16 xs m)
      _  = printsym (quote "End")
      _  = printsym (quote "NEWLINE")

      _  = printsym (quote "Running pass mapChain1 (map, uses=1, ilp=2, shared=1): ")
      _  = printsym (quote "NEWLINE")
      rchain1 = iterate (mapChain1 xs m)
      _  = printsym (quote "End")
      _  = printsym (quote "NEWLINE")

      _  = printsym (quote "Running pass mapChain2 (map, uses=1, ilp=2, shared=1): ")
      _  = printsym (quote "NEWLINE")
      rchain2 = iterate (mapChain2 xs m)
      _  = printsym (quote "End")
      _  = printsym (quote "NEWLINE")

      _  = printsym (quote "Running pass mapChain4 (map, uses=1, ilp=2, shared=1): ")
      _  = printsym (quote "NEWLINE")
      rchain4 = iterate (mapChain4 xs m)
      _  = printsym (quote "End")
      _  = printsym (quote "NEWLINE")

      _  = printsym (quote "Running pass mapChain8 (map, uses=1, ilp=2, shared=1): ")
      _  = printsym (quote "NEWLINE")
      rchain8 = iterate (mapChain8 xs m)
      _  = printsym (quote "End")
      _  = printsym (quote "NEWLINE")

      _  = printsym (quote "Running pass mapPar1 (map, uses=1, ilp=1, shared=1): ")
      _  = printsym (quote "NEWLINE")
      rpar1 = iterate (mapPar1 xs m)
      _  = printsym (quote "End")
      _  = printsym (quote "NEWLINE")

      _  = printsym (quote "Running pass mapPar2 (map, uses=1, ilp=1, shared=1): ")
      _  = printsym (quote "NEWLINE")
      rpar2 = iterate (mapPar2 xs m)
      _  = printsym (quote "End")
      _  = printsym (quote "NEWLINE")

      _  = printsym (quote "Running pass mapPar4 (map, uses=1, ilp=1, shared=1): ")
      _  = printsym (quote "NEWLINE")
      rpar4 = iterate (mapPar4 xs m)
      _  = printsym (quote "End")
      _  = printsym (quote "NEWLINE")

      _  = printsym (quote "Running pass mapPar8 (map, uses=1, ilp=1, shared=1): ")
      _  = printsym (quote "NEWLINE")
      rpar8 = iterate (mapPar8 xs m)
      _  = printsym (quote "End")
      _  = printsym (quote "NEWLINE")

      _  = printsym (quote "Running pass mapPar16 (map, uses=1, ilp=1, shared=1): ")
      _  = printsym (quote "NEWLINE")
      rpar16 = iterate (mapPar16 xs m)
      _  = printsym (quote "End")
      _  = printsym (quote "NEWLINE")

      -- Consume every result so no pass can be eliminated as dead.
      crser1 = sumFirst rser1
      crser2 = sumFirst rser2
      crser4 = sumFirst rser4
      crser8 = sumFirst rser8
      crser16 = sumFirst rser16
      crchain1 = sumFirst rchain1
      crchain2 = sumFirst rchain2
      crchain4 = sumFirst rchain4
      crchain8 = sumFirst rchain8
      crpar1 = sumFirst rpar1
      crpar2 = sumFirst rpar2
      crpar4 = sumFirst rpar4
      crpar8 = sumFirst rpar8
      crpar16 = sumFirst rpar16
      ln = lenL rpar16
  in (crser1, crser2, crser4, crser8, crser16, crchain1, crchain2, crchain4, crchain8, crpar1, crpar2, crpar4, crpar8, crpar16, ln)
