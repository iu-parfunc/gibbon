-- Int32VecArith: AList (Factored).
-- Functions: mkAList, mapArith, digest, sumA, sumB, sumC.
-- Annotated: MayVectorize on mapArith; StoreScalarCounts on mkAList.
data AList = ACons Int Int Int AList | ANil
{-# ANN type AList "Factored" #-}

{-# ANN mkAList "OPT:StoreScalarCounts" #-}
mkAList :: Int -> AList
mkAList n =
  if n <= 0
  then ANil
  else let rst = mkAList (n - 1)
           s = if (mod n 2) == 0 then n else 0 - n
       in ACons (s * 7) (s * 13) (s + 5) rst

{-# ANN mapArith "OPT:MayVectorize" #-}
mapArith :: AList -> Int -> AList
mapArith xs k =
  case xs of
    ANil -> ANil
    ACons a b c rst -> ACons (a * k) (b / k) (mod c k) (mapArith rst k)

-- Position-weighted digest: a per-element difference cannot cancel out the way
-- a plain sum can.
digest :: AList -> Int -> Int
digest xs i =
  case xs of
    ANil -> 0
    ACons a b c rst ->
      ((((a * 11) + (b * 101)) + (c * 1009)) * (i + 1)) + digest rst (i + 1)

sumA :: AList -> Int
sumA xs =
  case xs of
    ANil -> 0
    ACons a _ _ rst -> a + sumA rst

sumB :: AList -> Int
sumB xs =
  case xs of
    ANil -> 0
    ACons _ b _ rst -> b + sumB rst

sumC :: AList -> Int
sumC xs =
  case xs of
    ANil -> 0
    ACons _ _ c rst -> c + sumC rst

gibbon_main =
  let n0 = sizeParam
      xs = mkAList n0
      ys = mapArith xs (0 - 3)
      d = digest ys 0
      a = sumA ys
      b = sumB ys
      c = sumC ys
  in (d, a, b, c)
