-- VerifyV1CondDivide: DList (Factored).
-- Functions: mkDList, mapCondDiv, sumFirst, sumSecond.
-- Annotated: MayVectorize on mapCondDiv; StoreScalarCounts on mkDList.
data DList = DCons Int Int DList | DNil
{-# ANN type DList "Factored" #-}

{-# ANN mkDList "OPT:StoreScalarCounts" #-}
mkDList :: Int -> DList
mkDList n =
  if n <= 0
  then DNil
  else let rst = mkDList (n - 1)
       in DCons n (n * 2) rst

{-# ANN mapCondDiv "OPT:MayVectorize" #-}
mapCondDiv :: DList -> Int -> DList
mapCondDiv xs k =
  case xs of
    DNil -> DNil
    DCons i j rst ->
      let i1 = if (i / k) == 0 then 111 else 222
          j1 = j + k
      in DCons i1 j1 (mapCondDiv rst k)

sumFirst :: DList -> Int
sumFirst xs = case xs of
                DNil -> 0
                DCons i _ rst -> let s = sumFirst rst in i + s

sumSecond :: DList -> Int
sumSecond xs = case xs of
                 DNil -> 0
                 DCons _ j rst -> let s = sumSecond rst in j + s

gibbon_main =
  let xs = mkDList 40
      ys = mapCondDiv xs 7
      s1 = sumFirst ys
      s2 = sumSecond ys
  in (s1, s2)
