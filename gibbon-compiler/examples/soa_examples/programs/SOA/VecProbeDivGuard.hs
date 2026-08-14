-- Probe: guarded integer division inside a CanVectorize map.
-- The scalar program never divides by zero because the `d == 0` branch is
-- taken first.  The vectorizer canonicalizes the statement-level conditional
-- write into a value-level select, which evaluates BOTH arms eagerly.
data DList = DCons Int Int DList | DNil
{-# ANN type DList "Factored" #-}

-- Field 0 counts down (n .. 1), field 1 is the divisor and is 0 for the
-- element where n == 3 so the guarded branch is actually exercised.
{-# ANN mkDList "OPT:StoreScalarCounts" #-}
mkDList :: Int -> DList
mkDList n =
  if n <= 0
  then DNil
  else let rst = mkDList (n - 1)
           dv = if n == 3 then 0 else n
       in DCons (n * 100) dv rst

{-# ANN mapDiv "OPT:CanVectorize" #-}
mapDiv :: DList -> Int -> DList
mapDiv xs k =
  case xs of
    DNil -> DNil
    DCons i j rst ->
      let i1 = if j == 0 then 0 else i / j
          j1 = j + k
      in DCons i1 j1 (mapDiv rst k)

sumFirst :: DList -> Int
sumFirst xs =
  case xs of
    DNil -> 0
    DCons i _ rst -> i + sumFirst rst

sumSecond :: DList -> Int
sumSecond xs =
  case xs of
    DNil -> 0
    DCons _ j rst -> j + sumSecond rst

gibbon_main =
  let xs = mkDList 40
      ys = mapDiv xs 1
      s1 = sumFirst ys
      s2 = sumSecond ys
  in (s1, s2)
