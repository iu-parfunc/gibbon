-- VARIANT of the V1 defect: the trapping division is inside a NESTED
-- conditional arm (if .. then .. else (if .. then .. else n/d)), and a second
-- field puts a division inside the IF CONDITION -- a position the scalar loop
-- also evaluates unconditionally, so it must remain vectorizable.
data DList = DCons Int Int DList | DNil
{-# ANN type DList "Factored" #-}

{-# ANN mkDList "OPT:StoreScalarCounts" #-}
mkDList :: Int -> DList
mkDList n =
  if n <= 0
  then DNil
  else let rst = mkDList (n - 1)
           dv = if n == 3 then 0 else n
       in DCons (n * 100) dv rst

{-# ANN mapNested "OPT:CanVectorize" #-}
mapNested :: DList -> Int -> DList
mapNested xs k =
  case xs of
    DNil -> DNil
    DCons i j rst ->
      let i1 = if i == 0
               then 0
               else (if j == 0 then 7 else i / j)
          j1 = j + k
      in DCons i1 j1 (mapNested rst k)

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
      ys = mapNested xs 1
      s1 = sumFirst ys
      s2 = sumSecond ys
  in (s1, s2)
