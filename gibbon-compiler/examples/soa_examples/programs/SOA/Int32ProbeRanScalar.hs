-- Int32 audit probe: RAN insertion where an Int *literal* field sits after a
-- packed field, which drives AddRAN.mkRANs' `AddFixed <prev-ran> (sizeOfTy IntTy)`
-- branch (AddRAN.hs:323).  That size is the hardcoded-8 `sizeOfTy`, so under
-- --int32 the offset should be 4 bytes too large.
data Foo = A Foo Foo Int Foo | B Int
{-# ANN type Foo "Linear" #-}

mkFoo :: Int -> Foo
mkFoo n =
  if n <= 0
  then B n
  else let p = mkFoo (n - 1)
           q = mkFoo (n - 1)
           r = mkFoo (n - 1)
       in A p q 7 r

lastOf :: Foo -> Int
lastOf f =
  case f of
    B i -> i
    A p q k r -> lastOf r

midOf :: Foo -> Int
midOf f =
  case f of
    B i -> i
    A p q k r -> k

sumFoo :: Foo -> Int
sumFoo f =
  case f of
    B i -> i
    A p q k r -> k + (sumFoo p + (sumFoo q + sumFoo r))

gibbon_main =
  let t = mkFoo 3
      a = lastOf t
      b = midOf t
      c = sumFoo t
  in (a, b, c)
