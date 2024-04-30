module Insertion where

import Gibbon.Vector

isort2 :: (a -> a -> Int) -> Vector a -> Vector a
isort2 cmp xs =
  let n = length xs
  in go 1 n cmp (copy xs)

go :: Int -> Int -> (a -> a -> Int) -> Vector a -> Vector a
go i n cmp ys =
  if i == n
  then ys
  else let ys' = shift i cmp ys
       in go (i+1) n cmp ys'


shift :: Int -> (a -> a -> Int) -> Vector a -> Vector a
shift j cmp ys =
  if j == 0
  then ys
  else let a = nth ys j
           b = nth ys (j-1)
       in if (cmp a b) > 0
          then ys
          else let ys' = inplaceUpdate j b ys
                   ys'' = inplaceUpdate (j-1) a ys'
               in shift (j-1) cmp ys''

--------------------------------------------------------------------------------

insert :: (a -> a -> Int) -> Vector a -> a -> Int -> Vector a
insert cmp xs x n =
  if n == 0
  then inplaceUpdate 0 x xs
  else let y = nth xs (n-1)
       in if (cmp x y) < 0
          then let xs' = inplaceUpdate n y xs
               in insert cmp xs' x (n-1)
          else inplaceUpdate n x xs

isort :: (a -> a -> Int) -> Vector a -> Vector a -> Int -> Vector a
isort cmp xs b n =
  let len = length xs
  in if len <= 1
  then xs
  else if n == 0
       then b
       else let xs' = isort cmp xs b (n-1)
            in insert cmp xs' (nth xs n) n

isort1 :: (a -> a -> Int) -> Vector a -> Vector a
isort1 cmp xs =
  let n = length xs
      hd = nth xs 0
      b :: Vector a
      b = generate n (\i -> hd)
  in isort cmp xs b ((length xs) - 1)

--------------------------------------------------------------------------------

gibbon_main =
  let n = sizeParam
      ls :: Vector Int
      ls = generate sizeParam (\i -> n-i)

      ls1 = isort1 compare_int ls
      ls2 = isort2 compare_int ls

      _ = printVec (\i -> printint i) ls1
  in (ls1,ls2)