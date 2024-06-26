{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Packed lists.
module Gibbon.PList where

import Gibbon.Prelude
--------------------------------------------------------------------------------

data PList a = Nil | Cons a (PList a)

is_empty_plist :: PList a -> Bool
is_empty_plist ls =
  case ls of
    Nil -> True
    Cons _ _ -> False

head_plist :: PList a -> a
head_plist ls =
  case ls of
    Cons x _ -> x

tail_plist :: PList a -> PList a
tail_plist ls =
  case ls of
    Cons _ xs -> xs

elem_plist :: (a -> a -> Int) -> a -> PList a -> Bool
elem_plist cmp a list =
  case list of
    Nil       -> False
    Cons x rst -> if (cmp x a) == 0
                   then True
                   else (elem_plist cmp a rst)


-- nth element of a plist
--       arguments
--       plist
--       default value
--       index needed in the plist 
--       start counter always use it as 0   

nth_plist :: PList a -> a -> Int -> Int -> a
nth_plist list def index start = case list of
        Nil -> def 
        Cons x rst -> if (index == start) 
                      then x 
                      else nth_plist rst def index (start+1)   


length_plist :: PList a -> Int
length_plist a = case a of
  Nil       -> 0
  Cons x xs -> 1 + length_plist xs

filter_plist :: (a -> Bool) -> PList a -> PList a
filter_plist f a =
  case a of
    Nil -> Nil
    Cons z zs -> if f z then Cons z (filter_plist f zs) else filter_plist f zs

take_plist :: Int -> PList a -> PList a
take_plist n a =
  if n == 0
    then Nil
  else
    case a of
      Nil -> Nil
      Cons z zs -> Cons z (take_plist (n-1) zs)

drop_plist :: Int -> PList a -> PList a
drop_plist num list =
  case list of
    Nil        -> Nil
    Cons x rst -> if (num <= 0)
                   then Cons x rst
                   else drop_plist (num - 1) rst

reverse_plist :: PList a -> PList a -> PList a
reverse_plist xs acc =
  case xs of
    Nil       -> acc
    Cons z zs -> reverse_plist zs (Cons z acc)

map_plist :: (a -> b) -> PList a -> PList b
map_plist f ls =
  case ls of
    Nil -> Nil
    Cons x xs -> Cons (f x) (map_plist f xs)

splitAt_plist :: Int -> PList a -> (PList a, PList a)
splitAt_plist n a = if n == 0
  then (Nil, a)
  else case a of
    Nil       -> (Nil, Nil) -- error case
    Cons x xs -> let (c, d) = splitAt_plist (n - 1) xs in (Cons x c, d)

zip_plist :: PList a -> PList b -> PList (a, b)
zip_plist as bs =
  case as of
    Nil -> Nil
    Cons z zs -> case bs of
      Nil -> Nil
      Cons y ys -> Cons (z, y) (zip_plist zs ys)
