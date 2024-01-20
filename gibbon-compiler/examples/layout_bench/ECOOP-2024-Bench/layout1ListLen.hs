import Basics

type Text = Vector Char

data List = Snoc (List) Content | E

data Content = C Block


mkSnocList :: Int -> List
mkSnocList len = if len <= 0
      then E
      else let
               rst = mkSnocList (len - 1)
               val = C (Plain (mkRandomInlineList 100))
            in Snoc rst val

getLengthSnoc :: List -> Int
{-# ANN getLengthSnoc Snoc #-}
getLengthSnoc lst = case lst of
               Snoc rst val -> 1 + getLengthSnoc rst
               E            -> 0


gibbon_main = 
   let 
      snocList = mkSnocList 3000000
      --consList = mkConsList 100000
      l1       = iterate (getLengthSnoc snocList)
      --l2       = getLengthCons consList
      _        = printsym (quote "Length Snoc: ")
      _        = printint l1
      _        = printsym (quote "NEWLINE")
      --_        = printsym (quote "Length Cons: ")
      --_        = printint l2
      --_        = printsym (quote "NEWLINE")
    in ()
