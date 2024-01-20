import Basics

type Text = Vector Char

data Content = C Block

data List = Cons2 Content (List) | E

mkConsList :: Int -> List
mkConsList len = if len <= 0
                then E
                else let
                        rst = mkConsList (len - 1)
                        val = C (Plain (mkRandomInlineList 100))
                      in Cons2 val rst

getLengthCons :: List -> Int
{-# ANN getLengthCons Cons #-}
getLengthCons lst = case lst of
                        Cons2 val rst ->  getLengthCons rst + 1
                        E          -> 0



gibbon_main =
        let --snocList = mkSnocList 90000
            consList = mkConsList 3000000
            --l1       = getLengthSnoc snocList
            l2       = iterate (getLengthCons consList)
            --_        = printsym (quote "Length Snoc: ")
            --_        = printint l1
            --_        = printsym (quote "NEWLINE")
            _        = printsym (quote "Length Cons: ")
            _        = printint l2
            _        = printsym (quote "NEWLINE")
          in ()
