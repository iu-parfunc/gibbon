
module Layout where
import Set

build :: Int -> Int -> IntSet
build x sz = 
    if x == 0 then singleton 0
    else insert (x-(sz/2)) (build (x-1) sz)

eval :: Int -> IntSet -> Int
eval x m =
    let 
        _ = printsym (quote "---\n")
        _ = printsym (quote "ID: [")
        _ = printint (x)
        _ = printsym (quote"]\n")
        d = iterate (depth x 0 m)
        _ = printsym (quote "DEPTH: [")
        _ = printint (d)
        _ = printsym (quote "]\n")
    in  if d < 0 then 0
        else eval (x-1) m

gibbon_main = 
    let s = 16
    in eval (s/2) (build s s)