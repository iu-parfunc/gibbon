
module Right where
import Common
import Map

sumRight :: Map Int -> Int
sumRight m =
    case m of
        Tip -> 0
        Bin _ _ v l r -> sumRight(r) + v + sumRight(l)


build :: Int -> Int -> Map Int -> Map Int
build x sz m = 
    if (sz == 0) then m
    else (build (x - sz/2) (sz/2) (build (x + sz/2) (sz/2) (insert x x m)))

gibbon_main = 
    let m = (build 0 <SIZE> (singleton 0 0))
        --_ = printsym (quote "SUM RIGHT: ")
        s0 = iterate (sumRight m)
        _ = printint (s0)
        _ = printsym (quote "\n")
        --_ = printsym (quote "SUM RIGHT: ")
        s1 = iterate (sumRight m)
        _ = printint (s1)
        _ = printsym (quote "\n")
    in ()