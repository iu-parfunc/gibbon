module Fib where

import Gibbon.Prelude

--------------------------------------------------------------------------------

fib_seq :: Int -> Int
fib_seq n =
    if n == 0
    then 0
    else if n == 1
    then 1
    else
        let x = fib_seq (n - 1)
            y = fib_seq (n - 2)
        in x + y

fib_par :: Int -> Int -> Int
fib_par cutoff n =
    if n <= 1
    then n
    else if n <= cutoff
    then fib_seq n
    else
        let x = spawn (fib_par cutoff (n - 1))
            y = fib_par cutoff (n - 2)
            _ = sync
        in x + y

-- go :: Int -> ()
-- go n = 
--     if n == 0 
--         then 
--             let _ = printint 0 
--                 _ = printsym (quote "SPACE")
--             in ()
--         else 
--             let _ = go (n-1)
--                 v = fib_par 5 n 
--                 _ = printint v
--                 _ = printsym (quote "SPACE")
--             in ()

-- gibbon_main = 
--     let n = sizeParam
--         v = fib_par 5 n
--         _ = printint v
--     in ()
