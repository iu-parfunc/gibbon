module Common where

data Maybe a = Nothing | Just a

data Ord = Lt | Gt | Eq | Cc

compareInt :: Int -> Int -> Ord
compareInt a b = 
    let sub = a - b
    in 
        if sub < 0 then Lt
        else if sub > 0 then Gt
        else if sub == 0 then Eq
        else Cc