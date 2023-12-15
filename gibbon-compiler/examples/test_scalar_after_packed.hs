module Main where 

data ScalarAfterPacked = A ScalarAfterPacked Int | Nil 


gibbon_main = 
    let a = (A (A Nil 4) 4)
    in printPacked a 
