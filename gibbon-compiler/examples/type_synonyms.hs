-- Without Arguments
type A = Int

f :: Int -> Int 
f x = x + 1

g :: A -> A 
g x = x + 2

h :: A -> Int 
h x  = x + 3

ff :: Int -> A 
ff x = x + 4 

-- With data arguments
data B a b c d = B a b c d deriving Show
type C b d = B Int b Int d

foo :: Int -> Int -> Int -> Int -> C Int Int 
foo x y z w = B (x-1) (y+1) (z+2) (w-2)

gibbon_main = (f 1, g 2, h 3, ff 4, foo 1 2 3 4)

main = print gibbon_main