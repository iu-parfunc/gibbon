data A = A Int Int

foo :: A -> Int
foo (A x y) = x * x + y * y - 1

gibbon_main = foo (A 2 3)