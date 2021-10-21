module NestedTuples where

data Foo a b = MkFoo (a, b)

foo (a, b) = (b, a)
-- bar (a, b) (c, d) (e, f) g = (b, (a, d), (c, f), e, g)
-- baz (a, b, (c, d, (e, f, (g, h, (i, j))))) =
--   ((a, b), (c, (d, e), f), (g, h), (i, j))


gibbon_main = foo (1,(2,3)) 
  -- let (a, b, c) =
  --       ( bar (1, 2)
  --       , bar (1, 2) (3, 4) (5, (6, 7)) 4
  --       , baz (1, (2, 3), (4, (5, 6), (7, 8, (9, 10, (11, 12)))))
  --       )
  -- in  (c, a, b)
