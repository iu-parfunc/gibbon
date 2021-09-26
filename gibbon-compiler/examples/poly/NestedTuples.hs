module NestedTuples where

foo (a, b) (c, d) (e, f) g = (b, (a, d), (c, f), e, g)
bar (a, b) = (b, a)
baz (a, b, (c, d, (e, f, (g, h, (i, j))))) =
    ((a, b), (c, (d, e), f), (g, h), (i, j))


gibbon_main =
    ( foo (1, 2) (3, 4) (5, (6, 7)) 4
    , bar (1, 2)
    , baz (1, (2, 3), (4, (5, 6), (7, 8, (9, 10, (11, 12)))))
    )