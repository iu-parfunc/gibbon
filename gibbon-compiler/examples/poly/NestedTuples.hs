module NestedTuples where


foo (a, b, (c, d, (e, f, (g, h, (i, j))))) = ((a,b), (c,(d, e), f), (g, h), (i, j))

gibbon_main = foo (1, (2,3), (4, (5,6), (7,8, (9,10, (11,12))))) 