-- TupleTraversalEffects.
-- Functions: mkTree, sumCount, sumCount3, sumNested, sumOne, prodLit,
-- letShaped, ifShaped.

module TupleTraversalEffects where

data Tree = Leaf Int | Node Tree Tree

mkTree :: Int -> Tree
mkTree n = if n == 0 then Leaf 1 else Node (mkTree (n - 1)) (mkTree (n - 1))

-- Two-field product; prints 2 once per leaf.
sumCount :: Tree -> (Int, Int)
sumCount t =
  case t of
    Leaf n -> let _ = printint 2 in (n, 1)
    Node a b ->
      let pa = sumCount a
          pb = sumCount b
      in ((pa !!! 0) + (pb !!! 0), (pa !!! 1) + (pb !!! 1))

-- Three-field product; prints 3 once per leaf.
sumCount3 :: Tree -> (Int, Int, Int)
sumCount3 t =
  case t of
    Leaf n -> let _ = printint 3 in (n, 1, 10)
    Node a b ->
      let pa = sumCount3 a
          pb = sumCount3 b
      in ( (pa !!! 0) + (pb !!! 0)
         , (pa !!! 1) + (pb !!! 1)
         , (pa !!! 2) + (pb !!! 2) )

-- Nested product; prints 4 once per leaf.
sumNested :: Tree -> (Int, (Int, Int))
sumNested t =
  case t of
    Leaf n -> let _ = printint 4 in (n, (1, 10))
    Node a b ->
      let pa = sumNested a
          pb = sumNested b
      in ( (pa !!! 0) + (pb !!! 0)
         , ( ((pa !!! 1) !!! 0) + ((pb !!! 1) !!! 0)
           , ((pa !!! 1) !!! 1) + ((pb !!! 1) !!! 1) ) )

sumOne :: Tree -> Int
sumOne t =
  case t of
    Leaf n -> n
    Node a b -> sumOne a + sumOne b

-- Control: the let RHS is already a tuple literal of the right arity.
prodLit :: Tree -> (Int, Int)
prodLit t = (sumOne t, 7)

-- Control: the let RHS is a let-expression.
letShaped :: Tree -> (Int, Int)
letShaped t = let x = sumOne t in (x, x + 1)

-- Control: the let RHS is an if-expression; exactly one branch's effect runs.
ifShaped :: Int -> (Int, Int)
ifShaped k =
  if k == 0
  then let _ = printint 5 in (10, 11)
  else let _ = printint 6 in (20, 21)

gibbon_main =
  let t = mkTree 2
      p2 = sumCount t
      p3 = sumCount3 t
      pn = sumNested t
      pl = prodLit t
      ps = letShaped t
      pi = ifShaped 0
      -- Control: a trivial (VarE) RHS must not be re-bound or re-evaluated.
      pv = ps
      total = (p2 !!! 0) + (p2 !!! 1)
              + (p3 !!! 0) + (p3 !!! 1) + (p3 !!! 2)
              + (pn !!! 0) + ((pn !!! 1) !!! 0) + ((pn !!! 1) !!! 1)
              + (pl !!! 0) + (pl !!! 1)
              + (ps !!! 0) + (ps !!! 1)
              + (pi !!! 0) + (pi !!! 1)
              + (pv !!! 0)
  in total
