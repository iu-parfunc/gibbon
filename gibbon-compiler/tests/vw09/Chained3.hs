-- VW-09: chained ScalarCountCopyAll propagation.
--
-- P0 (mkW, OPT:StoreScalarCounts) -> P1 (p1, plain shape-preserving map,
-- +1/+1) -> P2 (p2, plain shape-preserving map, +10/+100) -> C (bumpW,
-- OPT:MayVectorize, loopified).  p1 and p2 each establish no counts of their
-- own; each must receive exactly one ScalarCountCopyAll at its call site, and
-- the sequence P0's physical writes -> p1's copied counts -> p2's copied
-- counts -> C's trip counts must agree, per buffer and per chunk.
data W = N Int8 Int16 Int32 Int64 W | S Int32 W | End
{-# ANN type W "Factored" #-}

{-# ANN mkW "OPT:StoreScalarCounts" #-}
mkW :: Int -> W
mkW n =
  if n <= 0
  then End
  else if mod n 4 == 0
       then S (toInt32 (n + 5000)) (mkW (n - 1))
       else N (toInt8 (mod n 7)) (toInt16 (n + 300)) (toInt32 (n + 40000)) (n + 500000) (mkW (n - 1))

-- P1: plain recursive shape-preserving map, no annotation.
p1 :: W -> W
p1 t =
  case t of
    End -> End
    N a b c d rst -> N (a + 1) b c d (p1 rst)
    S e rst -> S (e + 1) (p1 rst)

-- P2: a second plain recursive shape-preserving map, chained after P1.
p2 :: W -> W
p2 t =
  case t of
    End -> End
    N a b c d rst -> N a (b + 10) c d (p2 rst)
    S e rst -> S (e + 100) (p2 rst)

-- P3: a third plain recursive shape-preserving map, chained after P2.
p3 :: W -> W
p3 t =
  case t of
    End -> End
    N a b c d rst -> N a b (c + 1000) d (p3 rst)
    S e rst -> S (e + 10000) (p3 rst)

{-# ANN bumpW "OPT:MayVectorize" #-}
bumpW :: W -> W
bumpW t =
  case t of
    End -> End
    N a b c d rst -> N (a + 1) (b + 2) (c + 3) (d + 4) (bumpW rst)
    S e rst -> S (e + 9) (bumpW rst)

sN8 :: W -> Int
sN8 t = case t of
          End -> 0
          N a b c d rst -> (toInt64 a) + sN8 rst
          S e rst -> sN8 rst
sN16 :: W -> Int
sN16 t = case t of
           End -> 0
           N a b c d rst -> (toInt64 b) + sN16 rst
           S e rst -> sN16 rst
sN32 :: W -> Int
sN32 t = case t of
           End -> 0
           N a b c d rst -> (toInt64 c) + sN32 rst
           S e rst -> sN32 rst
sN64 :: W -> Int
sN64 t = case t of
           End -> 0
           N a b c d rst -> d + sN64 rst
           S e rst -> sN64 rst
sS :: W -> Int
sS t = case t of
         End -> 0
         N a b c d rst -> sS rst
         S e rst -> (toInt64 e) + sS rst
lenW :: W -> Int
lenW t = case t of
           End -> 0
           N a b c d rst -> 1 + lenW rst
           S e rst -> 1 + lenW rst

gibbon_main =
  let t = mkW (sizeParam)
      u1 = p1 t
      u2 = p2 u1
      u3 = p3 u2
      v  = bumpW u3
  in (lenW v, sN8 v, sN16 v, sN32 v, sN64 v, sS v)
