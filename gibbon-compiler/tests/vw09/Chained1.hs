-- VW-09: chained ScalarCountCopyAll propagation, one hop.
-- P0 (mkW) -> P1 (p1, plain shape-preserving map) -> C (bumpW, loopified).
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

p1 :: W -> W
p1 t =
  case t of
    End -> End
    N a b c d rst -> N (a + 1) b c d (p1 rst)
    S e rst -> S (e + 1) (p1 rst)

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
      v  = bumpW u1
  in (lenW v, sN8 v, sN16 v, sN32 v, sN64 v, sS v)
