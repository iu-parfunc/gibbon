-- Four widths inside ONE constructor (so they fuse), beside a second
-- constructor with a deliberately different frequency (so it must not).
data W = N Int8 Int16 Int32 Int64 W | S Int32 W | End
{-# ANN type W "Factored" #-}

-- The recursive call is written separately inside each constructor
-- alternative rather than let-bound once before the branch.  That is NOT a
-- workaround for VW-31 any more -- that miscompilation has since been fixed -- but a
-- consequence of the representation: this type is `Factored`, and a
-- fully-factored value's start is one cursor per buffer, so a tail built for
-- `S` does not begin where `N` needs it.  Sharing the binding is now refused
-- before code generation, by name, with the rewrite below suggested.  See
-- VW-31's "what is still not supported".
{-# ANN mkW "OPT:StoreScalarCounts" #-}
mkW :: Int -> W
mkW n =
  if n <= 0
  then End
  else if mod n 4 == 0
       then S (toInt32 (n + 5000)) (mkW (n - 1))
       else N (toInt8 (mod n 7)) (toInt16 (n + 300)) (toInt32 (n + 40000)) (n + 500000) (mkW (n - 1))

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
      u = bumpW t
  in (lenW u, sN8 u, sN16 u, sN32 u, sN64 u, sS u)
