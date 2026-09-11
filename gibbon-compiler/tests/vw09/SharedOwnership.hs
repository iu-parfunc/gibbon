-- VW-09 step 3: selective-buffer-sharing count ownership.
--
-- `touchOne` is shape preserving and changes exactly ONE of N's four scalar
-- buffers.  The other three are pure copies and are therefore eligible for
-- selective sharing, as is S's buffer and the dcon stream.  The program then
-- consumes BOTH the shared output and the original input, and consumes the
-- original AFTER the output, so a count update that mutated metadata still
-- owned by the input would show up as a wrong sum rather than as nothing.
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

-- Only the Int32 field of N changes; a, b, d and S's field are copied.
{-# ANN touchOne "OPT:MayVectorize" #-}
touchOne :: W -> W
touchOne t =
  case t of
    End -> End
    N a b c d rst -> N a b (c + 3) d (touchOne rst)
    S e rst -> S e (touchOne rst)

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
  let t  = mkW (sizeParam)
      u  = touchOne t
      -- the output first ...
      o1 = lenW u
      o2 = sN8 u
      o3 = sN16 u
      o4 = sN32 u
      o5 = sN64 u
      o6 = sS u
      -- ... then the still-live input, whose Int32 field must be UNCHANGED
      i1 = sN32 t
      i2 = sN8 t
      i3 = sS t
  in (o1, o2, o3, o4, o5, o6, i1, i2, i3)
