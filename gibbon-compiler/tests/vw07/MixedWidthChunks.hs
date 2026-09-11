-- VW-07 capacity fixture.  Correctness-only; NOT a benchmark.
--
-- A genuine FullyFactored SoA datatype with one buffer per integer width, so a
-- single loopified traversal drives four output buffers whose byte-per-element
-- costs differ by 8x.  `bump` is shape-preserving and MayVectorize, so it is
-- replaced by the synthesized chunk loops whose capacity invariant VW-07 is
-- about.  Sized from sizeParam so the driver can cross chunk boundaries.
module MixedWidthChunks where

data Rec = Node Int8 Int16 Int32 Int64 Rec | Leaf
{-# ANN type Rec "Factored" #-}

{-# ANN mkRec "OPT:StoreScalarCounts" #-}
mkRec :: Int64 -> Rec
mkRec n = if n <= 0 then Leaf
          else Node (toInt8 (mod n 50)) (toInt16 (mod n 300))
                    (toInt32 (mod n 1000)) (mod n 7) (mkRec (n - 1))

{-# ANN bump "OPT:MayVectorize" #-}
bump :: Rec -> Rec
bump r = case r of
           Leaf -> Leaf
           Node a b c d rst -> Node (a + 3) (b - 2) (c * 2) (d + 1) (bump rst)

total :: Rec -> Int64
total r = case r of
            Leaf -> 0
            Node a b c d rst -> toInt64 a + toInt64 b + toInt64 c + d + total rst

gibbon_main = total (bump (mkRec sizeParam))
