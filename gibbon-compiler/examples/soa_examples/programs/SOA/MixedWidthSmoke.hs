-- MixedWidthSmoke: Rec (Factored).
-- Functions: mkRec, bump, total.
-- Annotated: MayVectorize on bump; StoreScalarCounts on mkRec.
data Rec = Node Int8 Int16 Int32 Int64 Rec | Leaf

{-# ANN type Rec "Factored" #-}

{-# ANN mkRec "OPT:StoreScalarCounts" #-}
mkRec :: Int64 -> Rec
mkRec n = if n <= 0
          then Leaf
          else Node (toInt8 (mod n 50)) (toInt16 (mod n 300))
                    (toInt32 (mod n 1000)) (mod n 7)
                    (mkRec (n - 1))

{-# ANN bump "OPT:MayVectorize" #-}
bump :: Rec -> Rec
bump r = case r of
           Leaf -> Leaf
           Node a b c d rst -> Node (a + 3) (b - 2) (c * 2) (d + 1) (bump rst)

total :: Rec -> Int64
total r = case r of
            Leaf -> 0
            Node a b c d rst -> toInt64 a + toInt64 b + toInt64 c + d + total rst

gibbon_main = let _ = printsym (quote "Running program MixedWidthSmoke on Rec with explicit Int8/Int16/Int32/Int64 fields: ")
                  _ = printsym (quote "NEWLINE")
                  r = mkRec 1000

                  _ = printsym (quote "Running pass bump (map, uses=4, shared=0): ")
                  _ = printsym (quote "NEWLINE")
                  s = iterate (total (bump r))
                  _ = printsym (quote "End")
                  _ = printsym (quote "NEWLINE")
              in s
