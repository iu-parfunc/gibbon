-- DriverQualify: Rec (Factored), Rec (Factored).
-- Functions: mkRec, bump, total.
-- Annotated: MayVectorize on bump; StoreScalarCounts on mkRec.
module DriverQualify where

data Rec = Node Int8 Int64 Rec | Leaf
{-# ANN type Rec "Factored" #-}

{-# ANN mkRec "OPT:StoreScalarCounts" #-}
mkRec :: Int64 -> Rec
mkRec n = if n <= 0 then Leaf
          else Node (toInt8 (mod n 50)) (mod n 7) (mkRec (n - 1))

{-# ANN bump "OPT:MayVectorize" #-}
bump :: Rec -> Rec
bump r = case r of
           Leaf -> Leaf
           Node a b rst -> Node (a + 1) (b + 2) (bump rst)

total :: Rec -> Int64
total r = case r of
            Leaf -> 0
            Node a b rst -> toInt64 a + b + total rst

gibbon_main =
  let _ = printsym (quote "DRIVER-QUALIFY")
      _ = printsym (quote "NEWLINE")
      s = total (bump (mkRec 64))
   in s
