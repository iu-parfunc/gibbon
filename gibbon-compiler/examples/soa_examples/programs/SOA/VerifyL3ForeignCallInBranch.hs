-- VARIANT of the L3 defect: a user constructor branch containing a form the
-- synthesized loop cannot reproduce -- a call to another (non-self) function.
data L = C Int L | N
{-# ANN type L "Factored" #-}

{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int -> L
mkList n = if n <= 0
           then N
           else let rst = mkList (n - 1)
                 in C n rst

-- recursive so the frontend cannot inline it away
sideEffect :: Int -> Int
sideEffect x = if x <= 0
               then 0
               else let r = sideEffect (x - 1)
                     in r + 3

{-# ANN mapCall "OPT:MayVectorize" #-}
mapCall :: L -> L
mapCall lst = case lst of
                N -> N
                C i rst -> let i1 = sideEffect i
                            in C i1 (mapCall rst)

sumList :: L -> Int
sumList lst = case lst of
                N -> 0
                C i rst -> let s = sumList rst in i + s

gibbon_main =
  let l1 = mkList 20
      m  = mapCall l1
   in sumList m
