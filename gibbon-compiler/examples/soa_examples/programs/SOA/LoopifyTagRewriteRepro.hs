-- LoopifyTagRewriteRepro: L (Factored).
-- Functions: mkList, flipNil, endsWithB, sumList.
-- Annotated: MayVectorize on flipNil; StoreScalarCounts on mkList.
data L = Cons Int L | NilA | NilB
{-# ANN type L "Factored" #-}

{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int -> L
mkList n = if n <= 0
           then NilA
           else let rst = mkList (n - 1)
                 in Cons n rst

-- A map that changes a *nullary* constructor (NilA -> NilB) and bumps the
-- scalar field.  Nullary constructors contribute no scalar buffers, so the
-- LoopifyTraversals branch extractor sees an empty plan for the NilA branch
-- and the generated dcon loop copies input tags verbatim.
{-# ANN flipNil "OPT:MayVectorize" #-}
flipNil :: L -> L
flipNil lst = case lst of
                NilA -> NilB
                NilB -> NilB
                Cons i rst -> let i1 = i + 1
                               in Cons i1 (flipNil rst)

-- returns 1 if the list terminator is NilB, 0 if NilA
endsWithB :: L -> Int
endsWithB lst = case lst of
                  NilA -> 0
                  NilB -> 1
                  Cons i rst -> endsWithB rst

sumList :: L -> Int
sumList lst = case lst of
                NilA -> 0
                NilB -> 0
                Cons i rst -> let s = sumList rst
                               in i + s

gibbon_main =
  let l1 = mkList 20
      m  = flipNil l1
   in endsWithB m
