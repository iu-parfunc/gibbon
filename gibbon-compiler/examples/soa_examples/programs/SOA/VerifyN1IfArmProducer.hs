-- Probe for a residual gap in `ScalarCountPropagation.countPropagatedProducers`:
-- `callSiteCovered` (ScalarCountPropagation.hs:210-228) only checks that the
-- ENCLOSING function is one this pass rewrites (funRec == NotRec) and that the
-- two ends arguments are variables.  It does NOT check that the call sits in a
-- `LetE` right-hand side, which is the only position `rewriteExp` /
-- `copyBindsForRhs` (ScalarCountPropagation.hs:90-94, 139-153) actually
-- rewrites.  A producer call in, e.g., an `IfE` arm tail would therefore be
-- reported as "count propagated" while receiving no `ScalarCountCopyAll`,
-- which makes `LoopifyTraversals.countGuaranteedTyCons` admit the type and the
-- loopified consumer read a 0 trip count.
--
-- `mul2` is shape preserving (so ScalarCountPropagation recognizes it) but not
-- loopifiable (the nested conditional defeats the scalar-plan extractor), so it
-- is count-establishing ONLY via propagation.
data List = Cons Int List | Nil
{-# ANN type List "Factored" #-}

{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int -> List
mkList n = if n <= 0
           then Nil
           else let rst = mkList (n - 1)
                 in Cons n rst

mul2 :: List -> List
mul2 lst = case lst of
             Nil -> Nil
             Cons i rst -> let v = if i == 0
                                   then 0
                                   else (if i == 1 then 1 else i * 2)
                            in Cons v (mul2 rst)

{-# ANN add1 "OPT:CanVectorize" #-}
add1 :: List -> List
add1 lst = case lst of
             Nil -> Nil
             Cons i rst -> let i1 = i + 1
                            in Cons i1 (add1 rst)

sumList :: List -> Int
sumList lst = case lst of
                Nil -> 0
                Cons i rst -> let s = sumList rst
                               in i + s

gibbon_main =
  let l1 = mkList 100
      n  = sumList l1
      h  = if n == 0 then mul2 l1 else mul2 l1
      m  = add1 h
   in sumList m
