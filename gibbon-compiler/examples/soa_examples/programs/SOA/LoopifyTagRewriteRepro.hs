-- CORRECTNESS reproducer (audit finding F3).
--
-- Recursive (no flags)                         : prints 1
-- --store-scalar-field-counts --enable-loopification : prints 0   (WRONG)
-- (fires with the explicit OPT:CanVectorize annotation; --auto-loopification
--  is not required)
--
-- Cause: Gibbon/Passes/LoopifyTraversals.hs `extractBranchPlans` only
-- inspects `Ext (WriteScalar ..)` bindings (stepWrite, line 681-716).  A
-- constructor branch that changes only the *tag* (NilA -> NilB) yields an
-- empty plan map and is accepted.  mkDConInnerLoop (line 1200-1217) then
-- copies input tags verbatim, so the constructor rewrite is dropped.
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
{-# ANN flipNil "OPT:CanVectorize" #-}
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
