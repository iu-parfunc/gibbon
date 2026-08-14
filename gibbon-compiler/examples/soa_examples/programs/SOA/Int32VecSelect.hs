-- Audit probe (int32 vectorizer): drive the VecEq + VecSelect lowering, which
-- for int32x4 must use the NATIVE `_mm_cmpeq_epi32` mask (Codegen.hs:527-529)
-- rather than the scalar-spill emulation the int64x2 family uses.  No partial
-- primitive appears in the guard, so the loop is not rejected by fix V1.
data SList = SCons Int Int SList | SNil
{-# ANN type SList "Factored" #-}

{-# ANN mkSList "OPT:StoreScalarCounts" #-}
mkSList :: Int -> SList
mkSList n =
  if n <= 0
  then SNil
  else let rst = mkSList (n - 1)
           t = mod n 3
       in SCons t (n * 2) rst

{-# ANN mapSel "OPT:CanVectorize" #-}
mapSel :: SList -> Int -> SList
mapSel xs k =
  case xs of
    SNil -> SNil
    SCons i j rst ->
      let i1 = if i == 0 then i + k else i - k
          j1 = j + 1
      in SCons i1 j1 (mapSel rst k)

digest :: SList -> Int -> Int
digest xs p =
  case xs of
    SNil -> 0
    SCons i j rst -> (((i * 11) + (j * 101)) * (p + 1)) + digest rst (p + 1)

gibbon_main =
  let n0 = sizeParam
      xs = mkSList n0
      ys = mapSel xs 5
  in digest ys 0
