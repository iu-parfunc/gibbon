-- VW-09 step 5, case B: a value whose scalar-count metadata is ABSENT.
--
-- `readPackedFile` hands back a whole packed value.  It writes no constructor
-- tag, so nothing establishes the scalar-count footers for it, and every
-- footer it exposes reads back as 0 through the public getter -- exactly like a
-- chunk that genuinely holds zero elements.  A loopified traversal over such a
-- value would take 0 as its trip count and silently produce an empty result.
--
-- `countGuaranteedTyCons` must therefore refuse to loopify this type at all
-- (`unattributedPackedTyCons`), leaving `bumpP` recursive.  The answer below
-- is what a recursive traversal produces; if the type were ever admitted, the
-- second component would collapse toward zero instead.
data P = PC Int Int P | PEnd
{-# ANN type P "Factored" #-}

{-# ANN mkP "OPT:StoreScalarCounts" #-}
mkP :: Int -> P
mkP n = if n <= 0 then PEnd else PC n (n + 100) (mkP (n - 1))

{-# ANN bumpP "OPT:MayVectorize" #-}
bumpP :: P -> P
bumpP p = case p of
            PEnd -> PEnd
            PC a b rst -> PC (a + 1) (b + 2) (bumpP rst)

sumP :: P -> Int
sumP p = case p of
           PEnd -> 0
           PC a b rst -> a + b + sumP rst

gibbon_main =
  let v  = mkP 40
      _  = writePackedFile "vw09_untrusted.gpkd" v
      v2 = readPackedFile @P (Just "vw09_untrusted.gpkd")
      -- a map over the FILE-LOADED value, whose counts nothing established
      w  = bumpP v2
  in (sumP v2, sumP w)
