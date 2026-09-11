-- ExplicitConversions.
-- Functions: widenSum, buildChain, sumChain.
module ExplicitConversions where

data Mixed = Mixed Int8 Int16 Int32 Int64

widenSum :: Mixed -> Int64
widenSum m =
  case m of
    Mixed a b c d -> toInt64 a + toInt64 b + toInt64 c + toInt64 d

-- Conversions inside a recursive traversal over narrow fields.
data Chain = CNil | CCons Int8 Int32 Chain

buildChain :: Int64 -> Chain
buildChain n =
  if n == 0
  then CNil
  else CCons (toInt8 (n * 100)) (toInt32 (n * 100000)) (buildChain (n - 1))

sumChain :: Chain -> Int64
sumChain c =
  case c of
    CNil -> 0
    CCons a b rest -> toInt64 a + toInt64 b + sumChain rest

gibbon_main =
  let -- The named examples from the specification.
      _ = printint (toInt8 127)         -- 127
      _ = printint (toInt8 128)         -- -128
      _ = printint (toInt8 255)         -- -1
      _ = printint (toInt8 256)         -- 0
      _ = printint (toInt8 (0 - 128))   -- -128
      _ = printint (toInt8 (0 - 129))   -- 127
      -- Widening sign-extends.
      _ = printint (toInt16 (toInt8 255))          -- -1
      _ = printint (toInt64 (toInt32 2147483648))  -- -2147483648
      _ = printint (toInt64 (toInt32 4294967295))  -- -1
      _ = printint (toInt16 (toInt32 32768))       -- -32768
      _ = printint (toInt16 (toInt32 65535))       -- -1
      -- Unconstrained arithmetic resolves at Int64 first, then converts.
      _ = printint (toInt8 (200 + 100))            -- 44
      -- Same-width conversion is value-preserving.
      _ = printint (toInt32 (toInt32 123456))      -- 123456
      -- Conversion feeding a comparison and then arithmetic.
      narrowed = toInt8 200
      _ = printbool (narrowed < 0)                 -- #t  (200 -> -56)
      _ = printint (toInt64 narrowed + 100)        -- 44
      -- Conversions through a constructor, a case, and a recursive function.
      m = Mixed (toInt8 300) (toInt16 300) (toInt32 300) (toInt64 300)
      _ = printPacked m
      _ = printint (widenSum m)
      ch = buildChain 3
      _ = printint (sumChain ch)
  in 0
