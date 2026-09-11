-- Explicit-width coverage for a recursive packed AoS structure: an Int8
-- field alongside an Int32 field, copied (packed constructor traversal +
-- allocation), printed via the generated datatype printer, and reduced
-- with homogeneous Int32 arithmetic across the recursion.
data Chain = Nil | Link Int8 Int32 Chain

mkChain :: Int8 -> Chain
mkChain n =
  if n < 1
  then Nil
  else Link n 1000 (mkChain (n - 1))

copyChain :: Chain -> Chain
copyChain c =
  case c of
    Nil -> Nil
    Link a b rst -> Link a b (copyChain rst)

sumChain32 :: Chain -> Int32
sumChain32 c =
  case c of
    Nil -> 0
    Link _a b rst -> b + (sumChain32 rst)

gibbon_main =
  let c = mkChain 4
      c2 = copyChain c
      _u = printPacked c2
      s = sumChain32 c2
  in s
