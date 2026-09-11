-- VW-31 regression: a let-bound recursive packed tail consumed by MORE THAN
-- ONE constructor alternative.
--
-- The recursive call is written ONCE, before the branch, and both arms use its
-- result.  Before the fix this compiled without a diagnostic and then produced
-- a corrupt value: every node came back carrying the last arm's tag (because
-- `reorderScalarWrites` hoisted both arms' tag writes out of the conditional
-- and sequenced them), and deeper inputs faulted.
--
-- The observables are chosen to distinguish the ways it can go wrong: the
-- constructor counts catch a clobbered tag, the two sums catch a field written
-- at the wrong offset or read from the tail's end cursor, the length catches a
-- lost head or tail and a truncated traversal, and `headTag` catches the first
-- node specifically.  Expected values are derived from the source definition,
-- not recorded from Gibbon.
data T = A Int T | B Int T | End
{-# ANN type T "Linear" #-}

mkT :: Int -> T
mkT n =
  if n <= 0
  then End
  else let rst = mkT (n - 1)
        in if mod n 3 == 0
           then B (n + 200) rst
           else A (n * 7) rst

lenT :: T -> Int
lenT t = case t of
           End -> 0
           A x rst -> 1 + lenT rst
           B y rst -> 1 + lenT rst

countA :: T -> Int
countA t = case t of
             End -> 0
             A x rst -> 1 + countA rst
             B y rst -> countA rst

countB :: T -> Int
countB t = case t of
             End -> 0
             A x rst -> countB rst
             B y rst -> 1 + countB rst

sumA :: T -> Int
sumA t = case t of
           End -> 0
           A x rst -> x + sumA rst
           B y rst -> sumA rst

sumB :: T -> Int
sumB t = case t of
           End -> 0
           A x rst -> sumB rst
           B y rst -> y + sumB rst

headTag :: T -> Int
headTag t = case t of
              End -> 2
              A x rst -> 0
              B y rst -> 1

gibbon_main =
  let t = mkT 11
  in (lenT t, countA t, countB t, sumA t, sumB t, headTag t)
