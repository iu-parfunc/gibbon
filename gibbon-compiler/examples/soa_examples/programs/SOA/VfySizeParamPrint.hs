-- F2/F4 probe: `iterate` forces the LetTimedT path, which is where the
-- ITERS:/SIZE: printfs are emitted from the GibInt-returning RTS getters.
-- Also echoes sizeParam itself so the printf specifier and the value can be
-- checked independently of each other.
bump :: Int -> Int
bump n = n + 1

gibbon_main =
  let s = sizeParam
      r = iterate (bump s)
  in r
