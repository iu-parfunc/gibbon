-- F4 variant: a *different* RTS entry point, gib_expll, reached with a
-- negative exponent.  Pre-fix the 32-bit -2 arrives as 4294967294 and the
-- loop `for (i = 0; i < pow; i++)` runs ~4e9 times; post-fix it does not run.
gibbon_main =
  let b :: Int
      b = 3
      p = 0 - 2
      r = b ^ p
  in r
