module Main where

import PiecewiseFunctionsBase

-- One timed pass of the PiecewiseFunctions family; see
-- PiecewiseFunctionsBase.hs for the ADT, the builder and why this
-- benchmark is split one executable per pass.
-- @BENCH adt_fields=8

gibbon_main =
  let _ = printsym (quote "Running program Piecewise Functions (MADNESS style): ")
      _ = printsym (quote "NEWLINE")
      pfTree = buildPW (sizeParam + 23) 17

      _ = printsym (quote "Running pass norm2Estimate (fold, uses=5): ")
      _ = printsym (quote "NEWLINE")
      norm = iterate (norm2Estimate pfTree)
      _ = printsym (quote "End")
      _ = printsym (quote "NEWLINE")
  in norm
