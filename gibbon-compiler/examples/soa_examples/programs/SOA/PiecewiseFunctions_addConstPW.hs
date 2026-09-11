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

      _ = printsym (quote "Running pass addConstPW (map, uses=8, shared=5): ")
      _ = printsym (quote "NEWLINE")
      shifted = iterate (addConstPW pfTree 10)
      _ = printsym (quote "End")
      _ = printsym (quote "NEWLINE")
  in compressMass shifted
