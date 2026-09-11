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

      -- Untimed: diffPW's input. The combined program timed this as
      -- its own pass; here only diffPW is measured.
      shifted = addConstPW pfTree 10

      _ = printsym (quote "Running pass diffPW (map, uses=8, shared=4): ")
      _ = printsym (quote "NEWLINE")
      _diffed = iterate (diffPW shifted)
      _ = printsym (quote "End")
      _ = printsym (quote "NEWLINE")
  in compressMass _diffed
