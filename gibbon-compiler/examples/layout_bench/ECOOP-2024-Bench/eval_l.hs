import Eval

gibbon_main =
  let
    n = 30
    t = mkDeterministicTree n
  in iterate (eval t)
