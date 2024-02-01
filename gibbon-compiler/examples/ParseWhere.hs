-- Test that the where clause parses and evaluates.
-- Three places you can see where:
-- - top-level function definition
-- - let binding
-- - case alternative

module ParseWhere where

data MyBool = B Bool

f x = y + case B True of
    B _ -> z
      where z = 0
  where
    y = x+1

g x = let
  y = z
    where z = -1
  in x + y

gibbon_main = f 1 + g 1

main :: IO ()
main = print gibbon_main
