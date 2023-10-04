
module Main where
import Data.Set as Set
import Data.Set.Internal as Internal

insert_num :: Int -> Set Int -> Set Int
insert_num x s =
    if x == 0   then Set.insert x s
    else insert x (insert_num (x-1) s)

main = putStrLn (show (Set.foldr (+) 0 (insert_num 100 (insert_num 100 Set.empty))))