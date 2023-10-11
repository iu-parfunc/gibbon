module Clock where
import Map

type Clock = Map Int Int
data Timestamp = Int Clock

init :: Int -> Clock
init uid = singleton uid 0

step :: Int -> Clock -> Clock
step uid clk = case (lookup uid clk) of
                    Just v -> insert uid (v + 1) clk
                    Nothing -> insert uid 1 clk

stamp :: Int -> Clock -> Timestamp
stamp uid clk = uid clk

compare :: Timestamp -> Timestamp -> Ord
    
gibbon_main = stamp 0 (step 0 (step 1 (step 0 (init 0))))