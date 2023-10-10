module Clock where
import Map

type Clock = Map Int --Int

data Timestamp = Timestamp Int Int

init :: Int -> Clock
init uid = singleton uid 0

step :: Int -> Clock -> Clock
step uid clk = case (lookup uid clk) of
                    Just v -> insert uid (v + 1) clk
                    Nothing -> insert uid 1 clk

stamp :: Int -> Clock -> Timestamp
stamp uid clk = case (lookup uid clk) of
                    Just v -> Timestamp uid v
                    Nothing -> Timestamp uid 0
    
gibbon_main = stamp 0 (step 0 (step 1 (step 0 (init 0))))