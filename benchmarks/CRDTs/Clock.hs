module Clock where
import Map
import Common

type Clock = Map Int
data Timestamp = Timestamp Int Clock

clk :: Timestamp -> Clock
clk t = 
    case t of
        Timestamp _ c -> c

init :: Int -> Clock
init uid = singleton uid 0

step :: Int -> Clock -> Clock
step uid clk = case (Map.lookup uid clk) of
                    Common.Just v -> Map.insert uid (v + 1) clk
                    Common.Nothing -> Map.insert uid 1 clk

stamp :: Int -> Clock -> Timestamp
stamp uid clk = Timestamp uid clk

compare :: Clock -> Clock -> Common.Ord
compare a b = 
    case (a, b) of
        (Tip, Tip) -> Eq
        (Tip, Bin _ _ _ _ _) -> Lt
        (Bin _ _ _ _ _, Tip) -> Gt
        (Bin _ _ _ _ _, Bin _ _ _ _ _) ->
            let k = key a
                diff = case ((Map.lookup k a), (Map.lookup k b)) of
                    (Common.Nothing, Common.Nothing) -> Eq -- should never get here
                    (Common.Nothing, Common.Just _) -> Lt
                    (Common.Just _, Common.Nothing) -> Gt
                    (Common.Just ax, Common.Just bx) -> compareInt ax bx
            in
                case ((Clock.compare (delete k a) (delete k b)), diff) of
                    (Cc, _) -> Cc
                    (_, Cc) -> Cc
                    (Lt, Lt) -> Lt
                    (Gt, Gt) -> Gt
                    (Eq, Eq) -> Eq
                    (Eq, Lt) -> Lt
                    (Eq, Gt) -> Gt
                    (_, _) -> Cc
            

    
--gibbon_main = stamp 0 (step 0 (step 1 (step 0 (init 0))))

main = stamp 0 (step 0 (step 1 (step 0 (Clock.init 0))))