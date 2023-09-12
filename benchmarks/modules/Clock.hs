module Clock where
    import Data.Map as Map
    import Data.Set as Set

    data Order = Eq | Lt | Gt | Cc
        deriving (Eq, Ord, Enum, Read, Show)

    type Clock = Map Int Int

    data Timestamp = Timestamp {
            uid :: Int,
            v :: Int
        }
        deriving (Eq, Ord, Show)

    init :: Int -> Clock
    init uid = Map.insert uid 0 Map.empty

    step :: Int -> Clock -> Clock
    step uid clk = case v of
                        Just v -> Map.insert uid (v + 1) clk
                        Nothing -> Map.insert uid 1 clk
                    where v = Map.lookup uid clk

    stamp :: Int -> Clock -> Timestamp
    stamp uid clk = case v of
                        Just v -> Timestamp { uid = uid, v = v }
                        Nothing -> Timestamp { uid = uid, v = 0 }
                    where v = Map.lookup uid clk
    