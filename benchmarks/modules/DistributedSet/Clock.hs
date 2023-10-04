module Clock where
    import GibbonMap
    import Gibbon.Prim

    data Order = Eq | Lt | Gt | Cc
        deriving (Eq, Ord, Enum, Read, Show)

    type Clock = Map Int Int

    data Timestamp = Timestamp {
            uid :: Int,
            v :: Int
        }
        deriving (Eq, Ord, Show)

    init :: Int -> Clock
    init uid = GibbonMap.insert uid 0 GibbonMap.empty

    step :: Int -> Clock -> Clock
    step uid clk = case v of
                        Just v -> GibbonMap.insert uid (v + 1) clk
                        Nothing -> GibbonMap.insert uid 1 clk
                    where v = GibbonMap.lookup uid clk

    stamp :: Int -> Clock -> Timestamp
    stamp uid clk = case v of
                        Just v -> Timestamp { uid = uid, v = v }
                        Nothing -> Timestamp { uid = uid, v = 0 }
                    where v = GibbonMap.lookup uid clk
    
gibbon_main = stamp 0 (step 0 (step 1 (step 0 (init 0))))