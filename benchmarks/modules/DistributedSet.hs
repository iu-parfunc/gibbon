module DistributedSet where
    import Clock
    import Data.Set as Set
    import Data.Map as Map

    data ORSet eltype = ORSet {
        clock :: Clock.Clock,
        state:: Map eltype Clock.Timestamp,
        ds :: Set Clock.Timestamp
    }
        deriving(Show)

    init :: Ord eltype => Timestamp -> eltype -> ORSet eltype
    init id el = ORSet {
        clock = Clock.step (uid id) $ Clock.init $ uid id,
        state = Map.insert el id $ Map.empty,
        ds = Set.empty
    }

    add :: Ord eltype => Timestamp -> eltype -> ORSet eltype -> ORSet eltype
    add id el s = ORSet {
        ds = case timestamp of 
                    Just timestamp -> Set.insert timestamp $ ds s
                    Nothing -> ds s,
        clock = Clock.step (uid id) $ clock s,
        state = Map.insert el id $ state s
        }
        where timestamp = Map.lookup el $ state s

    remove :: Ord eltype => Timestamp -> eltype -> ORSet eltype -> ORSet eltype
    remove id el s = case timestamp of 
                        Just timestamp -> 
                            ORSet {
                                clock = Clock.step (uid id) $ clock s,
                                state = Map.delete el $ state s,
                                ds =  Set.insert timestamp $ ds s
                            }
                        Nothing -> s
                    where timestamp = Map.lookup el (state s)

    value :: Ord eltype => ORSet eltype -> Set eltype
    value s = Set.fromList $ Map.keys $ state s