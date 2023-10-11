{-# LANGUAGE BlockArguments #-}
module IntOrMap where
import Clock
import Map


data IntOrMap = OrMap Clock Map Timestamp Set Timestamp

getClock :: IntOrMap -> Clock
getClock OrMap clk _ _ = clk

getState :: IntOrMap -> Map Timestamp
getClock OrMap _ s _ = s

getDelSet :: IntOrMap -> Set Timestamp
getDelSet OrMap _ _ delset = delset

init :: Timestamp -> Int -> IntOrMap
init uid el = 
    let clk = init uid
    in OrMap (step uid clk) (singleton el (stamp uid clk)) empty

add :: Timestamp -> Int -> IntOrMap -> IntOrMap
add el s = 
    let clk = getClock s
        timestamp = lookup el (getState s)
    in OrMap (step uid clk) (insert el (stamp uid clk) (getState s)) 
        case timestamp of
            Just timestamp -> insert timestamp (getDelSet s)
            Noting -> (getDelSet s)

remove :: Timestamp -> eltype -> OrMap eltype -> OrMap eltype
remove el s = 
    let clk = getClock s
    in case (lookup el (getState s)) of 
        Just timestamp -> OrMap (step uid clk) (delete el (getState s)) (insert timestamp (getDelSet s))
        Nothing -> s

--merge :: Ord eltype => OrMap eltype -> OrMap eltype -> OrMap eltype

value :: IntOrMap -> IntSet
value OrMap _ s _ = buildValue s empty

buildValue :: Map -> IntSet -> IntSet
buildValue m s = case m of
                    Tip -> s
                    Bin _ _ v l r -> (buildValue r (insert v (buildValue r s)))
