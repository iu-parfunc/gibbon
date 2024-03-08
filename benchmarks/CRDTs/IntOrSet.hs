{-# LANGUAGE BlockArguments #-}

module IntOrSet where
import Clock
import Common
import Map as M
import Set as S

data IntOrSet = OrSet Clock (M.Map Timestamp) (IntSet Timestamp)

getClock :: IntOrSet -> Clock
getClock x = case x of
    OrSet clk _ _ _  -> clk

getState :: IntOrSet -> M.Map Timestamp
getState x = case x of
    OrSet _ s _ -> s
    _ -> M.empty

getDelSet :: IntOrSet -> IntSet Timestamp
getDelSet x = case x of
    OrSet _ _ delset -> delset

init :: Timestamp -> Int -> IntOrSet
init uid el = 
    let clk = init uid
    in OrSet (step uid clk) (singleton el (stamp uid clk)) S.empty

add :: Timestamp -> Int -> IntOrSet -> IntOrSet
add el s = 
    let clk = getClock s
        timestamp = lookup el (getState s)
    in OrSet (step uid clk) (insert el (stamp uid clk) (getState s)) 
        case timestamp of
            Just timestamp -> insert timestamp (getDelSet s)
            Nothing -> (getDelSet s)

remove ::  Timestamp -> Int -> IntOrSet -> IntOrSet
remove el s = 
    let clk = getClock s
    in case (lookup el (getState s)) of 
        Just timestamp -> OrSet (step uid clk) (delete el (getState s)) (insert timestamp (getDelSet s))
        Nothing -> s

--merge :: Ord eltype => ORSet eltype -> ORSet eltype -> ORSet eltype

value :: IntOrSet -> IntSet
value x = (buildValue (getState x) S.empty)

buildValue :: (M.Map Timestamp) -> IntSet -> IntSet
buildValue m s = case m of
                    Tip -> s
                    Bin _ k v l r -> buildValue r (S.insert k (buildValue r s))
