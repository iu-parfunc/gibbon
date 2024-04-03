{-# LANGUAGE BlockArguments #-}

module IntOrSet where
import qualified Timekeeping as T
import Common
import qualified Map as M
import qualified Set as S

data IntOrSet = OrSet T.Clock (M.Map T.Timestamp) S.IntSet

getClock :: IntOrSet -> T.Clock
getClock x = case x of
    OrSet clk _ _  -> clk

getState :: IntOrSet -> (M.Map T.Timestamp)
getState x = case x of
    OrSet _ s _ -> s

getDelSet :: IntOrSet -> S.IntSet
getDelSet x = case x of
    OrSet _ _ delset -> delset

init :: T.Timestamp -> Int -> IntOrSet
init el uid = 
    OrSet (T.step uid (T.init uid)) (M.singleton (T.serializetimestamp el) (T.stamp uid (T.init uid))) S.EmptySet

add :: T.Timestamp -> Int -> IntOrSet -> IntOrSet
add el uid s = 
    let clk = getClock s
        timestamp = M.lookup (T.serializetimestamp el) (getState s)
    in OrSet (T.step uid clk) (M.insert (T.serializetimestamp el) (T.stamp uid clk) (getState s)) 
        case timestamp of
            Just timestamp -> S.insert (T.serializetimestamp timestamp) (getDelSet s)
            Nothing -> (getDelSet s)

remove ::  T.Timestamp -> Int -> IntOrSet -> IntOrSet
remove el uid s = case (M.lookup (T.serializetimestamp el) (getState s)) of 
        Just timestamp -> OrSet (T.step uid (getClock s)) (M.delete (T.serializetimestamp el) (getState s)) (S.insert (T.serializetimestamp timestamp) (getDelSet s))
        Nothing -> s

--merge :: Ord eltype => ORSet eltype -> ORSet eltype -> ORSet eltype

buildValue :: (M.Map T.Timestamp) -> S.IntSet -> S.IntSet
buildValue m s = case m of
                    M.Tip -> s
                    M.Bin _ k v l r -> buildValue l (S.insert k (buildValue r s))

value :: IntOrSet -> S.IntSet
value x = case x of
    OrSet _ s _ -> (buildValue s S.EmptySet)