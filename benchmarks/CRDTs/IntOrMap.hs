{-# LANGUAGE BlockArguments #-}
module IntOrMap where
import qualified Timekeeping as T
import qualified Map as M
import qualified Set as S
import qualified Common as C

data Pair a = Pr T.Timestamp a
data IntOrMap a = OrMap T.Clock (M.Map (Pair a)) (M.Map T.Timestamp)

getClock :: IntOrMap a -> T.Clock
getClock x = case x of
    OrMap clk _ _ -> clk

getState :: IntOrMap a -> M.Map (Pair a)
getState x = case x of
    OrMap _ s _ -> s

getDelSet :: IntOrMap a -> M.Map T.Timestamp
getDelSet x = case x of
    OrMap _ _ delset -> delset

init :: Int -> Int -> a -> IntOrMap a
init uid key val = 
    let clk = (T.init uid)
        v = Pr (T.stamp uid clk) val
    in OrMap (T.step uid clk) (M.singleton key v) M.Tip

add :: Int -> Int -> a -> IntOrMap a -> IntOrMap a
add uid key val s = 
    let clk = (getClock s)
        timestamp = M.lookup key (getState s)
        v = Pr (T.stamp uid clk) val
    in OrMap (T.step uid clk) (M.insert key v (getState s)) (getDelSet s) 

toDelSet :: Int -> T.Timestamp -> (M.Map T.Timestamp) -> (M.Map T.Timestamp)
toDelSet key t delset = (M.insert key t delset)

remove :: Int -> Int -> IntOrMap a -> IntOrMap a
remove uid key s = 
    let clk = (getClock s)
    in case (M.lookup key (getState s)) of 
        C.Just val -> 
            case val of
                Pr t _ -> OrMap (T.step uid clk) (M.delete key (getState s)) (toDelSet key t (getDelSet s))
        C.Nothing -> s



-- --merge :: Ord eltype => OrMap eltype -> OrMap eltype -> OrMap eltype

value :: IntOrMap a -> M.Map a
value s = buildValue (getState s) M.Tip


buildValue :: M.Map (Pair a) -> M.Map a -> M.Map a
buildValue m s = case m of
                    M.Tip -> s
                    M.Bin _ k v l r -> case v of
                        Pr _ val -> (buildValue l (M.insert k val (buildValue r s)))


gibbon_main = 
    let x = (init 0 0 0)
        x = (add 1 9 5 x)
        x = (add 0 2 8 x)
        x = (add 1 3 1 x)
    in value x
