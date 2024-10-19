module Timekeeping where
import qualified Map as M
import qualified Common as C

data Clock = Clk Int (M.Map Int)

data Timestamp = Timestamp Int Clock

serializetimestamp :: Timestamp -> Int
serializetimestamp x = case x of 
    Timestamp uid clk -> case clk of
        Clk _ m -> (M.sum m) + 10000 * uid

clock :: Timestamp -> Clock
clock t = case t of
        Timestamp _ c -> c

author :: Timestamp -> Int
author t =
    case t of
        Timestamp a _ -> a

init :: Int -> Clock
init uid = Clk 0 (M.singleton uid 0)

lookup :: Int -> Clock -> C.Maybe Int
lookup k clk = 
    case clk of
        Clk _ m -> M.lookup k m

clockmap :: Clock -> (M.Map Int)
clockmap x = case x of 
    Clk _ y -> y

step :: Int -> Clock -> Clock
step uid clk = case (Timekeeping.lookup uid clk) of
                    C.Just v -> case clk of 
                        Clk _ m -> Clk 0 (M.insert uid (v + 1) m)
                    C.Nothing -> case clk of
                        Clk _ m -> Clk 0 (M.insert uid 1 m)

stamp :: Int -> Clock -> Timestamp
stamp uid clk = Timestamp uid clk

compare :: Clock -> Clock -> C.Ord
compare a b = 
    case (clockmap a) of
        M.Tip -> case (clockmap b) of
            M.Tip -> C.Eq
            M.Bin _ _ _ _ _ -> C.Lt
        M.Bin _ _ _ _ _ -> case (clockmap b) of
            M.Tip -> C.Gt
            M.Bin _ _ _ _ _ ->
                let k = M.key (clockmap a)
                    diff = case Timekeeping.lookup k a of
                            C.Nothing -> case Timekeeping.lookup k b of
                                C.Nothing -> C.Eq
                                C.Just _ -> C.Lt
                            C.Just ax -> case Timekeeping.lookup k b of
                                C.Nothing -> C.Gt
                                C.Just bx -> C.compareInt ax bx
                in case diff of 
                    C.Lt -> case diff of 
                        C.Lt -> C.Lt
                        C.Eq -> C.Lt
                        _ -> C.Cc
                    C.Gt -> case diff of
                        C.Gt -> C.Gt
                        C.Eq -> C.Gt
                        _ -> C.Cc
                    C.Eq -> case diff of
                        C.Eq -> C.Eq
                        _ -> C.Cc
                    C.Cc -> C.Cc
            

    
--gibbon_main = stamp 0 (step 0 (step 1 (step 0 (init 0))))

--main = stamp 0 (step 0 (step 1 (step 0 (Clock.init 0))))