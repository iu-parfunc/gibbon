module Clock where
import Map as M
import Common as C

data Clock = Clk (M.Map Int)

data Timestamp = Timestamp Int Clock

clock :: Timestamp -> Clock
clock t = case t of
        Timestamp _ c -> c

author :: Timestamp -> Int
author t =
    case t of
        Timestamp a _ -> a

init :: Int -> Clock
init uid = Clk (M.singleton uid 0)

lookup :: Int -> Clock -> C.Maybe Int
lookup k clk = 
    case clk of
        Clk m -> M.lookup k m

clockmap :: Clock -> M.Map Int
clockmap x = case x of 
    Clk y -> y

step :: Int -> Clock -> Clock
step uid clk = case (Clock.lookup uid clk) of
                    C.Just v -> case clk of 
                        Clk m -> Clk (M.insert uid (v + 1) m)
                    C.Nothing -> case clk of
                        Clk m -> Clk (M.insert uid 1 m)

stamp :: Int -> Clock -> Timestamp
stamp uid clk = Timestamp uid clk

compare :: Clock -> Clock -> C.Ord
compare a b = 
    case am of
        Tip -> case bm of
            Tip -> Eq
            Bin _ _ _ _ _ -> Lt
        Bin _ _ _ _ _ -> case bm of
            Tip -> Gt
            Bin _ _ _ _ _ ->
                let k = key am
                    diff = case M.lookup k am of
                            C.Nothing -> case M.lookup k bm of
                                C.Nothing -> Eq
                                C.Just _ -> Lt
                            C.Just ax -> case M.lookup k bm of
                                C.Nothing -> Gt
                                C.Just bx -> C.compareInt ax bx
                in case diff of 
                    Lt -> case diff of 
                        Lt -> Lt
                        Eq -> Lt
                        _ -> Cc
                    Gt -> case diff of
                        Gt -> Gt
                        Eq -> Gt
                        _ -> Cc
                    Eq -> case diff of
                        Eq -> Eq
                        _ -> Cc
                    Cc -> Cc
    where   am = clockmap a
            bm = clockmap b
            

    
--gibbon_main = stamp 0 (step 0 (step 1 (step 0 (init 0))))

main = stamp 0 (step 0 (step 1 (step 0 (Clock.init 0))))