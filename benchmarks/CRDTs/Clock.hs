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
init uid = Clk (singleton uid 0)

step :: Int -> Clock -> Clock
step uid clk = case (M.lookup uid clk) of
                    C.Just v -> M.insert uid (v + 1) clk
                    C.Nothing -> M.insert uid 1 clk

stamp :: Int -> Clock -> Timestamp
stamp uid clk = Timestamp uid clk

compare :: Clock -> Clock -> C.Ord
compare a b = 
    case a of
        Tip -> case b of
            Tip -> Eq
            Bin _ _ _ _ _ -> Lt
        Bin _ _ _ _ _ -> case b of
            Tip -> Gt
            Bin _ _ _ _ _ ->
                let k = key a
                    diff = case M.lookup k a of
                            C.Nothing -> case M.lookup k b of
                                C.Nothing -> Eq
                                C.Just _ -> Lt
                            C.Just ax -> case M.lookup k b of
                                C.Nothing -> Gt
                                C.Just bx -> C.compareInt ax bx
                    comp = Clock.compare (delete k a) (delete k b)
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
            

    
--gibbon_main = stamp 0 (step 0 (step 1 (step 0 (init 0))))

main = stamp 0 (step 0 (step 1 (step 0 (Clock.init 0))))