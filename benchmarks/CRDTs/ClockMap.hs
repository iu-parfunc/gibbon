{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module ClockMap where
import Common
import Clock

type Size = Int
data ClockMap a  = Tip
            | Bin Size Clock a (ClockMap a) (ClockMap a)

-- Construction -----------------------

empty :: ClockMap a
empty = Tip

singleton :: Clock -> a -> ClockMap a
singleton k x = Bin 1 k x Tip Tip


-- Query ------------------------------

null :: ClockMap a -> Bool
null m = case m of
            Tip -> True
            Bin _ _ _ _ _ -> False

size :: ClockMap a -> Size
size m = case m of
            Tip -> 0
            Bin sz _ _ _ _ -> sz

key :: ClockMap a -> Clock
key s = case s of
            Tip -> (Clock.init 0)
            Bin _ k _ _ _ -> k

lookup :: Clock -> ClockMap a -> Common.Maybe a
lookup k m = 
    case m of
        Tip -> Common.Nothing
        Bin _ kx v l r ->
            case (Clock.compare k kx) of
                Eq -> Common.Just v
                Lt -> ClockMap.lookup k l
                Gt -> ClockMap.lookup k r
                Cc -> ClockMap.lookup k r

member :: Clock -> ClockMap a -> Bool
member k m = case (ClockMap.lookup k m) of
    Common.Nothing -> False
    Common.Just _  -> True

-- Insertion --------------------------

insert :: Clock -> a -> ClockMap a -> ClockMap a
insert kx x m =
    case m of
        Tip -> singleton kx x
        Bin sz k v l r ->
            case (Clock.compare k kx) of 
                Eq -> Bin sz k x l r
                Lt -> balance k v (insert kx x l) r
                Gt -> balance k v l (insert kx x r)
                Cc -> balance k v l (insert kx x r)        

delta :: Int
delta = 4
ratio :: Int
ratio = 2

balance :: Clock -> a -> ClockMap a -> ClockMap a -> ClockMap a
balance k x l r =
        if (size l) + (size r) <= 1           then Bin ((size l) + (size r)) k x l r
        else if (size r) >= delta*(size l)    then rotateL k x l r
        else if (size l) >= delta*(size r)    then rotateR k x l r
        else                                       Bin ((size l) + (size r)) k x l r  

rotateL :: Clock -> b -> ClockMap b -> ClockMap b -> ClockMap b
rotateL k x l r =
    case r of 
        Bin _ _ _ ly ry ->
            if (size ly) < ratio*(size ry) then singleL k x l r
            else                                doubleL k x l r
        Tip -> empty -- cry
rotateR :: Clock -> b -> ClockMap b -> ClockMap b -> ClockMap b
rotateR k x l r =
    case l of
        Bin _ _ _ ly ry ->
            if (size ry) < ratio*(size ly) then singleR k x l r
            else                                doubleR k x l r
        Tip -> empty --cry

bin :: Clock -> a -> ClockMap a -> ClockMap a -> ClockMap a
bin k x l r = Bin ((size l) + (size r) + 1) k x l r

singleL :: Clock -> b -> ClockMap b -> ClockMap b -> ClockMap b
singleL k1 x1 t1 m =
    case m of 
        Bin _ k2 x2 t2 t3 -> bin k2 x2 (bin k1 x1 t1 t2) t3
        Tip -> empty --cry

singleR :: Clock -> b -> ClockMap b -> ClockMap b -> ClockMap b
singleR k1 x1 m t3 =
    case m of
        Bin _ k2 x2 t1 t2 -> bin k2 x2 t1 (bin k1 x1 t2 t3)
        Tip -> empty --cry

doubleL :: Clock -> b -> ClockMap b -> ClockMap b -> ClockMap b
doubleL k1 x1 t1 m0 =
    case m0 of
        Bin _ k2 x2 m1 t4 -> 
            case m1 of 
                Bin _ k3 x3 t2 t3 -> bin k3 x3 (bin k1 x1 t1 t2) (bin k2 x2 t3 t4)
                Tip -> empty --cry
        Tip -> empty --cry

doubleR :: Clock -> b -> ClockMap b -> ClockMap b -> ClockMap b
doubleR k1 x1 m0 t4 =
    case m0 of
        Bin _ k2 x2 t1 m1 -> 
            case m1 of
                Bin _ k3 x3 t2 t3 -> bin k3 x3 (bin k2 x2 t1 t2) (bin k1 x1 t3 t4)
                Tip -> empty
        Tip -> empty --cry

gibbon_main = 
    let clk0 = Clock.init 0
        m0 = singleton clk0 "a"
        clk1 = step 1 clk0
        m1 = insert clk1 "a"
        clk2 = step 2 clk1
        m2 = insert clk2 "a"
        clk3 = step 0 clk2
        m3 = insert clk3 "b"
        clk4 = step 1 clk3
        m4 = insert clk4 "b"
        clk5 = step 2 clk4
        m5 = insert clk5 "b"
    in m5
