{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module ClockMap where
import Common
import Clock

type Size = Int
data ClockMap a  = Tip
            | Bin Size Int a (ClockMap a) (ClockMap a)

-- Construction -----------------------

empty :: ClockMap a
empty = Tip

singleton :: Int -> a -> ClockMap a
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

key :: ClockMap a -> Int
key s = case s of
            Tip -> 0
            Bin _ k _ _ _ -> k

lookup :: Int -> ClockMap a -> Maybe a
lookup k m = 
    case m of
        Tip -> Nothing
        Bin _ kx v l r ->
            case (compare k kx) of
                Eq -> Just v
                Lt -> lookup k l
                Gt -> lookup k r
                Cc -> lookup k r

member :: Int -> ClockMap a -> Bool
member k m = case (lookup k m) of
    Nothing -> False
    Just _  -> True

-- Insertion --------------------------

insert :: Int -> a -> ClockMap a -> ClockMap a
insert kx x m =
    case m of
        Tip -> singleton kx x
        Bin sz k v l r ->
            case (compare k kx) of 
                Eq -> Bin sz k x l r
                Lt -> balance k v (insert kx x l) r
                Gt -> balance k v l (insert kx x r)
                Cc -> balance k v l (insert kx x r)        

delta :: Int
delta = 4
ratio :: Int
ratio = 2

balance :: Int -> a -> ClockMap a -> ClockMap a -> ClockMap a
balance k x l r =
        if (size l) + (size r) <= 1           then Bin ((size l) + (size r)) k x l r
        else if (size r) >= delta*(size l)    then rotateL k x l r
        else if (size l) >= delta*(size r)    then rotateR k x l r
        else                                       Bin ((size l) + (size r)) k x l r  

rotateL :: Int -> b -> ClockMap b -> ClockMap b -> ClockMap b
rotateL k x l r =
    case r of 
        Bin _ _ _ ly ry ->
            if (size ly) < ratio*(size ry) then singleL k x l r
            else                                doubleL k x l r
        Tip -> empty -- cry
rotateR :: Int -> b -> Map b -> Map b -> Map b
rotateR k x l r =
    case l of
        Bin _ _ _ ly ry ->
            if (size ry) < ratio*(size ly) then singleR k x l r
            else                                doubleR k x l r
        Tip -> empty --cry

bin :: Int -> a -> ClockMap a -> ClockMap a -> ClockMap a
bin k x l r = Bin ((size l) + (size r) + 1) k x l r

singleL :: Int -> b -> ClockMap b -> ClockMap b -> ClockMap b
singleL k1 x1 t1 m =
    case m of 
        Bin _ k2 x2 t2 t3 -> bin k2 x2 (bin k1 x1 t1 t2) t3
        Tip -> empty --cry

singleR :: Int -> b -> ClockMap b -> ClockMap b -> ClockMap b
singleR k1 x1 m t3 =
    case m of
        Bin _ k2 x2 t1 t2 -> bin k2 x2 t1 (bin k1 x1 t2 t3)
        Tip -> empty --cry

doubleL :: Int -> b -> ClockMap b -> ClockMap b -> ClockMap b
doubleL k1 x1 t1 m0 =
    case m0 of
        Bin _ k2 x2 m1 t4 -> 
            case m1 of 
                Bin _ k3 x3 t2 t3 -> bin k3 x3 (bin k1 x1 t1 t2) (bin k2 x2 t3 t4)
                Tip -> empty --cry
        Tip _ _ _ _  -> empty --cry

doubleR :: Int -> b -> ClockMap b -> ClockMap b -> ClockMap b
doubleR k1 x1 m0 t4 =
    case m0 of
        Bin _ k2 x2 t1 m1 -> 
            case m1 of
                Bin _ k3 x3 t2 t3 -> bin k3 x3 (bin k2 x2 t1 t2) (bin k1 x1 t3 t4)
                Tip -> empty
        Tip -> empty --cry

gibbon_main = insert 0 "f" (insert 5 "f" (insert 4 "e" (insert 3 "d" (insert 2 "c" (insert 1 "b" (insert 0 "a" empty))))))