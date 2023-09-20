{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

module GibbonMap where

data Maybe a = Nothing
             | Just a

type Size = Int
data GibbonMap a  = Tip
                  | Bin Size Int a (GibbonMap a) (GibbonMap a)

-- Construction -----------------------

empty :: GibbonMap a
empty = Tip

singleton :: Int -> a -> GibbonMap a
singleton k x = Bin 1 k x Tip Tip


-- Query ------------------------------

null :: GibbonMap a -> Bool
null m = case m of
            Tip -> True
            Bin _ _ _ _ _ -> False

size :: GibbonMap a -> Size
size m = case m of
            Tip -> 0
            Bin sz _ _ _ _ -> sz

lookup :: Int -> GibbonMap a -> Maybe a
lookup k m = 
    case m of
        Tip -> Nothing
        Bin _ kx v l r ->
            if k < kx then lookup k l
            else if k > kx then lookup k r
            else Just v

member :: Int -> GibbonMap a -> Bool
member k m = case lookup k m of
    Nothing -> False
    Just _  -> True

-- Insertion --------------------------

insert :: Int -> a -> GibbonMap a -> GibbonMap a
insert kx x m =
    case m of
        Tip -> singleton kx x
        Bin sz k v l r ->
            if kx == k then Bin sz k x l r
            else if kx <= k then 
                balance k v (insert kx x l) r
            else 
                balance k v l (insert kx x r)

delta :: Int
delta = 4
ratio :: Int
ratio = 2

balance :: Int -> a -> GibbonMap a -> GibbonMap a -> GibbonMap a
balance k x l r =
        if (size l) + (size r) <= 1           then Bin ((size l) + (size r)) k x l r
        else if (size r) >= delta*(size l)    then rotateL k x l r
        else if (size l) >= delta*(size r)    then rotateR k x l r
        else                                        Bin ((size l) + (size r)) k x l r  

rotateL :: Int -> b -> GibbonMap b -> GibbonMap b -> GibbonMap b
rotateL k x l r =
    case r of 
        Bin _ _ _ ly ry ->
            if (size ly) < ratio*(size ry) then singleL k x l r
            else                                doubleL k x l r
        Tip -> empty -- cry
--rotateL _ _ _ Tip = error "rotateL Tip"

rotateR :: Int -> b -> GibbonMap b -> GibbonMap b -> GibbonMap b
rotateR k x l r =
    case l of
        Bin _ _ _ ly ry ->
            if (size ry) < ratio*(size ly) then singleR k x l r
            else                                doubleR k x l r
        Tip -> empty --cry
--rotateR _ _ Tip _ = error "rotateR Tip"

bin :: Int -> a -> GibbonMap a -> GibbonMap a -> GibbonMap a
bin k x l r = Bin ((size l) + (size r) + 1) k x l r

singleL :: Int -> b -> GibbonMap b -> GibbonMap b -> GibbonMap b
singleL k1 x1 t1 m =
    case m of 
        Bin _ k2 x2 t2 t3 -> bin k2 x2 (bin k1 x1 t1 t2) t3
        Tip -> empty --cry
--singleL :: Int -> b -> GibbonMap Int b -> GibbonMap Int b -> GibbonMap Int b
--singleL k1 x1 t1 (Bin _ k2 x2 t2 t3)  = bin k2 x2 (bin k1 x1 t1 t2) t3
--singleL _ _ _ Tip = error "singleL Tip"

singleR :: Int -> b -> GibbonMap b -> GibbonMap b -> GibbonMap b
singleR k1 x1 m t3 =
    case m of
        Bin _ k2 x2 t1 t2 -> bin k2 x2 t1 (bin k1 x1 t2 t3)
        Tip -> empty --cry
--singleR k1 x1 (Bin _ k2 x2 t1 t2) t3  = bin k2 x2 t1 (bin k1 x1 t2 t3)
--singleR _ _ Tip _ = error "singleR Tip"

doubleL :: Int -> b -> GibbonMap b -> GibbonMap b -> GibbonMap b
doubleL k1 x1 t1 m0 =
    case m0 of
        Bin _ k2 x2 m1 t4 -> 
            case m1 of 
                Bin _ k3 x3 t2 t3 -> bin k3 x3 (bin k1 x1 t1 t2) (bin k2 x2 t3 t4)
                Tip -> empty --cry
        Tip _ _ _ _  -> empty --cry
--doubleL k1 x1 t1 (Bin _ k2 x2 (Bin _ k3 x3 t2 t3) t4) = bin k3 x3 (bin k1 x1 t1 t2) (bin k2 x2 t3 t4)
--doubleL _ _ _ _ = error "doubleL"

doubleR :: Int -> b -> GibbonMap b -> GibbonMap b -> GibbonMap b
doubleR k1 x1 m0 t4 =
    case m0 of
        Bin _ k2 x2 t1 m1 -> 
            case m1 of
                Bin _ k3 x3 t2 t3 -> bin k3 x3 (bin k2 x2 t1 t2) (bin k1 x1 t3 t4)
                Tip -> empty
        Tip -> empty --cry
--doubleR k1 x1 (Bin _ k2 x2 t1 (Bin _ k3 x3 t2 t3)) t4 = bin k3 x3 (bin k2 x2 t1 t2) (bin k1 x1 t3 t4)
--doubleR _ _ _ _ = error "doubleR"

--gibbon_main = insert 0 'a' empty
gibbon_main = insert 0 "f" (insert 5 "f" (insert 4 "e" (insert 3 "d" (insert 2 "c" (insert 1 "b" (insert 0 "a" empty))))))