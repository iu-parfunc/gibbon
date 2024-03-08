{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Map where
import Common as C

type Size = Int
data Map a  = Tip | Bin Size Int a (Map a) (Map a)

-- Construction -----------------------

empty :: Map a
empty = Tip

singleton :: Int -> a -> Map a
singleton k x = Bin 1 k x Tip Tip


-- Query ------------------------------

null :: Map a -> Bool
null m = case m of
            Tip -> True
            Bin _ _ _ _ _ -> False

size :: Map a -> Size
size m = case m of
            Tip -> 0
            Bin sz _ _ _ _ -> sz

key :: Map a -> Int
key s = case s of
            Tip -> 0
            Bin _ k _ _ _ -> k

lookup :: Int -> Map a -> C.Maybe a
lookup k m = 
    case m of
        Tip ->  C.Nothing
        Bin _ kx v l r ->
            if k < kx then Map.lookup k l
            else if k > kx then Map.lookup k r
            else  C.Just v

member :: Int -> Map a -> Bool
member k m = case Map.lookup k m of
     C.Nothing -> False
     C.Just _  -> True

-- Insertion --------------------------

insert :: Int -> a -> Map a -> Map a
insert kx x m =
    case m of
        Tip -> singleton kx x
        Bin sz k v l r ->
            if kx == k then Bin sz k x l r
            else if kx <= k then 
                balance k v (insert kx x l) r
            else 
                balance k v l (insert kx x r)

delete :: Int -> Map a -> Map a
delete kx m = 
    case m of
        Tip -> Tip
        Bin sz k v l r ->
            if kx == k then glue l r
            else if kx <= k then balance kx v (delete kx l) r
            else balance kx v l (delete kx r)

glue :: Map a -> Map a -> Map a
glue l r = 
    case l of
        Tip -> r
        Bin _ _ _ _ _ -> l
    {-
    case l r of
        (Tip, Tip) -> Tip
        (Bin _ _ _ _ _, Tip) -> r
        (Tip, Bin _ _ _ _ _) -> l
        (Bin _ _ _ _ _, Bin _ _ _ _ _) ->
            if size l > size r then 
                case (deleteFindMax l) of 
                     Just ((km, m),l') -> balance km m l' r
                     Nothing -> Tip
                    --let ((km,m),l') = deleteFindMax l in balance km m l' r
            else 
                case (deleteFindMin r) of
                     Just ((km,m),r') -> balance km m l r'
                     Nothing -> Tip
                
                --let ((km,m),r') = deleteFindMin r in balance km m l r'
    -}

deleteFindMin :: Map a -> (C.Maybe ((Int,a),Map a))
deleteFindMin t = case t of
        Tip             -> C.Nothing
        Bin _ k x l r   -> case l of 
                            Tip -> C.Just ((k,x),r)
                            _ -> case (deleteFindMin l) of
                                C.Nothing -> C.Nothing
                                C.Just res -> 
                                    let (kv, l') = res
                                        (delk, delv) = kv
                                    in C.Just ((delk, delv), (balance k x l' r))
            
            --let (km,l') = deleteFindMin l in  Just (km,balance k x l' r)

deleteFindMax :: Map a -> (C.Maybe ((Int,a),Map a))
deleteFindMax t = case t of
        Tip             ->  C.Nothing
        Bin _ k x l r   -> case r of
                            Tip -> C.Just ((k,x),l)
                            _ -> case (deleteFindMax r) of 
                                C.Nothing ->  C.Nothing
                                C.Just res ->  
                                    let (kv, r') = res
                                        (delk, delv) = kv
                                    in C.Just ((delk, delv), (balance k x l r'))
                             
        --let (km,r') = deleteFindMax r in  Just (km,balance k x l r')

delta :: Int
delta = 4
ratio :: Int
ratio = 2

balance :: Int -> a -> Map a -> Map a -> Map a
balance k x l r =
        if (size l) + (size r) <= 1           then Bin ((size l) + (size r)) k x l r
        else if (size r) >= delta*(size l)    then rotateL k x l r
        else if (size l) >= delta*(size r)    then rotateR k x l r
        else                                        Bin ((size l) + (size r)) k x l r  

rotateL :: Int -> b -> Map b -> Map b -> Map b
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

bin :: Int -> a -> Map a -> Map a -> Map a
bin k x l r = Bin ((size l) + (size r) + 1) k x l r

singleL :: Int -> b -> Map b -> Map b -> Map b
singleL k1 x1 t1 m =
    case m of 
        Bin _ k2 x2 t2 t3 -> bin k2 x2 (bin k1 x1 t1 t2) t3
        Tip -> empty --cry

singleR :: Int -> b -> Map b -> Map b -> Map b
singleR k1 x1 m t3 =
    case m of
        Bin _ k2 x2 t1 t2 -> bin k2 x2 t1 (bin k1 x1 t2 t3)
        Tip -> empty --cry

doubleL :: Int -> b -> Map b -> Map b -> Map b
doubleL k1 x1 t1 m0 =
    case m0 of
        Bin _ k2 x2 m1 t4 -> 
            case m1 of 
                Bin _ k3 x3 t2 t3 -> bin k3 x3 (bin k1 x1 t1 t2) (bin k2 x2 t3 t4)
                Tip -> empty --cry
        Tip -> empty --cry

doubleR :: Int -> b -> Map b -> Map b -> Map b
doubleR k1 x1 m0 t4 =
    case m0 of
        Bin _ k2 x2 t1 m1 -> 
            case m1 of
                Bin _ k3 x3 t2 t3 -> bin k3 x3 (bin k2 x2 t1 t2) (bin k1 x1 t3 t4)
                Tip -> empty
        Tip -> empty --cry

gibbon_main = insert 0 "f" (insert 5 "f" (insert 4 "e" (insert 3 "d" (insert 2 "c" (insert 1 "b" (insert 0 "a" empty))))))