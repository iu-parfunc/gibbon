{-# LANGUAGE TemplateHaskell #-}

module List where

data Node a = Tip | Bin a (Node a)

singleton :: a -> Node a
singleton x = Bin x Tip

insert :: Int -> a -> Node a -> Node a
insert idx x s = 
    case s of
        Bin v n -> 
            if idx == 0 then Bin x s
            else Bin v (insert (idx-1) x n)
        Tip -> Bin x Tip

remove :: Int -> Node a -> Node a
remove idx s =
    case s of
        Bin v n ->
            if idx == 0 then n
            else Bin v (remove (idx-1) n)
        Tip -> s

gibbon_main = (insert 5 0 (insert 4 1 (insert 3 2 (insert 2 3 (insert 1 4 (singleton 5))))))