module AddTree where
    data Tree = Leaf Int | Node Tree Tree

    sum :: Tree -> Int
    sum t =
        case t of
            Leaf v -> v
            Node l r -> (sum l) + (sum r)