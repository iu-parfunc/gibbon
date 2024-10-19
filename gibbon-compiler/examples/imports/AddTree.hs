module AddTree where
    data Tree = Node Tree Tree | Leaf Int

    sum :: Tree -> Int
    sum t =
        case t of
            Leaf v -> v
            Node l r -> (sum l) + (sum r)