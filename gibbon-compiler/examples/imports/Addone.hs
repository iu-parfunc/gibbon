module Addone where 
    import AddTree

    add :: Tree -> Tree
    add t = case t of
            Leaf x     -> Leaf (x + 1)
            Node x1 x2 -> Node (add x1) (add x2)

    sub :: Tree -> Tree
    sub t = case t of
            Leaf x     -> Leaf (x - 1)
            Node x1 x2 -> Node (sub x1) (sub x2)

    gibbon_main :: Tree
    gibbon_main = add (Node (Leaf 1) (Leaf 2))
