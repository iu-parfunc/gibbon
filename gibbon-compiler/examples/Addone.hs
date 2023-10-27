module Addone where 
    data Tree = Leaf Int | Node Tree Tree

    add/1 :: Tree -> Tree
    add/1 t = case t of
            Leaf x     -> Leaf (x + 1)
            Node x1 x2 -> Node (add1 x1) (add1 x2)

    main :: Tree
    main = add1 (Node (Leaf 1) (Leaf 2))
