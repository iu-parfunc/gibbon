data Tree = Leaf Int | Node Tree Tree

add1 :: Tree -> Tree
add1 (Leaf x)     = Leaf (x + 1)
add1 (Node x1 x2) = Node (add1 x1) (add1 x2)
