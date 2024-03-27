module Add where
data Tree = Leaf Int | Node Tree Tree

add1 :: Tree -> Tree
add1 t = case t of
           Leaf x     -> Leaf (x + 1)
           Node x1 x2 -> Node (Add.add1 x1) (Add.add1 x2)

gibbon_main :: Tree
gibbon_main = Add.add1 (Node (Leaf 1) (Leaf 2))
