
data Tree =   Node   Int Tree Tree
            | NodeL  Int Int Tree Tree
            | NodeR  Int Int Tree Tree 
            | NodeLR Int Int Int Tree Tree  
            | Leaf Int
            | LeafLR Int Int
            | LeafL Int 
            | LeafR Int   


data Tree = Node Int Tree Tree | Leaf Int


(Node Int (Node Int (Leaf Int) (Leaf Int) Node Int (Leaf Int) (Leaf Int))) 

(NodeLR Int Int Int (LeafLR Int Int) (LeafLR Int Int))

(Node Int Int Int Int Int (Leaf Int) (Leaf Int) )


add1 :: Tree -> Tree 
add1 tree = case tree of 
	Node val left right -> Node (val+1) (add1 left) (add1 right) 
        Leaf val -> Leaf (val+1)


mkTree :: Int -> Tree 
mkTree depth = if depth <= 0 then Leaf depth
	       else let left  = mkTree (depth -1)
 			right = mkTree (depth -1)
                     in Node depth left right 

