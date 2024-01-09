module TreeAddOne where


data PackedInt = P Int
data Tree = Node PackedInt Tree Tree | Nil 


mkTree :: Int -> Tree 
mkTree depth = if depth <= 0
	       then Nil 
	       else 
		 let 
	            val = P depth
		    left = mkTree (depth-1)
	            right = mkTree (depth-1)
		   in Node val left right

addOne :: PackedInt -> PackedInt
addOne val = case val of
                 P b -> P (b+1)

addOneTree :: Tree -> Tree
addOneTree tree = case tree of 
		    Node val l r -> let 
					a1 = addOne val
					sl = addOneTree l 
					sr = addOneTree r
                                      in Node a1 sl sr 
		    Nil -> Nil 



gibbon_main = let 
		tree = mkTree 27 
	        newTree = iterate (addOneTree tree)
               in ()
