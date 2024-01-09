module TreeAddOne where


data PackedInt = P Int
data Tree = Node Tree Tree PackedInt | Nil 


mkTree :: Int -> Tree 
mkTree depth = if depth <= 0
	       then Nil 
	       else 
		 let
		    left = mkTree (depth-1)
	            right = mkTree (depth-1)
		    val = P depth
		   in Node left right val

addOne :: PackedInt -> PackedInt
addOne val = case val of
                 P b -> P (b+1)

addOneTree :: Tree -> Tree
addOneTree tree = case tree of 
		    Node l r val -> let 
					a1 = addOne val
					sl = addOneTree l 
					sr = addOneTree r
                                      in Node sl sr a1 
		    Nil -> Nil 



gibbon_main = let 
		tree = mkTree 27 
	        newTree = iterate (addOneTree tree)
               in ()
