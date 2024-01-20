module TreeAddOne where


data PackedInt = P Int
data Tree = Node Tree PackedInt Tree | Nil 


mkTree :: Int -> Tree 
mkTree depth = if depth <= 0
	       then Nil 
	       else 
		 let
		    left = mkTree (depth-1)
		    val = P depth
	            right = mkTree (depth-1)
		   in Node left val right

addOne :: PackedInt -> PackedInt
addOne val = case val of
                 P b -> P (b+1)

addOneTree :: Tree -> Tree
addOneTree tree = case tree of 
		    Node l val r -> let 
					sl = addOneTree l
					a1 = addOne val 
					sr = addOneTree r
                                      in Node sl a1 sr 
		    Nil -> Nil 



gibbon_main = let 
		tree = mkTree 27 
	        newTree = iterate (addOneTree tree)
               in ()
