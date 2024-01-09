module TreeSum where

data PackedInt = P Int
data Tree = Node Tree PackedInt Tree | Leaf  


mkTree :: Int -> Tree 
mkTree depth = if depth <= 0
	       then Leaf
	       else 
		 let
		    left = mkTree (depth-1)
                    val = P depth
	            right = mkTree (depth-1)
		   in Node left val right 


copyPackedInt :: PackedInt -> PackedInt 
copyPackedInt a = case a of 
			P b -> P b 

copyTree :: Tree -> Tree  
copyTree tree = case tree of 
		    Node l val r -> let	 
				      sl = copyTree l
				      newVal = copyPackedInt val 
				      sr = copyTree r
				     in Node sl newVal sr 
		    Leaf -> Leaf



gibbon_main = let 
		tree = mkTree 27 
	        newTree = iterate (copyTree tree)
               in () --printPacked newTree
