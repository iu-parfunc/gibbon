module TreeRightSum where

data PackedInt = P Int
data Tree = Node Tree Tree | Leaf PackedInt  


mkTree :: Int -> Tree 
mkTree depth = if depth <= 0
	       then let 
		      val = P depth
		     in Leaf val
	       else 
		 let
		    left = mkTree (depth-1)
	            right = mkTree (depth-1)
		   in Node left right 


copyPackedInt :: PackedInt -> PackedInt 
copyPackedInt a = case a of 
			P b -> P b 

rightMostTree :: Tree -> PackedInt  
rightMostTree tree = case tree of 
		    	Node l r -> let
				      rightMostVal = rightMostTree r
			             in rightMostVal  
		    	Leaf val -> val 



gibbon_main = let 
		tree = mkTree 32 
	        newTree = iterate (rightMostTree tree)
               in () --printPacked newTree
