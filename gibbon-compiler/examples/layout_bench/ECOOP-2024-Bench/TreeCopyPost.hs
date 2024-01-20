module TreeSum where

data PackedInt = P Int
data Tree = Node Tree Tree PackedInt | Leaf  


mkTree :: Int -> Tree 
mkTree depth = if depth <= 0
	       then Leaf
	       else 
		 let
		    left = mkTree (depth-1)
	            right = mkTree (depth-1)
	            val = P depth
		   in Node left right val 


copyPackedInt :: PackedInt -> PackedInt 
copyPackedInt a = case a of 
			P b -> P b 

copyTree :: Tree -> Tree  
copyTree tree = case tree of 
		    Node l r val -> let	 
				      sl = copyTree l 
				      sr = copyTree r
				      newVal = copyPackedInt val
				     in Node sl sr newVal 
		    Leaf -> Leaf



gibbon_main = let 
		tree = mkTree 27 
	        newTree = iterate (copyTree tree)
               in () --printPacked newTree
