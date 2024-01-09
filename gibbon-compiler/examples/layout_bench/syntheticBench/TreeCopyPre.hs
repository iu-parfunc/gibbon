module TreeSum where

data PackedInt = P Int
data Tree = Node PackedInt Tree Tree | Leaf  


mkTree :: Int -> Tree 
mkTree depth = if depth <= 0
	       then Leaf
	       else 
		 let 
	            val = P depth
		    left = mkTree (depth-1)
	            right = mkTree (depth-1)
		   in Node val left right 


copyPackedInt :: PackedInt -> PackedInt 
copyPackedInt a = case a of 
			P b -> P b 

copyTree :: Tree -> Tree  
copyTree tree = case tree of 
		    Node val l r -> let
				      newVal = copyPackedInt val	 
				      sl = copyTree l 
				      sr = copyTree r
				     in Node newVal sl sr 
		    Leaf -> Leaf



gibbon_main = let 
		tree = mkTree 27 
	        newTree = iterate (copyTree tree)
               in () --printPacked newTree
