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
		    right = mkTree (depth-1)
                    left = mkTree (depth-1)
		   in Node right left 


copyPackedInt :: PackedInt -> PackedInt 
copyPackedInt a = case a of 
			P b -> P b 

rightMostTree :: Tree -> PackedInt  
rightMostTree tree = case tree of 
		    	Node r l -> let
				      rightMostVal = rightMostTree r
			             in rightMostVal  
		    	Leaf val -> val 

rightMostTreeRec :: Int -> Tree -> PackedInt
rightMostTreeRec iters tree = if iters <= 0
                              then rightMostTree tree
                              else let
                                    val = rightMostTree tree
                                 in rightMostTreeRec (iters-1) tree

gibbon_main = let 
		tree = mkTree 29
	        newTree = iterate (rightMostTree tree) --(rightMostTreeRec 1 tree)
               in () --printPacked newTree
