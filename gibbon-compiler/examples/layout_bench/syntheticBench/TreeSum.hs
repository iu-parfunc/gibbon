module TreeSum where

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



getInt :: PackedInt -> Int 
getInt p = case p of 
		P val -> val


sumPackedInt :: PackedInt -> PackedInt -> PackedInt 
sumPackedInt a b = let 
		     aa = getInt a 
		     bb = getInt b 
		    in P (aa + bb) 


sumTree :: Tree -> PackedInt  
sumTree tree = case tree of 
		    Node val l r -> let 
				      sl = sumTree l 
				      sr = sumTree r
				      sum1 = sumPackedInt sr sl  
                                      in sumPackedInt sum1 val 
		    Nil -> P 0 



gibbon_main = let 
		tree = mkTree 18 
	        sum = iterate (sumTree tree)
               in () 
