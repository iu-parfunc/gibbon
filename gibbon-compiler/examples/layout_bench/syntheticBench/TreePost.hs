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

sumPackedInt :: PackedInt -> PackedInt -> PackedInt 
sumPackedInt a b = case a of 
			P c -> case b of 
				P d -> P (c + d)

addOne :: PackedInt -> PackedInt 
addOne val = case val of 
		 P b -> P (b+1)

sumTree :: Tree -> Tree
sumTree tree = case tree of 
		    Node l r val -> let 
					sl = sumTree l 
					sr = sumTree r
					a1 = addOne val
                                      in Node sl sr a1
		    Nil -> Nil



gibbon_main = let
                tree = mkTree 25
                sum = iterate (sumTree tree)
               in () 
