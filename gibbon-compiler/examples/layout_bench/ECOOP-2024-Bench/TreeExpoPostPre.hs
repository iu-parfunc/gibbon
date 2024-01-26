module TreeExpo where

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

expo :: Int -> Int -> Int
{-# INLINE expo #-}
expo power val = if power == 0
                then val
                else val * expo (power-1) val


expo2 :: PackedInt -> PackedInt
expo2 val = case val of
                P x -> P (expo 10 x)

expoTree :: Tree -> Tree
expoTree tree = case tree of 
		    Node l r val -> let 
					a1 = expo2 val
					sl = expoTree l 
					sr = expoTree r
                                      in Node sl sr a1 
		    Nil -> Nil 



gibbon_main = let 
		tree = mkTree 27 
	        sum = iterate (expoTree tree)
               in () 
