module TreeExpo where

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
		    Node val l r -> let 
					a1 = expo2 val
					sl = expoTree l 
					sr = expoTree r
                                      in Node a1 sl sr 
		    Nil -> Nil 



gibbon_main = let 
		tree = mkTree 26 
	        sum = iterate (expoTree tree)
               in () 
