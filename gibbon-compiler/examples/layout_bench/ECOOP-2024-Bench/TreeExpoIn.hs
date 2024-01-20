module TreeExpo where

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
		    Node l val r -> let 
					sl = expoTree l 
				        a1 = expo2 val
					sr = expoTree r
                                      in Node sl a1 sr 
		    Nil -> Nil 



gibbon_main = let 
		tree = mkTree 27 
	        sum = iterate (expoTree tree)
               in () 
