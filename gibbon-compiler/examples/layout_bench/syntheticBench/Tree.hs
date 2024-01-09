
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



sumPackedInt :: PackedInt -> PackedInt -> PackedInt 
sumPackedInt a b = case a of 
			P c -> case b of 
				P d -> P (c + d)

addOne :: PackedInt -> PackedInt
addOne val = case val of
                 P b -> P (b+1)

expo :: Int -> Int -> Int
{-# INLINE expo #-}
expo power val = if power == 0
                then val
                else val * expo (power-1) val


expo2 :: PackedInt -> PackedInt
expo2 val = case val of
                P x -> P (expo 10 x)

sumTree :: Tree -> Tree
sumTree tree = case tree of 
		    Node val l r -> let 
					a1 = expo2 val
					sl = sumTree l 
					sr = sumTree r
                                      in Node a1 sl sr 
		    Nil -> Nil 



gibbon_main = let 
		tree = mkTree 26 
	        sum = iterate (sumTree tree)
               in () 
