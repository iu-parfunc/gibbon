
data PackedInt = P Int 

data List = Cons List PackedInt | Nil 

mkList :: Int -> List 
mkList len = if len <=0 
	     then Nil 
	     else 
		let rst = mkList (len-1)
		    val = (P len)
		  in Cons rst val


addOne :: PackedInt -> PackedInt 
addOne val = case val of 
		P x -> P (expo 100 x) --(x+1+4+2 -1)

addVal :: List -> List 
addVal lst = case lst of 
		Cons rst x -> let rst' = addVal rst
				  x' = addOne x 
				in Cons rst' x' 
		Nil -> Nil

expo :: Int -> Int -> Int
expo power val = if power == 0
                then val
                else val * expo (power -1) val
 


gibbon_main = let 
		lst = mkList 1000000
		lst' = iterate (addVal lst)
	       in ()   
