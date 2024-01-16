
data PackedInt = P Int 

data List = Cons PackedInt List | Nil 

mkList :: Int -> List 
mkList len = if len <=0 
	     then Nil 
	     else 
		let val = P len
		    rst = mkList (len-1)
		  in Cons val rst


addOne :: PackedInt -> PackedInt 
addOne val = case val of 
		P x -> P (expo 100 x) --(x+1+4+2 -1)


expo :: Int -> Int -> Int 
expo power val = if power == 0
		then val 
		else val * expo (power-1) val

addVal :: List -> List 
addVal lst = case lst of 
		Cons x rst -> let xp = addOne x
				  rst' = addVal rst 
				in Cons xp rst' 
		Nil -> Nil 


gibbon_main = let 
		lst = mkList 1000000
		lst' = iterate (addVal lst)
	       in ()   
