
data PackedInt = P Int 

data List = Cons PackedInt List | Nil 

mkList :: Int -> List 
mkList len = if len <=0 
	     then Nil 
	     else 
		let val = P len
		    rst = mkList (len-1)
		  in Cons val rst


power :: PackedInt -> PackedInt 
power val = case val of 
		P x -> P (pow_helper x 100)


pow_helper :: Int -> Int -> Int 
pow_helper val power = if power == 0
		       then val 
		       else val * pow_helper (power-1) val

powerList :: List -> List 
powerList lst = case lst of 
		   Cons x rst -> let xp = power x
				     rst' = powerList rst 
				   in Cons xp rst' 
		   Nil -> Nil 


gibbon_main = let 
		lst = mkList 1000000
		lst' = iterate (powerList lst)
	       in ()   
