
data PackedInt = P Int 

data List = Cons List PackedInt | Nil 

mkList :: Int -> List 
mkList len = if len <=0 
	     then Nil 
	     else 
		let rst = mkList (len-1)
		    val = (P len)
		  in Cons rst val


power :: PackedInt -> PackedInt 
power val = case val of 
		P x -> P (power_helper x 100)

powerList :: List -> List 
powerList lst = case lst of 
		Cons rst x -> let rst' = powerList rst
				  x' = power x 
				in Cons rst' x' 
		Nil -> Nil

power_helper :: Int -> Int -> Int
power_helper val power = if power == 0
                         then val
                         else val * power_helper (power -1) val
 


gibbon_main = let 
		lst = mkList 1000000
		lst' = iterate (powerList lst)
	       in ()   
