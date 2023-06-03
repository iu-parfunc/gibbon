module Post where 
 
import Gibbon.Prim

data Tree = Node Tree Tree | Leaf Int 

mkTree :: Int -> Tree 
mkTree len = if len <=0 
	     then Leaf 43 
             else 
		let 
		  left  = mkTree (len-1)
                  right = mkTree (len-1)
                 in Node left right 
               
getBytes :: Tree -> Int 
getBytes tree = case tree of 
		   Node left right -> 1 + (getBytes left) + (getBytes right) 
 		   Leaf val        -> 1 + 8

gibbon_main = 
    let  
       tree = mkTree 5
       response = send_bytes tree (getBytes tree) 8080
    in response
       
