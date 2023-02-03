data Tree = Leaf Int | Node Int Tree Tree

--type A = Float 
--type B = Float
--type C = Float

--data Struct = Nil | Cons A B C (Struct)   

--testCFG :: Tree -> Int 
--testCFG tree = case tree of 
--	Leaf val -> val
--        Node val left right -> 
--	    let 
-- 		newVal = val 
--                leftVal = testCFG left 
--                rightVal = testCFG right
--              in newVal + leftVal + rightVal

mkTree :: Int -> Tree
mkTree i =
  if i <= 0
  then Leaf 1
  else
      let x = (mkTree (i-1))
          y = (mkTree (i-1))
       in Node i x y

testCFG1 :: Tree -> Int 
testCFG1 tree = case tree of 
	Leaf val            -> 2*val + 3*val + 5*val 
        Node val left right -> 
	    let 
	       newVal = val 
               leftVal = testCFG1 left 
               rightVal = testCFG1 right 
             in newVal + leftVal + rightVal   
              

--foo :: Bool ->  -> Int -> Int 
--foo a b c = if (a) then 
--                   let b' = b 
--                       c' = c
--                     in b' + c' 
--                   else 
--                   let c' = c 
--                    in c' 
--
--testCFG2 :: Int -> Int 
--testCFG2 val = 2*val + 3*val + 5*val 
--
--
--


--foo :: Bool -> Struct -> Int 
--foo bool struct = if (bool) then 
--			    let newStruct = Struct 1.0 2.0 3.0 Nil 
--                             in  
--                            else 
--                             struct 


gibbon_main = testCFG1 (mkTree 10)
