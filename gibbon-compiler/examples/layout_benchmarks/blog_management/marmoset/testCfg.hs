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


type Id      = Int  
type Date    = Int 
type Author  = Int 
type Header  = Int 
type Tags    = Int 
type Content = Int
type String = Int


data BlogList = Blog Id Date Author Header Tags Content BlogList | End 


search :: Tags -> Bool 
search tags = True

editContent :: Content -> Content 
editContent cont = cont + 1

emphasizeKeyword :: String -> BlogList -> BlogList
emphasizeKeyword keyword bloglist =
	case bloglist of
		Blog _id date author header tags content rst ->
			let present = search tags 
			  in if ( present ) then
					    let newcontent = editContent content 
						newNext = emphasizeKeyword keyword rst
					     in Blog _id date author header tags newcontent newNext
					    else
					     let newNext = emphasizeKeyword keyword rst
						in Blog _id date author header tags content newNext
                End -> End







gibbon_main = let 
                 val   = testCFG1 (mkTree 10)
                 blog' = emphasizeKeyword 1 End 
                 _     = printPacked blog'
               in ()






