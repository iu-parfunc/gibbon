module LinkedListAdd1 where 

data List =   Cons Int List
            | Cons4 Int Int Int Int List  
            | Nil 


add1 :: List -> List 
add1 lst = case lst of 
        Cons4 a b c d rst -> Cons4 (a+1) (b+1) (c+1) (d+1) (add1 rst)
        Cons a rst -> Cons (a+1) (add1 rst)
        Nil -> Nil 


mkList :: Int -> List 
mkList len = if len <=0 then Nil 
             else if (mod len 4) == 0 then Cons4 len (len-1) (len-2) (len-3) (mkList (len-4))
             else Cons len (mkList (len-1) )


gibbon_main = 
    let 
        lst  = mkList 10
        _ = printPacked lst
        _ = printsym (quote "\n")
        lst' = add1 lst
        _ = printPacked lst'
        _ = printsym (quote "\n")
     in () 
