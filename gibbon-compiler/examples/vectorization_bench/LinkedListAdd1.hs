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


pSum :: List -> List
pSum lst = case lst of
        Cons4 a b c d rst -> let h = (sum rst) in Cons4 (a+b+c+d+h) (b+c+d+h) (c+d+h) (d+h) (pSum rst)
        Cons a rst -> let h = (sum rst) in Cons (a+h) (pSum rst)
        Nil -> Nil
        
gibbon_main = 
    let 
        lst  = mkList 1000000
        --_ = printPacked lst
        --_ = printsym (quote "\n")
        lst' = iterate (add1 lst)
        --_ = printPacked lst'
        --_ = printsym (quote "\n")
     in () 
