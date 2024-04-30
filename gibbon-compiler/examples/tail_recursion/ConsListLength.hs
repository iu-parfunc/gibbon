module ConsListLength where 


data ConsIntList = Cons Int (ConsIntList) | Nil



mkConsIntList :: Int -> ConsIntList
mkConsIntList len = if len <= 0 
                    then Nil 
                    else let 
                           rst = mkConsIntList (len-1)
                          in Cons len rst


length :: ConsIntList -> Int -> Int
length lst accum = case lst of 
                    Nil -> accum 
                    Cons x rst -> length rst (accum+1)



gibbon_main = 
    let n = sizeParam
        lst = mkConsIntList n
        len = iterate (length lst 0)
      in len