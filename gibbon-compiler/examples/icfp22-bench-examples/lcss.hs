module Main where


data A = Val Int | Tup A A

data MList = ENil | Cons A (MList)


sndList :: MList -> MList 
sndList tupleList = case tupleList of
                         ENil          -> ENil -- return ENil if the list is ENil
                         Cons aVal rst -> case aVal of 
                                               Val intVal -> tupleList               -- this should error out since this function cannot be called on just a list of intVals
                                               Tup a b    -> (Cons b) (sndList rst)  -- Return a list with just the second elements in it

max_A :: A -> A -> A 
max_A a b = case a of 
               Val a' -> case b of 
                            Val b' -> if (a' > b') then (Val a') else (Val b')
                            Tup l m -> b -- error out not support max on tuple case
               Tup l m -> a -- error out not supporting max on tuple case


algb2 :: A -> A -> A -> MList -> MList
algb2 x k0j1 k1j1 mList = case mList of 
                            ENil                -> ENil
                            Cons aVal ys -> case aVal of 
                                               Val intVal -> ENil      -- error out since this case should not really 
                                               Tup y k0j  -> let kjcurr = if (comp_A x y) then (add_A k0j1 (Val 1)) else (max_A k1j1 k0j) 
                                                                 in Cons (Tup y kjcurr) (algb2 x k0j kjcurr ys) 

algb1 :: MList -> MList -> MList
algb1 list1 ys' = case list1 of 
                       ENil -> sndList ys'
                       Cons aVal xs -> case aVal of             
                                             Val x -> algb1 xs (algb2 aVal (Val 0) (Val 0) ys')  --aVal is the same as the value of the integer x but wrapped around type A
                                             Tup a' b' -> ENil -- I think its safe to error out on this  

zeroTupleList :: MList -> MList
zeroTupleList list = case list of 
                         ENil -> ENil
                         Cons aVal rst -> case aVal of 
                                              Val y -> Cons (Tup aVal (Val 0)) (zeroTupleList rst) --aVal is the value of y wrapped around the type A
                                              Tup a b -> ENil -- technically i think i should error out on this


algb :: MList -> MList -> MList
algb xs ys = Cons (Val 0) (algb1 xs (zeroTupleList ys))


mzip :: MList -> MList -> MList
mzip list1 list2 = case list1 of
  ENil -> case list2 of 
             ENil          -> ENil 
             Cons aVal rst -> ENil                                            -- should this be an error situation coz lists should technically be of the same length ? 
  Cons aVal rst -> case list2 of 
                       ENil           -> ENil                                 -- should this be an error situation coz lists should technically be of the same length ? 
                       Cons bVal rst' -> Cons (Tup aVal bVal) (mzip rst rst')   -- zip the rest of the lists together


m_id :: MList -> MList 
m_id a = a


mreverse :: MList -> MList -> MList
mreverse xs acc = case xs of
  ENil       -> acc
  Cons z zs  -> mreverse zs (Cons z acc)

comp_MList :: MList -> MList -> Bool
comp_MList list1 list2 = case list1 of 
                                 ENil -> case list2 of 
                                          ENil -> True
                                          Cons x rst -> False
                                 Cons x' rst' -> case list2 of
                                                    ENil         -> False
                                                    Cons y rst'' -> if (comp_A x' y) then True && (comp_MList rst' rst'') else False
--Equality operator for type A
comp_A :: A -> A -> Bool
comp_A a b = case a of 
             Val x -> case b of 
                        Val y -> if x == y then True else False
                        Tup l m -> False 
             Tup a' b' -> case b of 
                             Val y' -> False
                             Tup l' m' -> (comp_A a' l') && (comp_A b' m')

--Greater x > y for type A
gr_A :: A -> A -> Bool 
gr_A a b = case a of 
             Val x -> case b of 
                         Val y   -> if (x > y) then True else False
                         Tup a b -> False -- This should error out find a way to error out in gibbon
             Tup x y -> case b of 
                          Val y'  -> False -- This should error out find a way to error out in gibbon
                          Tup a b -> if (gr_A x a) && (gr_A y b) then True else False  



elem :: A -> MList -> Bool
elem a list = case list of 
                   ENil       -> False
                   Cons x rst -> if (comp_A x a) then True else (False || elem a rst) 


take :: A -> MList -> MList
take num list = case list of 
                   ENil       -> ENil
                   Cons x rst -> case num of 
                                    Val intVal ->  if (intVal > 0) then Cons x (take (Val (intVal - 1)) rst) else Cons x ENil
                                    Tup a b -> list -- This should error out technically, what does it mean to take according to a Tuple ?   

drop :: A -> MList -> MList
drop num list = case list of 
                    ENil -> ENil 
                    Cons x rst -> case num of 
                                     Val intVal -> if (intVal > 0) then drop (Val (intVal - 1)) rst else rst
                                     Tup a b -> list --This should error out technically

add_A :: A -> A -> A
add_A v1 v2 = case v1 of 
                 Val x -> case v2 of 
                            Val y -> Val (x + y)
                            Tup a b -> v1          -- This should technically call error saying that can't add Tup and Val kinds but for now can return just v1
                 Tup x y -> case v2 of 
                              Tup a b -> Tup (add_A x a) (add_A y b)
                              Val a'  -> v1        -- This should also technically error out saying that can't add val to Tup but for now this works 

sub_A :: A -> A -> A
sub_A v1 v2 = case v1 of 
                 Val x -> case v2 of 
                            Val y -> Val (x - y)
                            Tup a b -> v1          -- This should technically call error saying that can't add Tup and Val kinds but for now can return just v1
                 Tup x y -> case v2 of 
                              Tup a b -> Tup (sub_A x a) (sub_A y b)
                              Val a'  -> v1        -- This should also technically error out saying that can't add val to Tup but for now this works 

findk :: A -> A -> A -> MList -> A
findk k km m list = case list of 
                       ENil -> km                 
                       Cons aVal rst -> case aVal of 
                                          Val dontCare -> k        --Find out how to error out on this
                                          Tup x y      -> case k of        {-if condition checks to see x + y >= m-} 
                                                            Val intVal -> if ((gr_A (add_A x y) m) || (comp_A (add_A x y) m)) then findk (Val (intVal + 1)) k (add_A x y) rst else findk (Val (intVal + 1)) km m rst
                                                            Tup a' b' -> k --find out how to error out on this
div_A :: A -> A -> A 
div_A a b = case a of 
                Val intVal -> case b of 
                               Val intVal'   -> Val (div intVal intVal')
                               Tup a' b'     -> a -- this should be error, so handle this later 
                Tup a'' b'' -> case b of 
                                Val intVal'  -> a -- this should be error, so handle this later
                                Tup a' b'    -> Tup (div_A a'' a') (div_A b'' b') 



algc :: A -> A -> MList -> MList -> MList -> MList
algc m n xs emptyCheck ys = case emptyCheck of 
                          ENil       -> m_id ys
                          Cons x rst -> case xs of 
                                          ENil -> ENil
                                          Cons x' rst' -> if (comp_MList rst' ENil) then (if  (elem x' emptyCheck) then (Cons x' ys) else m_id ys)
                                                                                    else let m2  = div_A  m  (Val 2)
                                                                                             xs1 = take m2 xs
                                                                                             xs2 = drop m2 xs
                                                                                             l1  = algb xs1 emptyCheck
                                                                                             l2  = mreverse (algb (mreverse xs2 ENil) (mreverse emptyCheck ENil)) ENil
                                                                                             k   = findk (Val 0) (Val 0) (Val (-1)) (mzip l1 l2)
                                                                                             in algc m2 k xs1 (take k emptyCheck) ( (algc (sub_A m m2) (sub_A n k) xs2 (drop k emptyCheck) ys) )
length :: MList -> A 
length list = case list of 
                   ENil          -> (Val 0) 
                   Cons aVal rst -> add_A (Val 1) (length rst)  



lcss :: MList -> MList -> MList
lcss xs ys = algc (length xs) (length ys) xs ys ENil

-- main = do
--  [a,b,c,d,e,f] <- getArgs
--  let a', b', c', d', e', f' :: Int
--      a' = read a; b' = read b; c' = read c;
--      d' = read d; e' = read e; f' = read f
--  print (lcss [a',b'..c'] [d',e'..f'])


-- Need Complete the main function for this benchmark, original main function above, write in gibbon and read the argument from file

-- bench_main :: ()
-- bench_main =
--   in  ()

-- gibbon_main = bench_main


