module Main where

import Gibbon.Vector

data A = Val Int | Tup A A

data MList = ENil | Cons A (MList)

sndList :: MList -> MList 
sndList tupleList = case tupleList of
                         ENil          -> ENil -- return ENil if the list is ENil
                         Cons aVal rst -> case aVal of 
                                               Val intVal -> tupleList               -- this should error out since this function cannot be called on just a list of intVals
                                               Tup a b    -> case b of 
                                                                 Val i   -> Cons (Val i) (sndList rst)
                                                                 Tup a' b' -> Cons (Tup a' b') (sndList rst)  
max_A :: A -> A -> A 
max_A a b = case a of 
               Val a' -> case b of 
                            Val b' -> if (a' >= b') then (Val a') else (Val b')
                            Tup l m -> b -- error out not support max on tuple case
               Tup l m -> a -- error out not supporting max on tuple case



ifalgb :: Bool -> A -> A -> A 
ifalgb tell add max = if (tell) then add else max


-- Function to make a tuple out of two A values
makeTuple :: A -> A -> A 
makeTuple val1 val2 = case val1 of 
                           Val a -> case val2 of 
                                         Val b     -> (Tup (Val a) (Val b))
                                         Tup a' b' -> val2 

                           Tup a' b' -> case val2 of 
                                             Val b''     -> val1
                                             Tup a'' b'' -> val2 

algb2 :: A -> A -> A -> MList -> MList
algb2 x k0j1 k1j1 mList = case mList of 
                            ENil      -> ENil
                            Cons aVal ys -> case aVal of 
                                               Val intVal -> ENil                                     -- error out since this case should not happen
                                               Tup y k0j  -> let kjcurr, addVal, maxVal, newTup :: A
                                                                 tell   :: Bool 
                                                                 tell   = comp_A x y
                                                                 addVal = add_A k0j1 (Val 1)
                                                                 maxVal = max_A k1j1 k0j 
                                                                 --_ = printA maxVal
                                                                 --_   = printsym (quote "\n")
                                                                 --_ = printA addVal
                                                                 --_   = printsym (quote "\n")
                                                                 --_ = printbool tell
                                                                 --_   = printsym (quote "\n")
                                                                 kjcurr = ifalgb tell addVal maxVal
                                                                 --_ = printA kjcurr
                                                                 --_ = printsym (quote "\n")
                                                                 newTup = makeTuple y kjcurr
                                                                 --_ = printsym (quote "\nPrint newTuple\n")
                                                                 --_ = printA newTup
                                                                 --_ = printsym (quote "\nys\n")
                                                                 --_ = printList ys
                                                                 --_ = printsym (quote "\nx\n")
                                                                 --_ = printA x
                                                                 --_ = printsym (quote "\n")
                                                                 --_ = printsym (quote "\nk0j\n")
                                                                 --_ = printA k0j
                                                                 --_ = printsym (quote "\n")
                                                                 --_ = printsym (quote "kjcur\n")
                                                                 --_ = printA kjcurr
                                                                 --_ = printsym (quote "\n")
                                                                 --rstPart = (algb2 x k0j kjcurr ys)
                                                                 --newList = Cons newTup (rstPart)
                                                                 --_ = printsym (quote "Printing the new List here!\n")
                                                                 --_       = printList newList
                                                                 --_ = printsym (quote "\n")
                                                                 in Cons newTup (algb2 x k0j kjcurr ys)  --  newList


algb1 :: MList -> MList -> MList
algb1 list1 ys' = case list1 of 
                       ENil -> sndList ys'
                       Cons aVal xs -> case aVal of             
                                             Val x -> let recurse   = algb1 xs (algb2 (Val x) (Val 0) (Val 0) ys')
                                                          --_ = printsym (quote "Print the algb2Part in algb1 here!\n")
                                                          --_         = printList algb2Part
                                                          --_ = printsym (quote "\n")
                                                          in recurse                             
                                                              
                                             Tup a' b' -> ENil -- I think its safe to error out on this  

zeroTupleList :: MList -> MList
zeroTupleList list = case list of 
                         ENil -> ENil
                         Cons aVal rst -> case aVal of 
                                              Val y -> Cons (makeTuple aVal (Val 0)) (zeroTupleList rst) --aVal is the value of y wrapped around the type A
                                              Tup a b -> ENil                                            --technically i think i should error out on this


algb :: MList -> MList -> MList
algb xs ys = let leftPart =  Cons (Val 0) (algb1 xs (zeroTupleList ys))
                 --result   =  leftPart
                 --_ = printsym (quote "Print the result in algb here!\n")
                 --_ = printList leftPart
                 --_ = printsym (quote "\n")      
                 in leftPart 


mzip :: MList -> MList -> MList
mzip list1 list2 = case list1 of
  ENil -> case list2 of 
             ENil          -> ENil 
             Cons aVal rst -> ENil                                            -- should this be an error situation coz lists should technically be of the same length ? 
  Cons aVal rst -> case list2 of 
                       ENil           -> ENil                                 -- should this be an error situation coz lists should technically be of the same length ? 
                       Cons bVal rst' -> Cons (makeTuple aVal bVal)  (mzip rst rst')   -- zip the rest of the lists together


-- This function looks correct, return exact copy of the list
m_id :: MList -> MList 
m_id a = a


-- tested mreverse works, now
mreverse :: MList -> MList -> MList
mreverse xs acc = case xs of
  ENil       -> acc
  Cons z zs  -> case z of 
                   Val int ->  mreverse zs (Cons (Val int) acc)
                   Tup a b ->  mreverse zs (Cons (makeTuple a b) acc)

comp_MList :: MList -> MList -> Bool
comp_MList list1 list2 = case list1 of 
                                 ENil -> case list2 of 
                                          ENil -> True
                                          Cons x rst -> False
                                 Cons x' rst' -> case list2 of
                                                    ENil         -> False
                                                    Cons y rst'' -> if (comp_A x' y) then (True && (comp_MList rst' rst'')) else False
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


-- Tested elem it works correctly
elem :: A -> MList -> Bool
elem a list = case list of 
                   ENil       -> False
                   Cons x rst -> if (comp_A x a) then True else (False || elem a rst) 



-- This works now, tested and degubbed it
take :: A -> MList -> MList
take num list = case list of 
                   ENil       -> ENil
                   Cons x rst -> case x of 
                                     Val intVal -> case num of 
                                                       Val x'  -> if (x' > 0) then Cons (Val intVal) (take (Val (x' - 1)) rst) else ENil
                                                       Tup x' y' -> list --This should error out technically, coz how to take from a Tuple

                                     Tup a b    -> case num of 
                                                       Val x'  -> if (x' > 0) then Cons (Tup a b) (take (Val (x' - 1)) rst) else ENil
                                                       Tup x' y' -> list --This should error out technically, coz how to take from a Tuple 

-- This works 
drop :: A -> MList -> MList
drop num list = case list of 
                    ENil -> ENil 
                    Cons x rst -> case num of 
                                     Val intVal -> if (intVal <= 0) then Cons x rst else drop (Val (intVal - 1)) rst
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
                                                            Val intVal -> if ((gr_A (add_A x y) m) || (comp_A (add_A x y) m)) then (findk (Val (intVal + 1)) k (add_A x y) rst) else (findk (Val (intVal + 1)) km m rst)


                                                            Tup a' b' -> k --find out how to error out on this

-- The division function works                                                        
div_A :: A -> A -> A 
div_A a b = case a of 
                Val intVal -> case b of 
                               Val intVal'   -> Val (div intVal intVal')
                               Tup a' b'     -> a -- this should be error, so handle this later 
                Tup a'' b'' -> case b of 
                                Val intVal'  -> a -- this should be error, so handle this later
                                Tup a' b'    -> Tup (div_A a'' a') (div_A b'' b') 



ifalgc :: Bool -> MList -> MList -> MList
ifalgc check list1 list2 = if (check) then list1 else list2


appendCons :: A -> MList -> MList
appendCons val tail = case val of 
                         Val int -> Cons (Val int) tail
                         Tup a b -> Cons (makeTuple a b) tail

algc :: A -> A -> MList -> MList -> MList -> MList
algc m n xs ys ys' = case ys of 
                          ENil       -> m_id ys'
                          Cons x rst -> case xs of 
                                          ENil -> ENil
                                          Cons x' rst' -> if (comp_MList rst' ENil) then let isElem :: Bool
                                                                                             headList, idList :: MList
                                                                                             isElem = elem x' ys 
                                                                                             headList = appendCons x' ys' 
                                                                                             idList   = m_id ys'
                                                                                             --_  = printsym  (quote "\n")
                                                                                             --_  = printbool isElem 
                                                                                             --_  = printsym  (quote "\n")
                                                                                             --_  = printsym  (quote "\n")
                                                                                             --_  = printsym  (quote "ys'")
                                                                                             --_  = printList ys' 
                                                                                             --_  = printsym  (quote " || Cons x ys'")
                                                                                             --_  = printList headList
                                                                                             --_  = printsym  (quote " || x ")
                                                                                             --_  = printA x'
                                                                                             --_  = printsym  (quote "\n")
                                                                                             in (ifalgc isElem headList idList)    
                                                                                    else let m2  = div_A  m  (Val 2)                                    
                                                                                             xs1 = take m2 xs
                                                                                             xs2 = drop m2 xs
                                                                                            --  _   = printsym (quote "\n")
                                                                                            --  _   = printsym (quote "Print m2 value in algc\n")
                                                                                            --  _   = printA m2
                                                                                            --  _   = printsym (quote "\n")
                                                                                            --  _   = printsym (quote "Print the List xs1 in algc\n")
                                                                                            --  _   = printList xs1
                                                                                            --  _   = printsym (quote "\n")
                                                                                            --  _   = printsym (quote "Print the List xs2 in algc\n")
                                                                                            --  _   = printList xs2
                                                                                            --  _   = printsym (quote "\n")
                                                                                            --  _   = printsym (quote "Print the List ys in algc\n")
                                                                                            --  _   = printList ys
                                                                                            --  _   = printsym (quote "\n")                                                                                              
                                                                                             l1  = algb xs1 ys
                                                                                             l2  = mreverse (algb (mreverse xs2 ENil) (mreverse ys ENil)) ENil
                                                                                             k   = findk (Val 0) (Val 0) (Val (-1)) (mzip l1 l2)
                                                                                            --  _ = printsym (quote "List l1!\n")
                                                                                            --  _ = printList l1
                                                                                            --  _ = printsym (quote "\n")
                                                                                            --  _ = printsym (quote "\n")
                                                                                            --  _ = printsym (quote "List l2!\n")
                                                                                            --  _ = printList l2
                                                                                            --  _ = printsym (quote "\n")
                                                                                            --  _ = printsym (quote "\n")
                                                                                            --  _ = printsym (quote "K in algc!\n")
                                                                                            --  _ = printA k
                                                                                            --  _ = printsym (quote "\n")
                                                                                            --  _ = printsym (quote "\n")
                                                                                             algc'  = algc (sub_A m m2) (sub_A n k) xs2 (drop k ys) ys'
                                                                                             algc'' =  algc m2 k xs1 (take k ys) algc'
                                                                                            --  _ = printsym (quote "List algc'!\n")
                                                                                            --  _ = printList algc'
                                                                                            --  _ = printsym (quote "\n")
                                                                                            --  _ = printsym (quote "List algc''!\n")
                                                                                            --  _ = printList algc''
                                                                                            --  _ = printsym (quote "\n")
                                                                                            -- _  = printsym (quote "\nPrint the variables in algc function\n")
                                                                                            -- _  = printsym (quote "Print m2 value:\n")
                                                                                            -- _   = printA m2
                                                                                            -- _   = printsym (quote "\nPrint the List xs1\n")
                                                                                            -- _   = printList xs1
                                                                                            -- _   = printsym (quote "\nPrint the List xs2\n")
                                                                                            -- _   = printList xs2
                                                                                            -- _   = printsym (quote "\nPrint the List l1\n")
                                                                                            -- _   = printList l1
                                                                                            -- _   = printsym (quote "\nPrint the List l2\n")
                                                                                            -- _   = printList l2
                                                                                            -- _   = printsym (quote "\nPrint the value k\n")
                                                                                            -- _   = printA k
                                                                                            -- _   = printsym (quote "\nPrint the List algc'\n")
                                                                                            -- _   = printList algc'
                                                                                            -- _   = printsym (quote "\nPrint the List algc''\n")
                                                                                            -- _    = printList algc''
                                                                                             in algc'' 


length' :: MList -> A 
length' list = case list of 
                   ENil          -> (Val 0) 
                   Cons aVal rst -> add_A (Val 1) (length' rst)   



lcss :: MList -> MList -> MList
lcss xs ys = algc (length' xs) (length' ys) xs ys ENil              


--Make a MList from start index to an end index
makeMList :: Int -> Int -> Int -> MList
makeMList start end skipFactor = if (start <= end) then Cons (Val start) (makeMList (start + skipFactor) end skipFactor) else ENil

--A function for printing A                                  
printA :: A -> ()
printA val = case val of 
                  Val x -> 
                    let _ = printsym (quote "(Val ")
                        _ = printint x
                        _ = printsym (quote ")" )
                    in ()
                  Tup a b -> 
                    let _ = printsym (quote "(Tup " )
                        _ = printA a
                        _ = printsym (quote " ")
                        _ = printA b
                        _ = printsym (quote ")" )
                    in ()

--A function for printing the MList
printList :: MList -> ()
printList list = case list of 
                      ENil -> 
                        let _ = printsym (quote " ENil " )
                        in ()
                      Cons x rst -> 
                        let _ = printsym (quote " Cons " )
                            _ = printA x                             
                            _ = printsym (quote " ->" )
                            _ = printList rst
                        in ()

-- Main program to run longest common subsequence
bench_main :: ()
bench_main = 
  let f :: Vector Int
      a', b', c', d', e', f'  :: Int
      l1, l2, l3, l4          :: MList
      len, len1, chelem, n1   :: A
      t1, t2                  :: Bool
      f  = readArrayFile Nothing
      --_  = printVec (\i -> printint i) f
      a' = nth f 0
      b' = nth f 1
      c' = nth f 2
      d' = nth f 3
      e' = nth f 4
      f' = nth f 5
      --_  = printsym (quote "\n")
      --_  = printint a'
      --_  = printsym (quote "\n")
      --_  = printint b'
      --_  = printsym (quote "\n")
      --_  = printint c'
      --_  = printsym (quote "\n")
      --_  = printint d'
      --_  = printsym (quote "\n")
      --_  = printint e'
      --_  = printsym (quote "\n")
      --_  = printint f'
      --_  = printsym (quote "\n")
      l1 = makeMList a' c' (b' - a')
      l2 = makeMList d' f' (e' - d')
      -- lenth' function looks correct
     -- len  = length' l1
     -- len1 = length' l2 
      -- m_id function looks correct
     -- l3  = m_id l1                 
     -- _   = printsym (quote "Print the list lengths!!\n")
     -- _   = printA len
     -- _   = printsym (quote "\n")
     -- _   = printA len1
     -- _   = printsym (quote "\n")
     -- _   = printsym (quote "Print List l1!\n")
      --_   = printsym (quote "\n")
     -- _   = printsym (quote "\n")
     -- _   = printsym (quote "\n")
     -- _   = printList l1
     -- _   = printsym (quote "\n")
     -- _   = printsym (quote "\n")
     -- _   = printsym (quote "Print List l2!\n")
     -- _   = printList l2
     -- _   = printsym (quote "\n")
      --t1  = comp_A len len1 
      --_   = printbool t1
      --_   = printsym (quote "\n")
      --t2  = comp_MList l1 l2
      --_   = printbool t2
      --_   = printsym (quote "\n")
      --chelem = (Val 2)
      --t2     = elem chelem l1
      --_   = printbool t2
      --_   = printsym (quote "Check Arithmetic!\n") 
      --t1 = (gr_A (Val 0) (Val 0)) || (comp_A (Val 0) (Val 0))
      --t1 = comp_MList ENil ENil 
      --_   = printbool t1
      --chelem = max_A (Val 100) (Val 2)
     -- _   = printsym (quote "\nPrint the max function out\n")
     -- _   = printA chelem 
     -- _   = printsym (quote "\n")
      --n1  = (Val 1)
      --_   = printA n1 
      --_   = printsym (quote "Result of take\n")
      --l4  = drop n1 l1
      --_   = printList l4
      --_   = printsym (quote "Check reverse List!\n")
      --l4  = mreverse l1 ENil
      --_   = printList l4
      --_   = printsym (quote "\n")
      --_   = printsym (quote "Check mzip List!\n")
      --l4  = mzip l2 l1
      --_   = printList l4
      --_   = printsym (quote "\n")
      --_   = printsym (quote "Print output of zero tuple list\n")
      --l4 = zeroTupleList l1
      --l4  = sndList l4
      --_   = printList l4
      --_   = printsym (quote "\n")
      --chelem = add_A len len1
      --_   = printA chelem
      --_   = printsym (quote "\n")
      l3  = lcss l1 l2
      _   = printsym (quote "\n")
      _   = printsym (quote "The final output produced is:\n")
      _   = printList l3
      _   = printsym (quote "\n")

  in ()

gibbon_main = bench_main


-- Instructions to run

-- Command use to run 
-- gibbon --packed --to-exe lcss.hs; ./lcss.exe --array-input-length 6 --array-input lcss.txt 
-- Modify lcss.txt file for 3 cases like so: 

-- 1.) Fast   Case  -> 1 
--                     2
--                     2000
--                     1000
--                     1001
--                     2000

-- 2.) Slow   Case  -> 1
--                     2
--                     2000
--                     1000
--                     1001
--                     4000 

-- 3.) Normal Case  -> 1
--                     2 
--                     4000
--                     1000
--                     1001
--                     4000
