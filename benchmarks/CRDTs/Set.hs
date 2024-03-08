{-# LANGUAGE BlockArguments #-}

module Set where

--            type size value left right
type Size = Int
data IntSet = PureSet Int Int IntSet IntSet
            | EmptySet

-- Construction --------------------------------------------

-- Create an empty set
empty :: IntSet
empty = EmptySet

-- Create a set with an initialization element
singleton :: Int -> IntSet
singleton x = PureSet 1 x EmptySet EmptySet

-- Insertion, Deletion --------------------------------------
insert :: Int -> IntSet -> IntSet
insert x s =
    case s of
        EmptySet -> singleton x
        PureSet sz v l r ->
            if x == v then s
            else if x <= v then 
                let nl = insert x l
                in balanceL v nl r
            else 
                let nr = insert x r
                in balanceR v l nr

balanceL :: Int -> IntSet -> IntSet -> IntSet
balanceL x l r = case r of
  EmptySet -> case l of
           EmptySet -> PureSet 1 x EmptySet EmptySet
           PureSet ls lx ll lr ->
                case ll of
                    PureSet lls llx lll llr -> case lr of
                        PureSet lrs lrx lrl lrr ->
                            if (lrs < 2*lls) then PureSet (1+ls) lx ll (PureSet (1+lrs) x lr EmptySet)
                            else PureSet (1+ls) lrx (PureSet (1+lls+size lrl) lx ll lrl) (PureSet (1+size lrr) x lrr EmptySet)
                        EmptySet -> PureSet 3 lx ll (PureSet 1 x EmptySet EmptySet)
                    EmptySet -> case lr of 
                        PureSet lrs lrx lrl lrr -> PureSet 3 lrx (PureSet 1 lx EmptySet EmptySet) (PureSet 1 x EmptySet EmptySet)
                        EmptySet -> PureSet 2 x l EmptySet

  PureSet rs _ _ _ -> case l of
           EmptySet -> PureSet (1+rs) x EmptySet r

           PureSet ls lx ll lr ->
                if (ls > 3*rs) then
                    if ((size lr) < 2*(size ll)) then PureSet (1+ls+rs) lx ll (PureSet (1+rs+(size lr)) x lr r)
                    else PureSet (1+ls+rs) (val lr) (PureSet (1+(size ll)+(size (left lr))) lx ll (left lr)) (PureSet (1+rs+(size (right lr))) x (right lr) r)
                else PureSet (1+ls+rs) x l r

balanceR :: Int -> IntSet -> IntSet -> IntSet
balanceR x l r = case l of
  EmptySet -> case r of
            EmptySet -> PureSet 1 x EmptySet EmptySet
            PureSet rs rx rl rr ->
                case rr of
                    PureSet rrs rrx rrl rrr -> case rl of 
                        PureSet rls rlx rll rlr ->
                            if ((size rl) < 2*(size rr)) then PureSet (1+rs) rx (PureSet (1+(size rl)) x EmptySet rl) rr
                            else PureSet (1+rs) (val rl) (PureSet (1+(size (left rl))) x EmptySet (left rl)) (PureSet (1+(size rr)+(size (right rl))) rx (right rl) rr)
                        EmptySet -> PureSet 3 rx (PureSet 1 x EmptySet EmptySet) rr
                    EmptySet -> case rl of
                        PureSet rls rlx rll rlr -> PureSet 3 (val rl) (PureSet 1 x EmptySet EmptySet) (PureSet 1 rx EmptySet EmptySet)
                        EmptySet -> PureSet 2 x EmptySet r

  PureSet ls _ _ _ -> case r of
           EmptySet -> PureSet (1+ls) x l EmptySet

           PureSet rs rx rl rr ->
                if (rs > 3*ls) then
                    if ((size rl) < 2*(size rr)) then PureSet (1+ls+rs) rx (PureSet (1+ls+(size rl)) x l rl) rr
                    else PureSet (1+ls+rs) (val rl) (PureSet (1+ls+(size (left rl))) x l (left rl)) (PureSet (1+(size rr)+(size (right rl))) rx (right rl) rr)
                else PureSet (1+ls+rs) x l r

-- Contents ------------------------------------------------

-- Check if the set is empty
null :: IntSet -> Bool
null s = 
    case s of 
        EmptySet -> True
        PureSet _ _ _ _ -> False
-- Check if a set has an element
member :: Int -> IntSet -> Bool
member x s  =
    case s of
        EmptySet -> False
        PureSet _ v l r -> 
            if x == v then True
            else if x <= v then member x l
            else member x r

depth :: Int -> Int -> IntSet -> Int
depth x c s =
    case s of
        EmptySet -> -1
        PureSet _ v l r -> 
            if x == v then c + 1
            else if x <= v then depth x (c+1) l
            else depth x (c+1) r

size :: IntSet -> Int
size s = case s of 
    EmptySet -> 0
    PureSet sz _ _ _ -> sz

val :: IntSet -> Int
val x = case x of
    EmptySet -> 0
    PureSet _ v _ _ -> v

left :: IntSet -> IntSet
left x = case x of
    EmptySet -> EmptySet
    PureSet _ _ l _ -> l

right :: IntSet -> IntSet
right x = case x of
    EmptySet -> EmptySet
    PureSet _ _ _ r -> r

insert_num :: Int -> IntSet -> IntSet
insert_num x s =
    if x == 0   then insert x s
    else insert x (insert_num (x-1) s)

sum :: IntSet -> Int
sum s = case s of
    EmptySet -> 0
    PureSet _ v l r -> v + (Set.sum l) + (Set.sum r)

--gibbon_main = sum (insert_num 100 (insert_num 100 empty))
gibbon_main = Set.sum (insert 0 empty)