{-# LANGUAGE TemplateHaskell #-}
module IntOrderedList where
import Common
import qualified Timekeeping as T
import qualified List as L
import qualified Map as M

data OrderedList a = OrderedList T.Clock (OrderedNode a)
data OrderedNode a = Bin T.Timestamp a T.Timestamp T.Timestamp (OrderedNode a)
                    | Tip

getHead :: OrderedList a -> OrderedNode a
getHead list =
    case list of
        OrderedList _ node -> node

getClock :: OrderedList a -> T.Clock
getClock list =
    case list of
        OrderedList c _ -> c

getStamp :: OrderedNode a -> T.Timestamp
getStamp x = 
    case x of
        Bin t _ _  _ _ -> t
        Tip -> T.Timestamp (-1) (T.init (-1))

next :: OrderedNode a -> OrderedNode a
next x = case x of
        Bin _ _ _ _ n -> n
        Tip -> Tip

singleton :: Int -> a -> OrderedList a
singleton uid x = 
    let clk = T.init uid
        t = (T.stamp uid clk)
    in OrderedList clk (Bin t x t t Tip)

insert :: Int -> a -> T.Timestamp -> T.Timestamp -> OrderedList a -> OrderedList a
insert uid x l r list = 
    let clk = T.step uid (getClock list)
    in OrderedList clk (place uid (T.stamp uid clk) x (getHead list) l r)

place' :: OrderedNode a -> OrderedNode a -> OrderedNode a
place' x acc =
    case x of
        Tip -> acc
        Bin tx vx lx rx _ ->
            case acc of
                Tip -> x
                Bin tacc vacc lacc racc nacc -> Bin tacc vacc lacc racc (Bin tx vx lx rx nacc)

place :: Int -> T.Timestamp -> a -> OrderedNode a -> T.Timestamp -> T.Timestamp -> OrderedNode a
place uid clk x s l r =
    let t = getStamp s
    in case t of
        T.Timestamp suid sclk ->
            if suid < 0 then Bin t x l r Tip
            else case (T.compare (T.clock t) (T.clock r)) of 
                Eq -> place' (Bin t x l r Tip) s
                _ -> case s of 
                        Tip -> place' (Bin t x l r Tip) s
                        Bin t v ls rs n -> 
                            case (T.compare (T.clock t) (T.clock l)) of
                                Eq -> Bin t v l r (Bin (T.stamp uid (T.clock clk)) x l r (place uid clk x n l r))
                                _ ->
                                    if uid < (T.author t) then Bin t v l r (place uid clk x n l r)
                                    else Bin t v l r (Bin (T.stamp uid (T.clock clk)) x l r (place uid clk x n l r))

cursor' :: Int -> OrderedNode a -> T.Timestamp
cursor' i x = case x of
    Tip ->
        T.stamp (-1) (T.init (-1))
    Bin t _ _ _ n -> case n of
        Tip -> t
        _ ->
            if i == 0 then t
            else (cursor' (i-1) n)

cursor :: Int -> OrderedList a -> T.Timestamp
cursor t x = (cursor' t (getHead x))

value' :: OrderedNode a -> L.Node a
value' x = case x of
    Tip -> L.Tip
    Bin _ v _ _ n -> L.Bin v (value' n)

value :: OrderedList a -> L.Node a
value x = value' (getHead x)

--gibbon_main = (
--    (insert 1 "\n" () ()
--        (insert 1 "d" () ()
--            (insert 1 "l" () ()
--                (insert 0 " " () ()
--                    (insert 0 "o" () ()
--                        (insert 1 "r" () ()
--                            (insert 0 "l" () ()
--                                (insert 1 "o" () ()
--                                    (insert 1 "w" () ()
--                                        (insert 0 "l" () ()
--                                            (insert
--                                                0 "e" (T.Timestamp 0 ()) (T.Timestamp 0 ())
--                                                (singleton 0 "h")
--                                            )
--                                        )
--                                    )
--                                )
--                            )
--                        )
--                    )
--                )
--            )
--        )
--    )
--)

gibbon_main = 
    let x = (singleton 0 0)
        x = (insert 0 1 (cursor 0 x) (cursor 1 x) x)
        x = (insert 0 1 (cursor 0 x) (cursor 1 x) x)
    in value x