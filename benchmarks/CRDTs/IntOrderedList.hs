module IntOrderedList where
import Common
import Clock
--import List

data OrderedList a = OrderedList Clock (OrderedNode a)
data OrderedNode a = Bin Timestamp a Timestamp Timestamp (OrderedNode a)
                    | Tip

getHead :: OrderedList a -> OrderedNode a
getHead list =
    case list of
        OrderedList _ node -> node

getClock :: OrderedList a -> Clock
getClock list =
    case list of
        OrderedList c _ -> c

getStamp :: OrderedNode a -> Common.Maybe Timestamp
getStamp x = 
    case x of
        Bin t _ _  _ _ -> Common.Just t
        Tip -> Common.Nothing

next :: OrderedNode a -> OrderedNode a
next x = case x of
        Bin _ _ _ _ n -> n
        Tip -> Tip

singleton :: Int -> a -> OrderedList a
singleton uid x = 
    let clk = Clock.init uid
        t = (stamp uid clk)
    in OrderedList clk (Bin t x t t Tip)

insert :: Int -> a -> Timestamp -> Timestamp -> OrderedList a -> OrderedList a
insert uid x l r list = 
    let clk = step uid (getClock list)
    in OrderedList clk (place uid (stamp uid clk) x (getHead list) l r)

place :: Int -> Timestamp -> a -> OrderedNode a -> Timestamp -> Timestamp -> OrderedNode a
place uid clk x s l r =
    case (getStamp s) of
        Common.Nothing -> s
        Common.Just t ->
            case (Clock.compare (clock t) (clock r)) of 
                Eq -> s
                _ -> case s of 
                        Tip -> s
                        Bin t v ls rs n -> 
                            case (Clock.compare (clock t) (clock l)) of
                                Eq -> Bin t v l r (Bin (stamp uid (Clock.clock clk)) x l r (place uid clk x n l r))
                                _ ->
                                    if uid < (author t) then Bin t v l r (place uid clk x n l r)
                                    else Bin t v l r (Bin (stamp uid (Clock.clock clk)) x l r (place uid clk x n l r))

--value :: OrderedNode a -> List a