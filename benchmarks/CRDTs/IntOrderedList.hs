module IntOrderedList where
import Common
import Clock

data OrderedList a = OrderedList Clock (OrderedNode a)
data OrderedNode a = Bin Timestamp a Timestamp Timestamp (OrderedNode a)
                    | Tip

getStamp :: OrderedNode a -> Maybe Timestamp
getStamp x = 
    case x of
        Bin t _ _  _ _ -> Just t
        Tip -> Nothing

right :: OrderedNode a -> OrderedNode a
right x = case x of
        Bin _ _ _  r _ -> r
        Tip -> Tip

left :: OrderedNode a -> OrderedNode a
left x = case x of
        Bin _ _ _  _ l -> l
        Tip -> Tip

singleton :: Int -> Int -> OrderedNode a
singleton uid x = 
    let clk = singleton uid
    in Bin clk x clk clk

insert :: Int -> Int -> Timestamp -> Timestamp -> OrderedList
insert uid x clk l r node = 
    let t = step uid clk
    int OrderedList t (place t uid x node l r)

place :: Timestamp -> Int -> Int -> OrderedNode a -> Timestamp -> Timestamp -> OrderedNode a
place clk uid x s l r =
    case (getStamp s) of
        Nothing -> s
        Just t -> case (compare t r) of
            Eq -> s
            Lt, Gt, Cc -> 
                let t v ls rs n 
                in Bin t v ls lr case (compare t l) of 
                    Eq -> Bin (stamp uid clk) x l r (place uid clk x n l r)
                    Lt, Gt, Cc ->
                        if uid < t then place uid clk x n l r
                        else Bin (stamp uid clk) x l r (place uid clk x n l r)

value :: OrderedNode a -> List