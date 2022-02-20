-- source: https://github.com/ghc/nofib/blob/f34b90b5a6ce46284693119a06d1133908b11856/gc/treejoin/Main.hs
type Key = Int
data Tree e =
    Node Key (Tree e) (Tree e)
    | Leaf Key e
    | Empty

type Entity = (Int, Int, Int)
type Join = (Int, Int, Int, Int, Int)

insertT :: Key -> entity -> Tree entity -> Tree entity
insertT k e t = case t of
  Node k' l r -> if k <= k' then Node k' (insertT k e l) r else Node k' l (insertT k e r)
  Leaf k' v   -> if k < k' then Node k (Leaf k e) (Leaf k' v) else if k > k' then Node k' (Leaf k' v) (Leaf k' e) else error "Key Value already exists"
  Empty       -> Leaf k e


lookupT :: Key -> Tree entity -> Maybe entity
lookupT k n = case n of
  Node k' l r -> if k <= k' then lookupT k l else lookupT k r

  Leaf k' e   -> if k == k' then Just e else Nothing

  Empty       -> Nothing

mkTree :: (Entity -> Key) -> Vector (Int, Int, Int) -> Tree Entity -> Tree Entity
mkTree fk [] t = t
mkTree fk (pt@(f, g, h):pts) t =
  let k         = fk pt
  in  readTree fk pts (insertT k e t)

join :: Tree Entity -> Tree Entity -> Tree Join -> Tree Join
join t1 t2 j = case t1 of
  Empty            -> j
  Leaf k (a, b, c) -> case t2 of
    Empty -> j
    _     -> case lookupT c t2 of
      Nothing        -> j
      Just (d, e, f) -> insertT c (a, b, c, d, e) j
  Node k l r -> case t2 of
    Empty -> j
    _     -> join l t2 (join r t2 j)
