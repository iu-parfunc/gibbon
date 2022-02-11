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

-- readTree :: (Entity -> Key) -> String -> Tree Entity -> Tree Entity
-- readTree fk [] t = t
-- readTree fk s t =
--   let (f, s'  ) = readInt s
--       (g, s'' ) = readInt s'
--       (h, s''') = readInt s''
--       e         = (f, g, h)
--       k         = fk e
--   in  readTree fk s''' (insertT k e t)

-- readInt :: String -> (Int, String)
-- readInt s = readInt' 0 s where
--   readInt' n s@(c : cs) | isDigit c = readInt' (n * 10 + fromEnum c - fromEnum '0') cs
--   readInt' n s                      = let s' = dropWhile isSpace s in (n, s')

join :: Tree Entity -> Tree Entity -> Tree Join -> Tree Join
-- n | l | e
-- ne, le, en, el, ee
-- nn, nl, ln, ll
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

gibbon_main = print 0
main = print 0
