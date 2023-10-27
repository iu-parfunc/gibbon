module Addimport where
    import Addone

    main :: Tree
    main = Addone.add1 2 (Addone.add1 (Node (Leaf 1) (Leaf 2)))