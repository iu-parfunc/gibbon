module Addimport where
    import Addone
    import Addtwo


    main :: Tree
    main = Addtwo.add1 2 (Addtwo.add1 (Node (Leaf 1) (Leaf 2)))