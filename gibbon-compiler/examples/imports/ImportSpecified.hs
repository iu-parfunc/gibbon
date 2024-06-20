module ImportSpecified where
    import Addtwo (add)
    import Addone (sub)
    import AddTree

    gibbon_main = sum (add (sub (Node (Leaf 1) (Leaf 2))))