module AddImport where
    import Addtwo (sub)
    import Addone
    import AddTree

    gibbon_main = add (sub (Node (Leaf 1) (Leaf 2)))