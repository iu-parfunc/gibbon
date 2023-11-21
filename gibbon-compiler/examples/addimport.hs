module AddImport where
    import Addone
    import Addtwo
    import AddTree

    gibbon_main = Addone.add (Addtwo.add (Node (Leaf 1) (Leaf 2)))