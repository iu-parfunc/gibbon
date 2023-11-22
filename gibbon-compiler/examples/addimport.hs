module AddImport where
    import Addone as On
    import Addtwo as Tw
    import AddTree

    gibbon_main = Tw.add (On.add (Node (Leaf 1) (Leaf 2)))