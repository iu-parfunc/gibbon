module ImportAliased where
    import qualified Addtwo as T
    import qualified Addone as O
    import AddTree as Tree

    gibbon_main = sum (T.add (O.add (Tree.Node (Tree.Leaf 1) (Tree.Leaf 2))))