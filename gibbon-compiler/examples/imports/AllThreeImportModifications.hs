module AllThreeImportModifications where
    import qualified Addtwo as T (add)
    import qualified Addone as O (add)
    import AddTree as Tree (Node, Lead)

    gibbon_main = sum (T.add (O.add (Tree.Node (Tree.Leaf 1) (Tree.Leaf 2))))