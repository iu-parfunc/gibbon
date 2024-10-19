module ImportQualified where
    import qualified Addtwo
    import qualified Addone
    import AddTree

    gibbon_main = sum (Addtwo.add (Addone.add (Node (Leaf 1) (Leaf 2))))