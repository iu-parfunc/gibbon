module ImportQualifiedAndSpecified where
    import qualified Addtwo (add)
    import qualified Addone (sub)
    import AddTree

    gibbon_main = sum (Addtwo.add (Addone.sub (Node (Leaf 1) (Leaf 2))))