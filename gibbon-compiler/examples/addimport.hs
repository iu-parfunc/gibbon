module AddImport where
    import qualified Addone as On
    import qualified Addtwo as Tw
    import AddTree

    gibbon_main = On.add (Tw.add (Node (Leaf 1) (Leaf 2)))