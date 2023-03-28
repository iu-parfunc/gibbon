data Foo = F Int Int Int 
{-# ANN F (foo, (0 ~> 2, 1 ~> 1, 2 ~> 0)) #-}


foo :: Foo -> Foo 
foo f = f

bar :: Foo -> Foo 
bar f = f