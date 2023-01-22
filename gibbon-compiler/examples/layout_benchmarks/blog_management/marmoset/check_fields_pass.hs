data Foo = Layout1  Int Foo Int  | Nil

--foo :: Recursive -> Recursive 
--foo rec = case rec of 
--    Nil -> Nil
--    Layout1 rst rst1 -> Layout1 rst rst1


--mkRec :: Int -> Recursive 
--mkRec len = if len <= 0 then Nil 
--            else let rst = mkRec (len-1) 
--                   in Layout1 len rst len


id :: Foo -> Foo 
id rec = rec
