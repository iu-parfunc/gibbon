
type Field2 = Vector Char

--data Blog = Layout1 (Int) (Field2) (Blog)

data Blog = Layout1 (Float) (Blog) (Float) (Int)

id_blog :: Blog -> Blog 
id_blog blog = case blog of 
    --Layout1 val field2 rst -> Layout1 (val+1) field2 rst
    Layout1 fl1 rst fl2 int   -> let newRst = id_blog rst 
                                   in Layout1 (fl1) (copyPacked newRst) (fl2) (int+1)
    
    
--id_blog' :: Blog -> Blog 
--id_blog' blog = blog
