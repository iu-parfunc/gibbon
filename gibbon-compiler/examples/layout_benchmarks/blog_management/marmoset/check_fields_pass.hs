
data Field2 = Vector Char

data Blog = Layout1 (Int) (Field2) (Blog)

--id_blog :: Blog -> Blog 
--id_blog blog = case blog of 
--    Layout1 val field2 rst -> Layout1 (val+1) field2 rst

id_blog' :: Blog -> Blog 
id_blog' blog = blog