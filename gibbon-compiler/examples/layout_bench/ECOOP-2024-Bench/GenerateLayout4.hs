module GenerateLayout4 where
import Basics

type Text = Vector Char

mkBlogs_layout4 :: Int -> Int -> Int -> Blog
mkBlogs_layout4 length contlen taglen =
   if length <= 0 then End
   else 
      let  
          tags = (TagList (mkSomeTags taglen))
          content = (Content (Plain (mkRandomInlineList contlen)))
          rst = (mkBlogs_layout4 (length - 1) contlen taglen)
          header = (Header (getRandomString 5))
          id     = (ID (10 - (mod length 10)))
          author = (Author (getRandomString 5))
          date   =  (Date (getRandomString 5))
         in Layout4 tags content rst header id author date
