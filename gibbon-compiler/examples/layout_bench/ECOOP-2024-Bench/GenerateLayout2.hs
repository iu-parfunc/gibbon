module GenerateLayout2 where
import Basics

type Text = Vector Char

mkBlogs_layout2 :: Int -> Int -> Int -> Blog
mkBlogs_layout2 length contlen taglen =
   if length <= 0 then End
   else 
      let
          content = (Content (Plain (mkRandomInlineList contlen))) 
          tags    = (TagList (mkSomeTags taglen))    
          rst     = (mkBlogs_layout2 (length - 1) contlen taglen)
          header  = (Header (getRandomString 5))
          id      = (ID (10 - (mod length 10)))
          author  = (Author (getRandomString 5))
          date    = (Date (getRandomString 5))
         in Layout2 content tags rst header id author date   
