module GenerateLayout8 where
import Basics

type Text = Vector Char


mkBlogs_layout8 :: Int -> Int -> Int -> Blog
mkBlogs_layout8 length contlen taglen = 
   if length <= 0 then End 
   else 
      let
          content = (Content (Plain (mkRandomInlineList contlen)))
          rst     = (mkBlogs_layout8 (length - 1) contlen taglen)
          id      = (ID (10 - (mod length 10)))
          author  = (Author (getRandomString 5))
          date    = (Date (getRandomString 5))
          header  = (Header (getRandomString 5))
          tags    = (TagList (mkSomeTags taglen))       
         in Layout8 content rst id author date header tags    
