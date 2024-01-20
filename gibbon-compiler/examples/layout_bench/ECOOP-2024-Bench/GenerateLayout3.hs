module GenerateLayout3 where
import Basics

type Text = Vector Char

mkBlogs_layout3 :: Int -> Int -> Int -> Blog
mkBlogs_layout3 length contlen taglen =
   if length <= 0 then End
   else 
      let      
          tags = (TagList (mkSomeTags taglen))
          rst  = (mkBlogs_layout3 (length - 1) contlen taglen)
          content = (Content (Plain (mkRandomInlineList contlen)))
          header  = (Header (getRandomString 5))
          id      = (ID (10 - (mod length 10)))
          author = (Author (getRandomString 5))
          date   = (Date (getRandomString 5))
         in Layout3 tags rst content header id author date
