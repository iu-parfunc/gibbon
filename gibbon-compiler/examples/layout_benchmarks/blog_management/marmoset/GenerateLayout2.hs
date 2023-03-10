module GenerateLayout2 where
import Basics

type Text = Vector Char

mkBlogs_layout2 :: PList Text -> PList Text -> Int -> Blog
mkBlogs_layout2 contentfiles tagfiles length =
   if length < 0 then End
   else 
      let select  = (mod length 10)
          def     = "default file"
          fc      = fromMaybe def (nth_plist contentfiles Nothing select)
          vvv :: Vector (Vector Char) 
          vvv = valloc 0
          --content_words = fileToContent' fc  (singleton (nth fc  0)) vvv 1 (vlength fc)
          ft      = fromMaybe def (nth_plist tagfiles Nothing select)               
         in Layout2 (Content (Plain (mkRandomInlineList 500))) (TagList (mkSomeTags 10)) (mkBlogs_layout2 contentfiles tagfiles (length - 1)) (Header (getRandomString (mod rand 9))) (ID (10 - (mod length 10))) (Author (getRandomString (mod rand 9))) (Date (getRandomString (mod rand 9))) 
