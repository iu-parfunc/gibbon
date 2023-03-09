module GenerateLayout4 where
import Basics

type Text = Vector Char

mkBlogs_layout4 :: PList Text -> PList Text -> Int -> Blog
mkBlogs_layout4 contentfiles tagfiles length =
   if length < 0 then End
   else 
      let select  = (mod length 10)
          def     = "default file"
          fc      = fromMaybe def (nth_plist contentfiles Nothing select)
          vvv :: Vector (Vector Char) 
          vvv = valloc 0
          --content_words = fileToContent' fc  (singleton (nth fc  0)) vvv 1 (vlength fc)
          ft      = fromMaybe def (nth_plist tagfiles Nothing select)       
         in Layout4 (TagList (mkSomeTags 10)) (Content (Plain (mkRandomInlineList 100))) (mkBlogs_layout4 contentfiles tagfiles (length - 1)) (Header (getRandomString (mod rand 9))) (ID (10 - (mod length 10))) (Author (getRandomString (mod rand 9))) (Date (getRandomString (mod rand 9)))
