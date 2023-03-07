module GenerateLayout6 where
import Basics

type Text = Vector Char

mkBlogs_layout6 :: PList Text -> PList Text -> Int -> Blog
mkBlogs_layout6 contentfiles tagfiles length =
   if length < 0 then End
   else 
      let select  = (mod length 10)
          def     = "default file"
          fc      = fromMaybe def (nth_plist contentfiles Nothing select)
          vvv :: Vector (Vector Char) 
          vvv = valloc 0
          content_words = fileToContent' fc  (singleton (nth fc  0)) vvv 1 (vlength fc)
          ft      = fromMaybe def (nth_plist tagfiles Nothing select)      
         in Layout6 (Header (getRandomString (mod rand 9))) (ID (10 - (mod length 10))) (Author (getRandomString (mod rand 9))) (Date (getRandomString (mod rand 9))) (Content (Plain (mkInlineList' (vlength content_words) 0 content_words ))) (mkBlogs_layout6 contentfiles tagfiles (length - 1)) (mkTagsFromText ft)