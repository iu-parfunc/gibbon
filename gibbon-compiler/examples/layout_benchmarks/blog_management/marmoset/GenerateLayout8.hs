-- module GenerateLayout8 where
-- import Basics

-- type Text = Vector Char


-- mkBlogs_layout8 :: PList Text -> PList Text -> Int -> Blog
-- mkBlogs_layout8 contentfiles tagfiles length = 
--    if length < 0 then End 
--    else 
--       let select  = (mod length 10)
--           def     = "default file"
--           fc      = fromMaybe def (nth_plist contentfiles Nothing select)
--           content = mkContentFromText fc
--           rst     = mkBlogs_layout8 contentfiles tagfiles (length - 1)
--           id      = ID (10 - (mod length 10)) 
--           author  = Author (getRandomString (mod rand 9))
--           date    = Date (getRandomString (mod rand 9))
--           header  = Header (getRandomString (mod rand 9))
--           ft      = fromMaybe def (nth_plist tagfiles Nothing select)
--           tags    = mkTagsFromText ft
--        in Layout8 content rst id author date header tags


module GenerateLayout8 where
import Basics

type Text = Vector Char


mkBlogs_layout8 :: PList Text -> PList Text -> Int -> Blog
mkBlogs_layout8 contentfiles tagfiles length = 
   if length < 0 then End 
   else 
      let select  = (mod length 10)
          def     = "default file"
          fc      = fromMaybe def (nth_plist contentfiles Nothing select)
          vvv :: Vector (Vector Char) 
          vvv = valloc 0
          --content_words = fileToContent' fc  (singleton (nth fc  0)) vvv 1 (vlength fc)
          ft      = fromMaybe def (nth_plist tagfiles Nothing select)
         in Layout8 (Content (Plain (mkRandomInlineList 1000))) (mkBlogs_layout8 contentfiles tagfiles (length - 1)) (ID (10 - (mod length 10))) (Author (getRandomString (mod rand 9))) (Date (getRandomString (mod rand 9))) (Header (getRandomString (mod rand 9))) (TagList (mkSomeTags 2)) 
