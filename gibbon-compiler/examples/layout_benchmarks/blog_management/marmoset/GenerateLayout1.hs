module GenerateLayout1 where
import Basics

type Text = Vector Char




mkBlogs_layout1 :: PList Text -> PList Text -> Int -> Blog
mkBlogs_layout1 contentfiles tagfiles length =
   if length < 0 then End
   else 
      let select  = (mod length 10)
          def     = "default file"
          fc      = fromMaybe def (nth_plist contentfiles Nothing select)
          vvv :: Vector (Vector Char) 
          vvv = valloc 0
          --content_words = fileToContent' fc  (singleton (nth fc  0)) vvv 1 (vlength fc)
          ft      = fromMaybe def (nth_plist tagfiles Nothing select)
         in Layout1 (Header (getRandomString (mod rand 9))) (ID (10 - (mod length 10))) (Author (getRandomString (mod rand 9))) (Date (getRandomString (mod rand 9))) (Content (Plain (mkRandomInlineList 500))) (TagList (mkSomeTags 5)) (mkBlogs_layout1 contentfiles tagfiles (length - 1))



--- Traversal 1 (Filter blogs)
-- Content -> 2000 
-- Tags    -> 5 
-- Blogs   -> 200,000 
-- Input always going to then branch


-- Traversal 2 (EmphContent)
-- Content -> 500
-- Tags    -> 10
-- Blogs   -> 400,000
-- Input always going to then branch 


-- Traversal 3 (SearchTag emph Content)
-- Content -> 50 
-- Tags    -> 8 
-- Blogs -> 1,000,000
-- Input always going to then branch