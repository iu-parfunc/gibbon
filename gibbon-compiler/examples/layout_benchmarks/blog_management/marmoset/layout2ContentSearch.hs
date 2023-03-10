import Basics
import GenerateLayout2

type Text   = Vector Char

emphKeywordInContent :: Text -> Blog -> Blog
{-# ANN emphKeywordInContent Layout2 #-}
emphKeywordInContent keyword blogs = 
   case blogs of 
      End -> End 
      Layout2 content tags rst header id author date -> let present = searchBlogContent keyword content 
                                                          in 
                                                            if present then 
                                                               let newContent = emphasizeBlogContent' keyword content 
                                                                   newRst     = emphKeywordInContent keyword rst 
                                                                 in Layout2 newContent tags newRst header id author date
                                                            else 
                                                               let newRst = emphKeywordInContent keyword rst 
                                                                 in Layout2 content tags newRst header id author date
                                                          

-- main function 
gibbon_main = 
   let --blogs  = mkBlogs_layout2 200000 0 1200                       -- mkBlogs_layout1 length start_id tag_length
       --keyword = (getRandomString 2)                                -- some random keyword
       --new_blogs = iterate (emphKeywordInContent keyword blogs)
       --_          = printsym (quote "NEWLINE")
       --_          = printsym (quote "NEWLINE") 
       --_          = printPacked new_blogs1
       --_          = printsym (quote "NEWLINE")
       --_          = printsym (quote "NEWLINE")
       fc1, fc2, fc3, fc4, fc5, fc6, fc7, fc8, fc9, fc10 :: Text
       ft1, ft2, ft3, ft4, ft5, ft6, ft7, ft8, ft9, ft10 :: Text
       fc1       = readArrayFile (Just ("blog1/blog1Out.txt",  5912))
       fc2       = readArrayFile (Just ("blog2/blog2Out.txt",  5906)) 
       fc3       = readArrayFile (Just ("blog3/blog3Out.txt",  5893))
       fc4       = readArrayFile (Just ("blog4/blog4Out.txt",  5903))
       fc5       = readArrayFile (Just ("blog5/blog5Out.txt",  5888)) 
       fc6       = readArrayFile (Just ("blog6/blog6Out.txt",  5890)) 
       fc7       = readArrayFile (Just ("blog7/blog7Out.txt",  5889))
       fc8       = readArrayFile (Just ("blog8/blog8Out.txt",  5886))
       fc9       = readArrayFile (Just ("blog9/blog9Out.txt",  5862))
       fc10      = readArrayFile (Just ("blog10/blog10Out.txt",  5887))
       ft1       = readArrayFile (Just ("blog1/blog1Tag.txt", 508))
       ft2       = readArrayFile (Just ("blog2/blog2Tag.txt", 496))
       ft3       = readArrayFile (Just ("blog3/blog3Tag.txt", 598))
       ft4       = readArrayFile (Just ("blog4/blog4Tag.txt", 485))
       ft5       = readArrayFile (Just ("blog5/blog5Tag.txt", 569))
       ft6       = readArrayFile (Just ("blog6/blog6Tag.txt", 533))
       ft7       = readArrayFile (Just ("blog7/blog7Tag.txt", 516))
       ft8       = readArrayFile (Just ("blog8/blog8Tag.txt", 669))
       ft9       = readArrayFile (Just ("blog9/blog9Tag.txt", 573))
       ft10      = readArrayFile (Just ("blog10/blog10Tag.txt", 485))
       lfc       = mkListFiles fc1 fc2 fc3 fc4 fc5 fc6 fc7 fc8 fc9 fc10 9
       ltc       = mkListFiles ft1 ft2 ft3 ft4 ft5 ft6 ft7 ft8 ft9 ft10 9
       blogs = mkBlogs_layout2 lfc ltc 400000
       --_ = printPacked blogs
       --_ = printsym (quote "NEWLINE")
       --_ = printsym (quote "NEWLINE")
       --_ = printsym (quote "NEWLINE")
       --_ = printsym (quote "NEWLINE")
       --_ = printsym (quote "NEWLINE")
       keyword :: Vector Char  
       keyword = "a"
       newblgs = iterate (emphKeywordInContent keyword blogs)
       --_ = printPacked newblgs
       --_ = printsym (quote "NEWLINE")
       --_ = printsym (quote "NEWLINE")
   in ()
