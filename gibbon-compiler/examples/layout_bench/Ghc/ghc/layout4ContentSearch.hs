import MainGhc 

emphKeywordInContent :: Text -> Blog -> Blog
emphKeywordInContent keyword blogs =
   case blogs of
      End -> End
      Layout4 tags content rst header id author date -> let --present = searchBlogContent keyword content 
                                                            newContent = case content of
                                                                               Content block -> Content (emphasizeKeywordInBlock keyword block)
                                                            newRst     = emphKeywordInContent keyword rst
                                                         in Layout4 tags newContent newRst header id author date



-- main function 
main :: IO ()
main =
    do let blogs1 = mkBlogs_layout4 1000000 50 10
       (newblog1, self1, batch1) <- benchFilterBlogsBasedOnKeywordInTagList emphKeywordInContent ['a'] blogs1 1
       putStrLn $ "Length of new Blogs is: "
       print (blogLength newblog1)
       putStrLn $ "Done."
