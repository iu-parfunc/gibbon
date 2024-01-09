import MainGhc 

emphKeywordInContent :: Text -> Blog -> Blog
emphKeywordInContent keyword blogs =
   case blogs of
      End -> End
      Layout6 header id author date content rst tags -> let --present = searchBlogContent keyword content 
                                                            newContent = case content of
                                                                               Content block -> Content (emphasizeKeywordInBlock keyword block)
                                                            newRst     = emphKeywordInContent keyword rst
                                                         in Layout6 header id author date newContent newRst tags



-- main function 
main :: IO ()
main =
    do let blogs1 = mkBlogs_layout6 1000000
       (newblog1, self1, batch1) <- benchFilterBlogsBasedOnKeywordInTagList emphKeywordInContent ['a'] blogs1 1
       putStrLn $ "Length of new Blogs is: "
       print (blogLength newblog1)
       putStrLn $ "Done."
