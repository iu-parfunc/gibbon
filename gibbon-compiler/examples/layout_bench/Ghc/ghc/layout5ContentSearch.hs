import MainGhc 

emphKeywordInContent :: Text -> Blog -> Blog
emphKeywordInContent keyword blogs =
   case blogs of
      End -> End
      Layout5 rst tags content header id author date -> let --present = searchBlogContent keyword content 
                                                            newContent = case content of
                                                                               Content block -> Content (emphasizeKeywordInBlock keyword block)
                                                            newRst     = emphKeywordInContent keyword rst
                                                         in Layout5 newRst tags newContent header id author date



-- main function 
main :: IO ()
main =
    do let blogs1 = mkBlogs_layout5 1000000
       (newblog1, self1, batch1) <- benchFilterBlogsBasedOnKeywordInTagList emphKeywordInContent ['a'] blogs1 1
       putStrLn $ "Length of new Blogs is: "
       print (blogLength newblog1)
       putStrLn $ "Done."
