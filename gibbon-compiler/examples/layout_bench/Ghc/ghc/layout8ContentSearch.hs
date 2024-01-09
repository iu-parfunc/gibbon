import MainGhc 

emphKeywordInContent :: Text -> Blog -> Blog
emphKeywordInContent keyword blogs =
   case blogs of
      End -> End
      Layout8 content rst id author date header tags -> let --present = searchBlogContent keyword content 
                                                            newContent = case content of
                                                                               Content block -> Content (emphasizeKeywordInBlock keyword block)
                                                            newRst     = emphKeywordInContent keyword rst
                                                         in Layout8 newContent newRst id author date header tags



-- main function 
main :: IO ()
main =
    do let blogs1 = mkBlogs_layout8 1000000
       (newblog1, self1, batch1) <- benchFilterBlogsBasedOnKeywordInTagList emphKeywordInContent ['a'] blogs1 1
       putStrLn $ "Length of new Blogs is: "
       print (blogLength newblog1)
       putStrLn $ "Done."
