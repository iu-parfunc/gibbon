import MainGhc 

emphKeywordInContent :: Text -> Blog -> Blog
emphKeywordInContent keyword blogs = 
   case blogs of 
      End -> End 
      Layout2 content tags rst header id author date -> --let present = searchBlogContent keyword content 
                                                        --  in 
                                                        --    if present then 
                                                        let newContent = case content of 
                                                                               Content block -> Content (emphasizeKeywordInBlock keyword block)
                                                            newRst     = emphKeywordInContent keyword rst 
                                                          in Layout2 newContent tags newRst header id author date
                                                        --    else 
                                                        --       let newRst = emphKeywordInContent keyword rst 
                                                        --         in Layout2 content tags newRst header id author date


-- main function 
main :: IO ()
main =
    do let blogs1 = mkBlogs_layout2 1000000
       (newblog1, self1, batch1) <- benchFilterBlogsBasedOnKeywordInTagList emphKeywordInContent ['a'] blogs1 1
       putStrLn $ "Length of new Blogs is: "
       print (blogLength newblog1)
       putStrLn $ "Done."
