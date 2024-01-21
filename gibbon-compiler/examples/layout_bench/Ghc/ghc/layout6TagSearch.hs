import MainGhc

emphKeywordInTag :: Text -> Blog -> Blog
emphKeywordInTag keyword blogs = case blogs of
                                    End -> End
                                    Layout6 header id author date content rst tags -> let present     = searchBlogTags keyword tags -- search the tags for the keyword 
                                                                                        in if present then
                                                                                                let newContent  = case content of
                                                                                                                         Content block -> Content (emphasizeKeywordInBlock keyword block)
                                                                                                    newRst      = emphKeywordInTag keyword rst
                                                                                                  in Layout6 header id author date newContent newRst tags
                                                                                           else
                                                                                            let newRst = emphKeywordInTag keyword rst
                                                                                              in Layout6 header id author date content newRst tags




-- main function 
main :: IO ()
main = 
    do let blogs1 = mkBlogs_layout6 400000 500 5
       (newblog1, self1, batch1) <- benchFilterBlogsBasedOnKeywordInTagList emphKeywordInTag ['a'] blogs1 1
       putStrLn $ "Length of new Blogs is: "
       print (blogLength newblog1)
       putStrLn $ "Done."
