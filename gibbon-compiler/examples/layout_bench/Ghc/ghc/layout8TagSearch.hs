import MainGhc

emphKeywordInTag :: Text -> Blog -> Blog
emphKeywordInTag keyword blogs = case blogs of
                                    End -> End
                                    Layout8 content rst id author date header tags -> let present     = searchBlogTags keyword tags -- search the tags for the keyword 
                                                                                       in if present then
                                                                                                let newContent  = case content of
                                                                                                                        Content block -> Content (emphasizeKeywordInBlock keyword block)
                                                                                                    newRst      = emphKeywordInTag keyword rst
                                                                                                  in Layout8 newContent newRst id author date header tags
                                                                                          else
                                                                                            let newRst = emphKeywordInTag keyword rst
                                                                                             in Layout8 content newRst id author date header tags




-- main function 
main :: IO ()
main = 
    do let blogs1 = mkBlogs_layout8 400000 500 5
       (newblog1, self1, batch1) <- benchFilterBlogsBasedOnKeywordInTagList emphKeywordInTag ['a'] blogs1 1
       putStrLn $ "Length of new Blogs is: "
       print (blogLength newblog1)
       putStrLn $ "Done."
