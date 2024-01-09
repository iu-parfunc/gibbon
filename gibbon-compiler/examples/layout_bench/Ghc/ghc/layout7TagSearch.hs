import MainGhc

emphKeywordInTag :: Text -> Blog -> Blog
emphKeywordInTag keyword blogs = case blogs of
                                    End -> End
                                    Layout7 rst content header id author date tags -> let present     = searchBlogTags keyword tags -- search the tags for the keyword 
                                                                                        in if present then
                                                                                            let newContent  = case content of
                                                                                                                         Content block -> Content (emphasizeKeywordInBlock keyword block)
                                                                                                newRst      = emphKeywordInTag keyword rst
                                                                                              in Layout7 newRst newContent header id author date tags
                                                                                           else
                                                                                            let newRst = emphKeywordInTag keyword rst
                                                                                             in Layout7 newRst content header id author date tags





-- main function 
main :: IO ()
main = 
    do let blogs1 = mkBlogs_layout7 400000
       (newblog1, self1, batch1) <- benchFilterBlogsBasedOnKeywordInTagList emphKeywordInTag ['a'] blogs1 1
       putStrLn $ "Length of new Blogs is: "
       print (blogLength newblog1)
       putStrLn $ "Done."