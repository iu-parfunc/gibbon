import MainGhc

emphKeywordInTag :: Text -> Blog -> Blog
emphKeywordInTag keyword blogs = case blogs of
                                    End -> End
                                    Layout4 tags content rst header id author date -> let present     = searchBlogTags keyword tags
                                                                                        in if (present) then
                                                                                             let newContent  = case content of
                                                                                                                         Content block -> Content (emphasizeKeywordInBlock keyword block)
                                                                                                 newRst      = emphKeywordInTag keyword rst
                                                                                            in Layout4 tags newContent newRst header id author date
                                                                                           else
                                                                                            let newRst = emphKeywordInTag keyword rst
                                                                                             in Layout4 tags content newRst header id author date




-- main function 
main :: IO ()
main = 
    do let blogs1 = mkBlogs_layout4 400000 500 5
       (newblog1, self1, batch1) <- benchFilterBlogsBasedOnKeywordInTagList emphKeywordInTag ['a'] blogs1 1
       putStrLn $ "Length of new Blogs is: "
       print (blogLength newblog1)
       putStrLn $ "Done."
