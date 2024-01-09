import MainGhc

emphKeywordInTag :: Text -> Blog -> Blog
emphKeywordInTag keyword blogs = case blogs of
                                    End -> End
                                    Layout5 rst tags content header id author date -> let present     = searchBlogTags keyword tags -- search the tags for the keyword 
                                                                                       in if present then
                                                                                              let newContent  = case content of
                                                                                                                         Content block -> Content (emphasizeKeywordInBlock keyword block)
                                                                                                  newRst      = emphKeywordInTag keyword rst
                                                                                                in Layout5 newRst tags newContent header id author date
                                                                                          else
                                                                                            let newRst = emphKeywordInTag keyword rst
                                                                                              in Layout5 newRst tags content header id author date




-- main function 
main :: IO ()
main = 
    do let blogs1 = mkBlogs_layout5 400000
       (newblog1, self1, batch1) <- benchFilterBlogsBasedOnKeywordInTagList emphKeywordInTag ['a'] blogs1 1
       putStrLn $ "Length of new Blogs is: "
       print (blogLength newblog1)
       putStrLn $ "Done."