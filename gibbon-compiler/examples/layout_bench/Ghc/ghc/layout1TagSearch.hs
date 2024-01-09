import MainGhc

emphKeywordInTag :: Text -> Blog -> Blog
emphKeywordInTag keyword blogs = case blogs of
                                    End -> End
                                    Layout1 header id author date content tags rst -> let present = searchBlogTags keyword tags -- search the tags for the keyword 
                                                                                          --_       = printbool present
                                                                                          in if (present)
                                                                                             then let newContent  = case content of
                                                                                                                         Content block -> Content (emphasizeKeywordInBlock keyword block)
                                                                                                      newRst      = emphKeywordInTag keyword rst
                                                                                                    in Layout1 header id author date newContent tags newRst
                                                                                             else
                                                                                               let newRst = emphKeywordInTag keyword rst
                                                                                                 in Layout1 header id author date content tags newRst



-- main function 
main :: IO ()
main = 
    do let blogs1 = mkBlogs_layout1 400000
       (newblog1, self1, batch1) <- benchFilterBlogsBasedOnKeywordInTagList emphKeywordInTag ['a'] blogs1 1
       putStrLn $ "Length of new Blogs is: "
       print (blogLength newblog1)
       putStrLn $ "Done."