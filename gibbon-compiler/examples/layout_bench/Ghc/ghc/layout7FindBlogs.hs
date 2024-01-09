import MainGhc

insertKeywordInTagList :: Text -> Blog -> Blog
insertKeywordInTagList keyword blogs = case blogs of
                                            End -> End
                                            Layout7 rst content header id author date tags -> let present = searchBlogTags keyword tags
                                                                                                in if present then
                                                                                                     let newRst  = insertKeywordInTagList keyword rst
                                                                                                      in Layout7 newRst content header id author date tags
                                                                                                   else insertKeywordInTagList keyword rst



-- main function 
main :: IO ()
main = 
    do let blogs1 = mkBlogs_layout7 1000000
       (newblog1, self1, batch1) <- benchFilterBlogsBasedOnKeywordInTagList insertKeywordInTagList ['a'] blogs1 1
       putStrLn $ "Length of new Blogs is: "
       print (blogLength newblog1)
       putStrLn $ "Done."