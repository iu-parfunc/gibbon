import MainGhc

insertKeywordInTagList :: Text -> Blog -> Blog
insertKeywordInTagList keyword blogs = case blogs of
                                            End -> End
                                            Layout6 header id author date content rst tags -> let present = searchBlogTags keyword tags
                                                                                                in if present then
                                                                                                    let newRst  = insertKeywordInTagList keyword rst
                                                                                                      in Layout6 header id author date content newRst tags
                                                                                                   else insertKeywordInTagList keyword rst



-- main function 
main :: IO ()
main = 
    do let blogs1 = mkBlogs_layout6 1000000
       (newblog1, self1, batch1) <- benchFilterBlogsBasedOnKeywordInTagList insertKeywordInTagList ['a'] blogs1 1
       putStrLn $ "Length of new Blogs is: "
       print (blogLength newblog1)
       putStrLn $ "Done."