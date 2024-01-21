import MainGhc


insertKeywordInTagList :: Text -> Blog -> Blog 
insertKeywordInTagList keyword blogs = case blogs of 
                                            End -> End 
                                            Layout2 content tags rst header id author date -> let present = searchBlogTags keyword tags
                                                                                                in if present then 
                                                                                                    let newRst  = insertKeywordInTagList keyword rst
                                                                                                     in Layout2 content tags newRst header id author date
                                                                                                   else insertKeywordInTagList keyword rst


-- main function 
main :: IO ()
main = 
    do let blogs1 = mkBlogs_layout2 1000000 500 5
       (newblog1, self1, batch1) <- benchFilterBlogsBasedOnKeywordInTagList insertKeywordInTagList ['a'] blogs1 1
       putStrLn $ "Length of new Blogs is: "
       print (blogLength newblog1)
       putStrLn $ "Done."
