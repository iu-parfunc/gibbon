import MainGhc 

emphKeywordInContent :: Text -> Blog -> Blog
emphKeywordInContent keyword blogs =
   case blogs of
      End -> End
      Layout1 header id author date content tags rst -> let --present = searchBlogContent keyword content
                                                            --_ = printsym (quote "NEWLINE") 
                                                            --_       = printbool present
                                                            --_ = printsym (quote "NEWLINE")
                                                            newContent = case content of
                                                                               Content block -> Content (emphasizeKeywordInBlock keyword block)
                                                            --_ = printsym (quote "NEWLINE")
                                                            --_          = printPacked newContent
                                                            --_ = printsym (quote "NEWLINE") 
                                                            newRst     = emphKeywordInContent keyword rst
                                                        in Layout1 header id author date newContent tags newRst


-- main function 
main :: IO ()
main =
    do let blogs1 = mkBlogs_layout1 1000000 50 10
       (newblog1, self1, batch1) <- benchFilterBlogsBasedOnKeywordInTagList emphKeywordInContent ['a'] blogs1 1
       putStrLn $ "Length of new Blogs is: "
       print (blogLength newblog1)
       putStrLn $ "Done."
