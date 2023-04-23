module Post where 
 
import Gibbon.Curl

gibbon_main = 
    let 
       url :: Vector Char 
       url = "http://0.0.0.0:8000/"
       response :: Int
       response = postToUrl url
       _ = printsym (quote "\n")
    in response
       
