{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gibbon.Curl where 

import Gibbon.Prelude


postToUrl :: Vector Char -> Int 
postToUrl url = post url 