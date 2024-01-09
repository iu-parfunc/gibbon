{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Strict #-}
module MainGhc where

import System.TimeIt
import System.CPUTime
import qualified Data.Vector as V
import System.Random
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Exception (evaluate)

import qualified Language.Haskell.TH as TH

--import Text.StringRandom

import Control.DeepSeq
import Data.Int
import Data.List
import Text.Printf

import Control.Monad
import System.IO.Unsafe

data PList a = Nil | Cons a (PList a) deriving (Show)

type Text   = String 

-- For simplicity, we are assuming for this benchmark that data Inline is tokenized at the "word" level.
-- Therefore, The Base case where "Text" is used is going to be a single word, i.e, "Str Text".
data Inline =     Str Text
                | Emph (PList Inline)
             -- | Underline (PList Inline)
             -- | Strong (PList Inline)
             -- | Strikeout (PList Inline)
             -- | Superscript (PList Inline)
             -- | Subscript (PList Inline)
             -- | SmallCaps (PList Inline)
             -- | Quoted QuoteType (PList Inline)
             -- | Cite [Citation] (PList Inline)
             -- | Code Attr Text
                | Space
             -- | SoftBreak
             -- | LineBreak
             -- | Math MathType Text
             -- | RawInline Format Text
             -- | Link Attr (PList Inline) Target
             -- | Image Attr (PList Inline) Target
             -- | Note (PList Block)
             -- | Span Attr (PList Inline)
                deriving (Show)  

data Block =      Plain (PList Inline)
             -- | Para  (PList Inline)
             -- | LineBlock (PList (PList Inline))
             -- | CodeBlock Attr Text
             -- | RawBlock Format Text
             -- | BlockQuote (PList Block)
             -- | OrderedList ListAttributes [[Block]]
             -- | BulletList (PList (PList Block))
             -- | DefinitionList PList ( PList Inline , PList (PList Block) ) ---> This is resulting in a compile time error (TODO: DEBUG)
             -- | Header Int Attr (PList Inline)
             -- | HorizontalRule
             -- | Table Attr Caption [ColSpec] TableHead [TableBody] TableFoot
             -- | Div Attr (PList Block)
                | Null
                deriving (Show)

-- Define Blog elements
data BlogHeader  = Header Text           deriving (Show)
data BlogId      = ID Int                deriving (Show)
data BlogAuthor  = Author Text           deriving (Show)
data BlogDate    = Date Text             deriving (Show)
data BlogContent = Content Block         deriving (Show) 
data BlogTags    = TagList (PList Text)  deriving (Show)

-- Define packed Blog data Type/s, we can arrange the fields here to change their relative ordering. 
data Blog =   End 
            | Layout1 (BlogHeader) (BlogId) (BlogAuthor) (BlogDate) (BlogContent) (BlogTags) (Blog)
            | Layout2 (BlogContent) (BlogTags) (Blog) (BlogHeader) (BlogId) (BlogAuthor) (BlogDate) 
            | Layout3 (BlogTags) (Blog) (BlogContent) (BlogHeader) (BlogId) (BlogAuthor) (BlogDate)
            | Layout4 (BlogTags) (BlogContent) (Blog) (BlogHeader) (BlogId) (BlogAuthor) (BlogDate)
            | Layout5 (Blog) (BlogTags) (BlogContent) (BlogHeader) (BlogId) (BlogAuthor) (BlogDate)
            | Layout6 (BlogHeader) (BlogId) (BlogAuthor) (BlogDate) (BlogContent) (Blog) (BlogTags)
            | Layout7 (Blog) (BlogContent) (BlogHeader) (BlogId) (BlogAuthor) (BlogDate) (BlogTags)
            | Layout8 (BlogContent) (Blog) (BlogId) (BlogAuthor) (BlogDate) (BlogHeader) (BlogTags)
            deriving (Show)

getChar' :: Int -> Char
getChar' decimal =  
        if decimal == 0 then '!'
        else if decimal == 1 then '#'
        else if decimal == 2 then '$'
        else if decimal == 3 then '%'
        else if decimal == 4 then '&'
        else if decimal == 5 then '('
        else if decimal == 6 then ')'
        else if decimal == 7 then '*'
        else if decimal == 8 then '+'
        else if decimal == 9 then ','
        else if decimal == 10 then '-'
        else if decimal == 11 then '.'
        else if decimal == 12 then '/'
        else if decimal == 13 then '0'
        else if decimal == 14 then '1'           
        else if decimal == 15 then '2'
        else if decimal == 16 then '3'
        else if decimal == 17 then '4'
        else if decimal == 18 then '5'
        else if decimal == 19 then '6'
        else if decimal == 20 then '7'
        else if decimal == 21 then '8'
        else if decimal == 22 then '9'
        else if decimal == 23 then ':'
        else if decimal == 24 then ';'
        else if decimal == 25 then '<'
        else if decimal == 26 then '='
        else if decimal == 27 then '>'
        else if decimal == 28 then '?'
        else if decimal == 29 then '@'
        else if decimal == 30 then 'A'
        else if decimal == 31 then 'B'
        else if decimal == 32 then 'C'
        else if decimal == 33 then 'D'
        else if decimal == 34 then 'E'
        else if decimal == 35 then 'F'
        else if decimal == 36 then 'G'
        else if decimal == 37 then 'H'
        else if decimal == 38 then 'I'
        else if decimal == 39 then 'J'
        else if decimal == 40 then 'K'
        else if decimal == 41 then 'L'
        else if decimal == 42 then 'M'
        else if decimal == 43 then 'N'
        else if decimal == 44 then 'O'
        else if decimal == 45 then 'P'
        else if decimal == 46 then 'Q'
        else if decimal == 47 then 'R'
        else if decimal == 48 then 'S'
        else if decimal == 49 then 'T'
        else if decimal == 50 then 'U'
        else if decimal == 51 then 'V'
        else if decimal == 52 then 'W'
        else if decimal == 53 then 'X'
        else if decimal == 54 then 'Y'
        else if decimal == 55 then 'Z'
        else if decimal == 56 then '['
        else if decimal == 57 then ']'
        else if decimal == 58 then '^'
        else if decimal == 59 then '_'
        else if decimal == 60 then '`'
        else if decimal == 61 then 'a'
        else if decimal == 62 then 'b'
        else if decimal == 63 then 'c'
        else if decimal == 64 then 'd'
        else if decimal == 65 then 'e'
        else if decimal == 66 then 'f'
        else if decimal == 67 then 'g'
        else if decimal == 68 then 'h'
        else if decimal == 69 then 'i'
        else if decimal == 70 then 'j'
        else if decimal == 71 then 'k'
        else if decimal == 72 then 'l'
        else if decimal == 73 then 'm'
        else if decimal == 74 then 'n'
        else if decimal == 75 then 'o'
        else if decimal == 76 then 'p'
        else if decimal == 77 then 'q'
        else if decimal == 78 then 'r'
        else if decimal == 79 then 's'
        else if decimal == 80 then 't'
        else if decimal == 81 then 'u'
        else if decimal == 82 then 'v'
        else if decimal == 83 then 'w'
        else if decimal == 84 then 'x'
        else if decimal == 85 then 'y'
        else if decimal == 86 then 'z'
        else if decimal == 87 then '{'
        else if decimal == 88 then '|'
        else if decimal == 89 then '}'
        else '~'

mkChar :: Int -> StdGen -> Char 
mkChar val r = let (val, _) = randomR (0, 91 :: Int) r
                in getChar' val

-- Get a random word, Int is the length of the string.
-- Based on internet, average english word is 5 characters long
getRandomString :: Int -> Text 
getRandomString length =  unsafePerformIO $ randString

-- A standard function generating random strings.
randString :: IO String
randString = liftM (take 5 . randomRs ('a','z')) newStdGen

-- A standard function generating random strings.
randStrinContent :: IO String
randStrinContent = liftM (take 1 . randomRs ('a','z')) newStdGen

randInt :: IO Int
randInt = liftM (mod 4 . head . take 1 . randomRs (0, maxBound :: Int)) newStdGen

randStringTag :: IO String
randStringTag = liftM (take 1 . randomRs ('a','z')) newStdGen

-- .. lifted to Q
randStringQ :: TH.Q String
randStringQ = TH.runIO randString

-- .. lifted to an Q Exp
randStringExp :: TH.Q TH.Exp
randStringExp = randStringQ >>= TH.litE . TH.stringL

-- | Declares a constant `String` function with a given name
-- that returns a random string generated on compile time.
randStringD :: String -> TH.DecsQ
randStringD fname = liftM (: []) $
    TH.funD (TH.mkName fname) [TH.clause [] (TH.normalB randStringExp) []]


-- Utility Functions to make Blogs and its Elements.
mkBlogHeader :: Text -> BlogHeader
mkBlogHeader text = Header text 

mkBlogID :: Int -> BlogId 
mkBlogID val = ID val 

mkBlogAuthor :: Text -> BlogAuthor 
mkBlogAuthor text = Author text

mkBlogDate :: Text -> BlogDate 
mkBlogDate text = Date text 

mkBlogContent :: Block -> BlogContent 
mkBlogContent block = Content block 

mkBlogTags :: (PList Text) -> BlogTags 
mkBlogTags taglist = TagList taglist 


checkBlogID :: BlogId -> Int -> Bool
checkBlogID id val = case id of 
                        ID x -> if ( x == val ) then True 
                                                else False

-- Function to compare two words, each represented by String. 
compareWord :: Text -> Text -> Bool
compareWord word1 word2 = 
   let len1        = length word1 
       len2        = length word2
       compare_len = if (len1 == len2) then True else False     
   in if (compare_len) then (cmp 0 len1 word1 word2) else False

-- Compare 2 String (Text) or words for equality if their length is the same. 
cmp :: Int -> Int -> String -> String -> Bool
cmp start end word1 word2 =
   if (start < end) then 
      let a       = word1 !! start
          b       = word2 !! start
          eq      = if (a == b) then True else False 
          recurse = cmp (start+1) end word1 word2
         in (eq && recurse) 
   else True

-- Search a TagList (PList Text) for some keyword
searchTagList :: Text -> PList Text -> Bool 
searchTagList keyword taglist = case taglist of 
   Nil -> False 
   Cons word rst -> (compareWord keyword word) || (searchTagList keyword rst) 

-- delete a keyword in a TagList
deleteTagList :: Text -> PList Text -> PList Text 
deleteTagList keyword taglist = case taglist of 
   Nil -> Nil 
   Cons word rst -> if (compareWord keyword word) then (deleteTagList keyword rst) 
                    else Cons word (deleteTagList keyword rst)


insertTagList :: Text -> PList Text -> PList Text 
insertTagList keyword taglist = case taglist of 
   Nil -> (Cons keyword) Nil
   Cons word rst -> insertTagList keyword rst

-- Tell if a particular keyword exists in a Block data type or not
isKeywordPresentInBlock :: Text -> Block -> Bool
isKeywordPresentInBlock keyword contentBlock = 
   case contentBlock of 
      Plain list_inline      -> (searchInlineListForKeyword keyword list_inline)
      --Para  list_inline      -> (searchInlineListForKeyword keyword list_inline)
      --BlockQuote list_block  -> (searchBlockListForKeyword keyword list_block)
      --HorizontalRule         -> False
      Null                   -> False 

-- Tell if a particular keyword exists in an inline data type or not. (search a Inline)
isKeywordPresentInline :: Text -> Inline -> Bool
isKeywordPresentInline keyword inline = 
   case inline of 
      Str text                -> (compareWord keyword text)
      Emph list_inline        -> (searchInlineListForKeyword keyword list_inline)
      --Underline list_inline   -> (searchInlineListForKeyword keyword list_inline)
      --Strong list_inline      -> (searchInlineListForKeyword keyword list_inline)
      --Strikeout list_inline   -> (searchInlineListForKeyword keyword list_inline)
      --Superscript list_inline -> (searchInlineListForKeyword keyword list_inline)
      --Subscript list_inline   -> (searchInlineListForKeyword keyword list_inline) 
      --SmallCaps list_inline   -> (searchInlineListForKeyword keyword list_inline) 
      Space                   -> False
      --SoftBreak               -> False 
      --LineBreak               -> False 
      --Note list_block         -> (searchBlockListForKeyword keyword list_block)

-- Search a block list for a particular keyword
searchBlockListForKeyword :: Text -> PList Block -> Bool 
searchBlockListForKeyword keyword block_list = 
   case block_list of 
      Nil             -> False
      Cons block rst  -> (isKeywordPresentInBlock keyword block) || (searchBlockListForKeyword keyword rst) 

-- Search an Inline list for a particular keyword
searchInlineListForKeyword :: Text -> PList Inline -> Bool
searchInlineListForKeyword keyword inline_list = 
   case inline_list of 
      Nil                -> False 
      Cons inline rst    -> (isKeywordPresentInline keyword inline) || (searchInlineListForKeyword keyword rst)

-- Emphasize a particular keyword in a Block type
emphasizeKeywordInBlock :: Text -> Block -> Block
emphasizeKeywordInBlock keyword contentBlock = 
   case contentBlock of 
      Plain list_inline      -> Plain (emphasizeInlineListForKeyword keyword list_inline)
      --Para  list_inline      -> Para  (emphasizeInlineListForKeyword keyword list_inline)
      --BlockQuote list_block  -> BlockQuote (emphasizeKeywordInBlockList keyword list_block)
      --HorizontalRule         -> HorizontalRule
      Null                   -> Null

-- Emphasize a particular keyword in an Inline data type
emphasizeKeywordInline :: Text -> Inline -> Inline 
emphasizeKeywordInline keyword inline = 
   case inline of 
      Str text           -> let isSame = compareWord keyword text 
                                --_ = printsym (quote "NEWLINE")
                                --_       = printbool isSame
                                --_ = printsym (quote "NEWLINE")
                                in if (isSame) then let
                                       newlist :: PList Inline 
                                       newlist = (Cons (inline)) Nil                        
                                    in (Emph newlist)
                                   else inline
      Emph list_inline        -> Emph (emphasizeInlineListForKeyword keyword list_inline)
      --Underline list_inline   -> Underline (emphasizeInlineListForKeyword keyword list_inline)
      --Strong list_inline      -> Strong (emphasizeInlineListForKeyword keyword list_inline)
      --Strikeout list_inline   -> Strikeout (emphasizeInlineListForKeyword keyword list_inline)
      --Superscript list_inline -> Superscript (emphasizeInlineListForKeyword keyword list_inline)
      --Subscript list_inline   -> Subscript (emphasizeInlineListForKeyword keyword list_inline) 
      --SmallCaps list_inline   -> SmallCaps (emphasizeInlineListForKeyword keyword list_inline) 
      Space                   -> Space
      --SoftBreak               -> SoftBreak 
      --LineBreak               -> LineBreak 
      --Note list_block         -> Note (emphasizeKeywordInBlockList keyword list_block) 

-- Emphasize a particular keyword in an Inline list
emphasizeInlineListForKeyword :: Text -> PList Inline -> PList Inline
{-# INLINE emphasizeInlineListForKeyword #-}
emphasizeInlineListForKeyword keyword inline_list = 
   case inline_list of 
      Nil                -> Nil 
      Cons inline rst    -> let 
                             newinline = emphasizeKeywordInline keyword inline
                             rst'      = emphasizeInlineListForKeyword keyword rst  
                             in Cons newinline rst'

-- Emphasize a particular keyword in a block list
emphasizeKeywordInBlockList :: Text -> PList Block -> PList Block 
emphasizeKeywordInBlockList keyword block_list = 
   case block_list of 
      Nil             -> Nil
      Cons block rst  -> let
                           newBlock = emphasizeKeywordInBlock keyword block 
                           rst'     = emphasizeKeywordInBlockList keyword rst 
                           in Cons newBlock rst'
                           

searchBlogTags :: Text -> BlogTags -> Bool 
searchBlogTags keyword tags = case tags of 
   TagList list -> searchTagList keyword list


deleteBlogTags :: Text -> BlogTags -> BlogTags
deleteBlogTags keyword tags = case tags of 
   TagList list -> TagList (deleteTagList keyword list)

insertBlogTags :: Text -> BlogTags -> BlogTags 
insertBlogTags keyword tags = case tags of 
   TagList list -> TagList (insertTagList keyword list)

-- emphasize blog content, if present is True
emphasizeBlogContent' :: Text -> BlogContent -> BlogContent
emphasizeBlogContent' keyword oldContent = case oldContent of 
                                                Content block -> Content (emphasizeKeywordInBlock keyword block)



emphasizeBlogContent :: Text -> BlogContent -> BlogContent
emphasizeBlogContent keyword oldContent = case oldContent of 
                                             Content block -> Content (emphasizeKeywordInBlock keyword block)

searchBlogContent :: Text -> BlogContent -> Bool 
searchBlogContent keyword content = case content of 
      Content block -> (isKeywordPresentInBlock keyword block)

-- printWordList :: TextList -> Int -> Int -> ()
-- printWordList vec start end = if start < end then
--                                  let 
--                                     element = V.unsafeIndex vec start
--                                     _   = printVec (\i -> printchar i) element
--                                   in printWordList vec (start+1) end
--                               else ()  
     

-- Make an Inline type, option chooses what kind of Inline data type we are creating
-- This creates all the base cases for the inline type 
mkInlineBaseCase :: Int -> Inline
mkInlineBaseCase option = let 
                           in if option == 0 then (Str (getRandomString 5))   -- get a random word
                              else Space 

-- Make a list of Inline data Type.
mkInlineList :: Int -> Int -> (PList Inline)
mkInlineList length base = 
   if length <= 0 then Nil 
   -- If its not base case, then don't stop recursion. 
   else if (base == 0) then 
      let item = Emph (mkInlineList 100 1)
          rst  = (mkInlineList (length - 1) base)
          in Cons item rst
   -- If its  base case, then stop recursion in Inline data type and only add base cases. 
   else let item = (mkInlineBaseCase (unsafePerformIO $ randInt))
            rst  = mkInlineList (length - 1) base 
         in Cons item rst

mkRandomInlineList :: Int -> (PList Inline)
mkRandomInlineList len = if len <= 0 then Nil 
                         else let word = Str (unsafePerformIO $ randStrinContent)
                                  rst  = mkRandomInlineList (len - 1) 
                                in Cons word rst

mkSomeTags :: Int -> PList Text
mkSomeTags len = if len <= 0 then Nil 
                 else let word =  ['a'] --unsafePerformIO $ randStringTag
                          rst  = mkSomeTags (len - 1)
                       in Cons word rst


-- Make a list of blocks
mkBlockList :: Int -> Int -> (PList Block)
mkBlockList length base = 
   if length <= 0 then Nil
   else if (base == 0) then
      let item = (Plain (mkInlineList 1000 1))
          rst  = (mkBlockList (length - 1) base)
      in Cons item rst
   else let item = Null
            rst  = (mkBlockList (length - 1) base) 
      in Cons item rst



--------------------------------------------------------------------------------
-- Timing things
--------------------------------------------------------------------------------
median :: [Double] -> Double
median ls = (sort ls) !! (length ls `div` 2)
--------------------------------------------------------------------------------

-- -- Timing for making the layouts
-- dotrialMkLayout :: (Int -> Int -> Int -> Blog) -> Int -> Int -> Int -> IO (Blog, Double)
-- dotrialMkLayout f length id tag_len = do
--     t1 <- getCurrentTime
--     a <- evaluate $ (f length id tag_len)
--     t2 <- getCurrentTime
--     let delt = fromRational (toRational (diffUTCTime t2 t1))
--     putStrLn ("iter time: " ++ show delt)
--     return $! (a,delt)

-- benchMkLayout :: (Int -> Int -> Int -> Blog) -> Int -> Int -> Int -> Int -> IO (Blog, Double, Double)
-- benchMkLayout f length id tag_len iters = do
--     putStrLn ("Timings for making the particular layout")
--     tups <- mapM (\_ -> dotrialMkLayout f length id tag_len) [1..iters]
--     let (results, times) = unzip tups
--     let selftimed = median times
--         batchtime = sum times
--     return $! (last results, selftimed, batchtime)

-- -- Timing for filtering the blogs based on a keyword
-- dotrialFilterBlogsBasedOnKeywordInTagList :: (Text -> Blog -> Blog) -> Text -> Blog -> IO (Blog, Double)
-- dotrialFilterBlogsBasedOnKeywordInTagList f keyword blogs = do
--     t1 <- getCurrentTime
--     a <- evaluate $ (f keyword blogs)
--     t2 <- getCurrentTime
--     let delt = fromRational (toRational (diffUTCTime t2 t1))
--     putStrLn ("iter time: " ++ show delt)
--     return $! (a,delt)

-- benchFilterBlogsBasedOnKeywordInTagList :: (Text -> Blog -> Blog) -> Text -> Blog -> Int -> IO (Blog, Double, Double)
-- benchFilterBlogsBasedOnKeywordInTagList f keyword blog iters = do
--     putStrLn ("Timings for filering blogs based on a keyword in the tag list")
--     tups <- mapM (\_ -> dotrialFilterBlogsBasedOnKeywordInTagList f keyword blog) [1..iters]
--     let (results, times) = unzip tups
--     let selftimed = median times
--         batchtime = sum times
--     return $! (last results, selftimed, batchtime)

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.6f sec\n" (diff :: Double)
    return v


mkBlogs_layout1 :: Int -> Blog
mkBlogs_layout1 length =
   if length < 0 then End
   else 
      let header = (Header (getRandomString 5))
          id     = (ID (10 - (mod length 10)))
          author = (Author (getRandomString 5))
          date   =  (Date (getRandomString 5))
          content = (Content (Plain (mkRandomInlineList 50)))
          tags    = (TagList (mkSomeTags 10))
          rst     = (mkBlogs_layout1 (length - 1))
         in Layout1 header id author date content tags rst

mkBlogs_layout2 :: Int -> Blog
mkBlogs_layout2 length =
   if length < 0 then End
   else 
      let content = (Content (Plain (mkRandomInlineList 50))) 
          tags    = (TagList (mkSomeTags 10))    
          rst     = (mkBlogs_layout2 (length - 1))
          header  = (Header (getRandomString 5))
          id      = (ID (10 - (mod length 10)))
          author  = (Author (getRandomString 5))
          date    = (Date (getRandomString 5))
         in Layout2 content tags rst header id author date   

mkBlogs_layout3 :: Int -> Blog
mkBlogs_layout3 length =
   if length < 0 then End
   else 
      let tags =   (TagList (mkSomeTags 10))
          rst  = (mkBlogs_layout3 (length - 1))
          content = (Content (Plain (mkRandomInlineList 50)))
          header  = (Header (getRandomString 5))
          id      = (ID (10 - (mod length 10)))
          author = (Author (getRandomString 5))
          date   = (Date (getRandomString 5))
         in Layout3 tags rst content header id author date

mkBlogs_layout4 :: Int -> Blog
mkBlogs_layout4 length =
   if length < 0 then End
   else 
      let tags = (TagList (mkSomeTags 10))
          content = (Content (Plain (mkRandomInlineList 50)))
          rst = (mkBlogs_layout4 (length - 1))
          header = (Header (getRandomString 5))
          id     = (ID (10 - (mod length 10)))
          author = (Author (getRandomString 5))
          date   =  (Date (getRandomString 5))
         in Layout4 tags content rst header id author date

mkBlogs_layout5 :: Int -> Blog
mkBlogs_layout5 length =
   if length < 0 then End
   else 
      let rst = (mkBlogs_layout5 (length - 1))
          tags = (TagList (mkSomeTags 10))
          content = (Content (Plain (mkRandomInlineList 50)))
          header  =  (Header (getRandomString 5))
          id      = (ID (10 - (mod length 10)))
          author = (Author (getRandomString 5))
          date   = (Date (getRandomString 5))
         in Layout5 rst tags content header id author date


mkBlogs_layout6 :: Int -> Blog
mkBlogs_layout6 length =
   if length < 0 then End
   else 
      let header =  (Header (getRandomString 5))
          id = (ID (10 - (mod length 10)))
          author = (Author (getRandomString 5))
          date   = (Date (getRandomString 5))
          content = (Content (Plain (mkRandomInlineList 50)))
          rst     = (mkBlogs_layout6 (length - 1))
          tags    = (TagList (mkSomeTags 10))
         in Layout6 header id author date content rst tags

mkBlogs_layout7 :: Int -> Blog
mkBlogs_layout7 length =
   if length < 0 then End
   else 
      let rst     = (mkBlogs_layout7 (length - 1))
          content = (Content (Plain (mkRandomInlineList 50)))
          header  = (Header (getRandomString 5))
          id      = (ID (10 - (mod length 10)))
          author  = (Author (getRandomString 5))
          date    = (Date (getRandomString 5))
          tags    = (TagList (mkSomeTags 10))          
         in Layout7 rst content header id author date tags


mkBlogs_layout8 :: Int -> Blog
mkBlogs_layout8 length = 
   if length < 0 then End 
   else 
      let content = (Content (Plain (mkRandomInlineList 50)))
          rst     = (mkBlogs_layout8 (length - 1))
          id      = (ID (10 - (mod length 10)))
          author  = (Author (getRandomString 5))
          date    = (Date (getRandomString 5))
          header  = (Header (getRandomString 5))
          tags    = (TagList (mkSomeTags 10))       
         in Layout8 content rst id author date header tags  


blogLength :: Blog -> Int 
blogLength blog = case blog of 
                        End -> 0 
                        Layout1 _ _ _ _ _ _ rst -> 1 + blogLength rst
                        Layout2 _ _ rst _ _ _ _  -> 1 + blogLength rst
                        Layout3 _ rst _ _ _ _ _ -> 1 + blogLength rst
                        Layout4 _ _ rst _ _ _ _ -> 1 + blogLength rst
                        Layout5 rst _ _ _ _ _ _ -> 1 + blogLength rst
                        Layout6 _ _ _ _ _ rst _ -> 1 + blogLength rst
                        Layout7 rst _ _ _ _ _ _ -> 1 + blogLength rst
                        Layout8 _ rst _ _ _ _ _ -> 1 + blogLength rst

benchFilterBlogsBasedOnKeywordInTagList :: (a -> b -> b) -> a -> b -> Int -> IO (b, Double, Double)
benchFilterBlogsBasedOnKeywordInTagList f keyword blog iters = do
    putStrLn ("Timings for filering blogs based on a keyword in the tag list")
    tups <- mapM (\_ -> dotrialFilterBlogsBasedOnKeywordInTagList f keyword blog) [1..iters]
    let (results, times) = unzip tups
    let selftimed = median times
        batchtime = sum times
    return $! (last results, selftimed, batchtime)

-- Timing for filtering the blogs based on a keyword
dotrialFilterBlogsBasedOnKeywordInTagList :: (a -> b -> b) -> a -> b -> IO (b, Double)
dotrialFilterBlogsBasedOnKeywordInTagList f keyword blogs = do
    t1 <- getCurrentTime
    a <- evaluate $ (f keyword blogs)
    t2 <- getCurrentTime
    let delt = fromRational (toRational (diffUTCTime t2 t1))
    putStrLn ("iter time: " ++ show delt)
    return $! (a,delt)


-- compile with
-- ghc MainGhc.hs -package V.Vector


-- Passes written so far
-- 1.) Search for a keyword in Content of Blogs 
-- 2.) Search for a keyword in TagList of Blogs 
-- 3.) Filter blogs based on a particular keyword
-- 4.) Filter blogs based on a particular keyword in the TagList
-- 5.) emphasize a particular keyword in the Content of Blogs


-- let blogs1 = mkBlogs_layout1 100 0 10
--     blogs2 = mkBlogs_layout2 100 0 10
--     blogs3 = mkBlogs_layout3 100 0 10
--     blogs4 = mkBlogs_layout4 100 0 10
--     keyword = getRandomString 2
--     start <- getCPUTimeWithUnit
--     time1 = timeIt $ print (filterBlogsBasedOnKeywordInTagList keyword blogs1)
--     time2 = timeIt $ print (filterBlogsBasedOnKeywordInTagList keyword blogs2)
--     time3 = timeIt $ print (filterBlogsBasedOnKeywordInTagList keyword blogs3)
--     time4 = timeIt $ print (filterBlogsBasedOnKeywordInTagList keyword blogs4)
--   in time1
