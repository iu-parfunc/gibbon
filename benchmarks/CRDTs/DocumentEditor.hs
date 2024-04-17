module DocumentEditor where

import IntOrMap as M
import OrderedList as L
import Timekeeping as T

data RichText = Text String
                | Block (IntOrMap String) (OrderedList RichText)

data Path = Pth T.Timestamp Path
            | Leaf T.Timestamp 

data Update = Up Path Transaction
data Transaction = SetAttr Int String
                 | DelAttr Int
                 | Insert T.Timestamp T.Timestamp RichText
                 | Del T.Timestamp

render_opening :: IntOrMap String -> String

render_closing :: IntOrMap String -> String


render :: RichText -> RichText
render x =
    case x of
        Text c -> 
            let _ = printsym (quote c)
            in x
        Block attrs content ->
            let _ = printsym (quote "\n")
                _ = printsym (quote (render_opening attrs))
                _ = printsym (quote "\n")
                _ = printsym (quote (render content))
                _ = printsym (quote "\n")
                _ = printsym (quote (render_closing attrs))
                _ = printsym (quote "\n")
            in x

edit :: Int -> RichText -> Update -> RichText
edit uid doc up = case up of
    Up p tr -> case p of
        Pth t np -> case doc of 
            Text _ -> doc 
            Block attrs els -> case (lookup t els) of
                Just ndoc -> edit ndoc (Up np tr)
                Nothing -> doc
        Leaf t -> case doc of
            Text -> doc
            Block attrs els -> case tr of
                SetAttr key val -> Block (M.add uid key val attrs) els
                DelAttr key -> Block (M.remove uid key attrs) els
                Insert l r val -> Block attrs (L.insert uid l r val els)
                Del t -> Block attrs (L.del uid t els)
