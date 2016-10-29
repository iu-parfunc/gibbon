{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

-- |  Parse an SExp representaton of our tree-walk language.

module Packed.FirstOrder.SExpFrontend where

import Data.Text as T
import Data.List as L
import Data.Text.IO (readFile)
import System.Environment
import Text.Parsec 
import GHC.Generics
import Text.PrettyPrint.GenericPretty
import Packed.FirstOrder.L1_Source as L1
import Packed.FirstOrder.Common 
import Prelude hiding (readFile, exp)
import Debug.Trace

-- There are several options for s-expression parsing, including these
-- packages on Hackage:
--  * sexp
--  * sexpr
--  * sexp-grammar
--  * atto-lisp 
--  * lispparser
-- Using 's-cargo' for the first attempt:    
import Data.SCargot.Language.HaskLike
import Data.SCargot.Parse
import Data.SCargot.Print
import Data.SCargot.Repr -- (SExpr, RichSExpr, toRich)

--------------------------------------------------------------------------------

deriving instance Generic a => Generic (SExpr a)
deriving instance Generic a => Generic (RichSExpr a)
deriving instance Generic HaskLikeAtom
instance (Generic a, Out a) => Out (SExpr a)
instance (Generic a, Out a) => Out (RichSExpr a)
instance Out HaskLikeAtom
instance Out Text where
  doc t = doc (T.unpack t)
  docPrec n t = docPrec n (T.unpack t)

type Sexp = RichSExpr HaskLikeAtom

prnt :: Sexp -> String
prnt = T.unpack . encodeOne haskLikePrinter . fromRich

toVar :: Text -> Var
toVar = T.unpack
    
-- Ideally, we'd extend the parser to ignore #lang lines.
-- But for now we'll just do that in a preprocessing hack.
treelangParser :: SExprParser HaskLikeAtom (SExpr HaskLikeAtom)
treelangParser =
    let langline = string "#lang " *> eatline
        comment  = string ";"      *> eatline
        eatline = manyTill anyChar newline *> pure ()
    in
    -- setCarrier (return . asRich) $
    setComment (comment <|> langline) $ 
    haskLikeParser

-- Hack:
stripHashLang :: Text -> Text
stripHashLang txt =
  if T.isPrefixOf "#lang" txt
  then snd $ T.break (== '\n') txt 
       -- (\c -> generalCategory c == LineSeparator)
  else txt

bracketHacks :: Text -> Text
bracketHacks = T.map $ \case '[' -> '('
                             ']' -> ')'
                             x   -> x
                                    
parseTreeLang :: [Sexp] -> SyM L1.Prog
parseTreeLang ses = go ses [] [] Nothing
 where
   go xs dds fds mn = 
    case xs of
     [] -> return $ Prog (fromListDD dds) (fromListFD fds) mn
     (RSList (RSAtom (HSIdent "data"): RSAtom (HSIdent tycon) : cs) : rst) ->
         go rst (DDef (toVar tycon) (L.map docase cs) : dds) fds mn
     (RSList [RSAtom (HSIdent "define"), funspec, ":", retty, bod] : rst)
        | RSList (RSAtom (HSIdent name) : args) <- funspec
        -> do
         let bod' = exp bod
             args' = L.map (\(RSList [id,":",t]) -> (getSym id, typ t))
                               args
         (arg,ty,bod'') <-
               case args' of
                 []   -> (,voidTy,bod') <$> gensym "vd" 
                 [(a,t)] -> pure (a,t,bod')
                 _    -> do vr <- gensym "tmp"
                            let (vs,ts) = unzip args'
                                ty = ProdTy ts
                                newbod = tuplizeRefs vr vs bod'
                            return (vr,ty,newbod)
         -- Here we directly desugar multiple arguments into a tuple
         -- argument.
         go rst dds (FunDef { funName  = toVar name
                            , funArg   = (arg, ty)
                            , funRetTy = typ retty
                            , funBody  = bod''
                            } : fds)
            mn
     (ex : rst) -> 
       let ex' = exp ex
       in go rst dds fds (case mn of
                            Nothing -> Just ex'
                            Just x  -> error$ "Two main expressions: "++
                                             show x++"\nAnd:\n"++show ex)

tuplizeRefs :: Var -> [Var] -> Exp -> Exp
tuplizeRefs tmp ls  = go (L.zip [0..] ls) 
  where 
   go []          e = e
   go ((ix,v):vs) e = go vs (subst v (ProjE ix (VarE tmp)) e)

typ :: RichSExpr HaskLikeAtom -> Ty
typ s = case s of          
         (RSAtom (HSIdent "Int"))  -> IntTy
         (RSAtom (HSIdent "Sym"))  -> SymTy
         -- (RSAtom (HSIdent "Bool")) -> BoolTy
         (RSAtom (HSIdent other))  -> Packed (toVar other)
         (RSList (RSAtom (HSIdent "Vector")  : rst)) -> ProdTy $ L.map typ rst
         (RSList [RSAtom (HSIdent "SymDict"), t]) -> SymDictTy $ typ t
         _ -> error$ "SExpression encodes invalid type:\n "++prnt s

getSym :: RichSExpr HaskLikeAtom -> Var
getSym (RSAtom (HSIdent id)) = toVar id
getSym s = error $ "expected identifier sexpr, got: "++prnt s

docase :: Sexp -> (Constr,[Ty])
docase s = 
  case s of
    (RSList ((RSAtom (HSIdent id)) : tys)) -> (toVar id, L.map typ tys)
    _ -> error$ "Badly formed variant of datatype:\n "++prnt s

exp :: Sexp -> L1.Exp
exp se =
 case se of
   RSList ["+",e1,e2] -> PrimAppE AddP [exp e1, exp e2]
   _ -> error $ "Expression form not handled (yet):\n  "++ 
               sdoc se ++ "\n"++ prnt se
--   RSList




main :: IO ()
main = do
  [file] <- getArgs
  txt    <- fmap bracketHacks $
            -- fmap stripHashLang $
            readFile file
  putStrLn $ "Parsing text: "++show txt
  let res :: Either String [RichSExpr HaskLikeAtom]
      res = fmap (fmap toRich) $
            decode treelangParser txt
  putStrLn "Result:"
  case res of
     Left err -> error err
     Right ls -> do mapM_ (\x -> putStrLn$"DECODED: "++show x) ls
                    putStrLn "Converted:"
                    putStrLn $ sdoc $ fst $ runSyM 0 $ parseTreeLang ls
  return ()


