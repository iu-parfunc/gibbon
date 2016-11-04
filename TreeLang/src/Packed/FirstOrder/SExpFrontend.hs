{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

-- |  Parse an SExp representaton of our tree-walk language.

module Packed.FirstOrder.SExpFrontend 
       (parseFile, parseSExp, primMap, main) where

import Data.Char
import Data.Text as T
import Data.List as L
import Data.Set as S
import Data.Map as M
import Data.Text.IO (readFile)
import System.Environment
import Text.Parsec 
-- import GHC.Generics (Generic)
import Text.PrettyPrint.GenericPretty
import Packed.FirstOrder.L1_Source as S
import Packed.FirstOrder.Common 
import Prelude hiding (readFile, exp)

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
_stripHashLang :: Text -> Text
_stripHashLang txt =
  if T.isPrefixOf "#lang" txt
  then snd $ T.break (== '\n') txt 
       -- (\c -> generalCategory c == LineSeparator)
  else txt

bracketHacks :: Text -> Text
bracketHacks = T.map $ \case '[' -> '('
                             ']' -> ')'
                             x   -> x

-- | Change regular applications into data constructor syntax.             
tagDataCons :: DDefs Ty -> Exp -> Exp
tagDataCons ddefs = go allCons
  where 
   allCons = S.fromList [ con
                        | DDef{dataCons} <- M.elems ddefs
                        , (con,_tys) <- dataCons ]
   go cons ex = 
     case ex of
       AppE v (MkProdE ls) | S.member v cons -> MkPackedE v (L.map (go cons) ls)
       AppE v e | S.member v cons -> MkPackedE v [go cons e]
                | otherwise       -> AppE      v (go cons e)
       LetE (v,t,rhs) bod -> 
         let go' = if S.member v cons
                      then go (S.delete v cons)
                      else go cons
         in LetE (v,t,go' rhs) (go' bod)
       ------------boilerplate------------
       VarE v          -> VarE v
       LitE _          -> ex
       PrimAppE p ls   -> PrimAppE p $ L.map (go cons) ls
       ProjE i e  -> ProjE i (go cons e)
       CaseE e ls -> CaseE (go cons e) (M.map (\(vs,er) -> (vs,go cons er)) ls)
       MkProdE ls     -> MkProdE $ L.map (go cons) ls
       MkPackedE k ls -> MkPackedE k $ L.map (go cons) ls
       TimeIt e  -> TimeIt $ go cons e
       IfE a b c -> IfE (go cons a) (go cons b) (go cons c)   

       MapE  (v,t,e) bod -> MapE (v,t, go cons e) (go cons bod)
       FoldE (v1,t1,e1) (v2,t2,e2) b -> FoldE (v1,t1,go cons e1) (v2,t2,go cons e2) (go cons b)
                    
parseSExp :: [Sexp] -> SyM Prog
parseSExp ses = 
  do prog@Prog {ddefs} <- go ses [] [] Nothing
     return $ mapExprs (tagDataCons ddefs) prog
 where   
   go xs dds fds mn = 
    case xs of
     [] -> return $ Prog (fromListDD dds) (fromListFD fds) mn

     -- IGNORED!:
     (RSList (A "provide":_) : rst) -> go rst dds fds mn
     (RSList (A "require":_) : rst) -> go rst dds fds mn

     (RSList (A "data": A tycon : cs) : rst) ->
         go rst (DDef (toVar tycon) (L.map docasety cs) : dds) fds mn
     (RSList [A "define", funspec, ":", retty, bod] : rst)
        |  RSList (A name : args) <- funspec
        -> do
         let bod' = exp bod
             args' = L.map (\(RSList [id,":",t]) -> (getSym id, typ t))
                               args
         (arg,ty,bod'') <-
               case args' of
                 []   -> (,voidTy,bod') <$> gensym "vd" 
                 [(a,t)] -> pure (a,t,bod')
                 _    -> do let (vs,ts) = unzip args'
                            vr <- gensym (L.concat$ L.intersperse "_" vs)
                            let ty = ProdTy ts
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
         (A "Int")  -> IntTy
         (A "Sym")  -> SymTy
         (A "Bool") -> BoolTy
         (A other)  -> Packed (toVar other)
         (RSList (A "Vector"  : rst)) -> ProdTy $ L.map typ rst
         (RSList [A "SymDict", t]) -> SymDictTy $ typ t
         (RSList [A "Listof", t])  -> ListTy $ typ t
         _ -> error$ "SExpression encodes invalid type:\n "++prnt s

getSym :: RichSExpr HaskLikeAtom -> Var
getSym (RSAtom (HSIdent id)) = toVar id
getSym s = error $ "expected identifier sexpr, got: "++prnt s

docasety :: Sexp -> (Constr,[Ty])
docasety s = 
  case s of
    (RSList ((A id) : tys)) -> (toVar id, L.map typ tys)
    _ -> error$ "Badly formed variant of datatype:\n "++prnt s

pattern A s = RSAtom (HSIdent s)

pattern L  a       = RSList a 
pattern L1 a       = RSList [a]   
pattern L2 a b     = RSList [A a, b]
pattern L3 a b c   = RSList [A a, b, c]
pattern L4 a b c d = RSList [A a, b, c, d]

trueE :: Exp
trueE = PrimAppE MkTrue []

falseE :: Exp
falseE = PrimAppE MkFalse []

-- FIXME: we cannot intern strings until runtime.
hackySymbol :: String -> Int
hackySymbol s = product (L.map ord s)  
         
exp :: Sexp -> Exp
exp se =
 -- trace ("\n ==> Processing Exp:\n  "++prnt se)  $ 
 case se of
   A "True"          -> trueE
   A "False"         -> falseE
   L ("and" : args)  -> go args
     where go [] = trueE
           go (x:xs) = IfE (exp x) (go xs) falseE       
   L ("or" : args)  -> go args
     where go [] = falseE
           go (x:xs) = IfE (exp x) trueE (go xs)

   L4 "if" test conseq altern -> 
     IfE (exp test) (exp conseq) (exp altern)

   -- FIXME: Need LitSym:
   L2 "quote" (A v) -> LitE $ hackySymbol (T.unpack v)
     
   -- Any other naked symbol is a variable:
   A v -> VarE (toVar v)
   RSAtom (HSInt n)  -> LitE (fromIntegral n)
                        
   -- L [A "error",arg] ->
   L3 "ann" (L2 "error" arg) ty -> 
      case arg of
        RSAtom (HSString str) -> PrimAppE (ErrorP (T.unpack str) (typ ty)) []
        _ -> error$ "bad argument to 'error' primitive: "++prnt arg

   -- Other annotations are dropped:
   L3 "ann" e _ty -> exp e
             
   L2 "time" arg -> (TimeIt (exp arg))
   
   L3 "let" (L bnds) bod -> 
     mkLets (L.map letbind bnds) (exp bod) 
     
   L (A "case": scrut: cases) -> 
     CaseE (exp scrut) (M.fromList $ L.map docase cases)

   L (A p : ls) | isPrim p -> PrimAppE (prim p) $ L.map exp ls

   L3 "for/list" (L1 (L4 v ":" t e)) bod ->     
     S.MapE (T.unpack v, typ t, exp e) (exp bod)

   -- I don't see why we need the extra type annotation:
   L4 "for/fold"
          (L1 (L4 v1 ":" t1 e1))
          (L1 (L4 v2 ":" t2 e2))
          bod -> 
     S.FoldE (T.unpack v1, typ t1, exp e1)
             (T.unpack v2, typ t2, exp e2)
             (exp bod)

   L3 "vector-ref" evec (RSAtom (HSInt ind)) -> S.ProjE (fromIntegral ind) (exp evec)
   L (A "vector" : es) -> S.MkProdE $ L.map exp es
                                                
   -- If NOTHING else matches, we are an application.  Be careful we didn't miss anything:             
   L (A rator : rands) -> 
     let app = AppE (toVar rator)
     in case rands of   
         [] -> app (MkProdE [])
         [rand] -> app (exp rand)
         _ -> app (MkProdE (L.map exp rands))

   _ -> error $ "Expression form not handled (yet):\n  "++ 
               sdoc se ++ "\nMore concisely:\n  "++ prnt se


-- | One case of a case expression
docase :: Sexp -> (Constr, ([Var], Exp))
docase s = 
  case s of
    RSList [ RSList (A con : args)
           , rhs ]
      -> (toVar con, (L.map getSym args, exp rhs))
    _ -> error$ "bad clause in case expression\n  "++prnt s

mkLets :: [(Var, Ty, Exp)] -> Exp -> Exp
mkLets [] bod = bod
mkLets (a:b) bod = LetE a (mkLets b bod)

letbind :: Sexp -> (Var,Ty,Exp)
letbind s = 
  case s of
   RSList [A vr, A ":",
           ty, rhs]
     -> (toVar vr, typ ty, exp rhs)
   _ -> error $ "Badly formed let binding:\n  "++prnt s

isPrim :: Text -> Bool
isPrim p = S.member p (M.keysSet primMap)

primMap :: Map Text Prim
primMap = M.fromList
  [ ("+", AddP)
  , ("-", SubP)
  , ("*", MulP)
  , ("eq?", EqSymP)
  , ("=",   EqIntP)
  ]

prim :: Text -> Prim
prim t = case M.lookup t primMap of
           Just x -> x
           Nothing -> error$ "Internal error, this is not a primitive: "++show t

main :: IO ()
main = do
  [file] <- getArgs
  _ <- parseFile file
  return ()



parseFile :: FilePath -> IO (Prog, Int)
parseFile file = do
  txt    <- fmap bracketHacks $
            -- fmap stripHashLang $
            readFile file
  dbgPrintLn 5 $ "Parsing text: "++show txt
  let res :: Either String [RichSExpr HaskLikeAtom]
      res = fmap (fmap toRich) $
            decode treelangParser txt
  dbgPrintLn 5 "Result of parsing:"
  case res of
     Left err -> error err
     Right ls -> return $ runSyM 0 $ parseSExp ls
