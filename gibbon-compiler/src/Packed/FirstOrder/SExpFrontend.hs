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

import Data.Text as T hiding (head)
import Data.List as L
import Data.Set as S
import Data.Map as M
import Data.Text.IO (readFile)
import System.FilePath
import System.Environment
import Text.Parsec
-- import GHC.Generics (Generic)
import Text.PrettyPrint.GenericPretty
import Packed.FirstOrder.L1.Syntax as S
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

-- | Baseline chatter level for this module:
lvl :: Int
lvl = 5


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

textToVar :: Text -> Var
textToVar = toVar . T.unpack

textToDataCon :: Text -> DataCon
textToDataCon = T.unpack

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
   allCons = S.fromList [ (toVar con)
                        | DDef{dataCons} <- M.elems ddefs
                        , (con,_tys) <- dataCons ]
   go cons ex = 
     case ex of
       Ext () -> Ext ()
       AppE v _ (MkProdE ls)
                  -- FIXME: check the type to determine if this is packed/unpacked:
                  | S.member v cons -> DataConE () (fromVar v) (Just dummyLoc) (L.map (go cons) ls)
       AppE v l e | S.member v cons -> DataConE () (fromVar v) (Just dummyLoc) [go cons e]
                  | otherwise       -> AppE v l (go cons e)
       LetE (v,l,t,rhs) bod ->
         let go' = if S.member v cons
                      then go (S.delete v cons)
                      else go cons
         in LetE (v,l,t,go' rhs) (go' bod)
       ------------boilerplate------------
       VarE v          -> VarE v
       LitSymE v       -> LitSymE v
       LitE _          -> ex
       PrimAppE p ls   -> PrimAppE p $ L.map (go cons) ls
       ProjE i e  -> ProjE i (go cons e)
       CaseE e ls -> CaseE (go cons e) (L.map (\(c,vs,er) -> (c,vs,go cons er)) ls)
       MkProdE ls     -> MkProdE $ L.map (go cons) ls
       DataConE loc k ml ls -> DataConE loc k ml $ L.map (go cons) ls
       TimeIt e t b -> TimeIt (go cons e) t b
       IfE a b c -> IfE (go cons a) (go cons b) (go cons c)

       MapE  (v,t,e) bod -> MapE (v,t, go cons e) (go cons bod)
       FoldE (v1,t1,e1) (v2,t2,e2) b -> FoldE (v1,t1,go cons e1) (v2,t2,go cons e2) (go cons b)

parseSExp :: [Sexp] -> SyM Prog
parseSExp ses =
  do prog@Prog {ddefs} <- go ses [] [] [] Nothing
     return $ mapExprs (tagDataCons ddefs) prog
 where

   -- WARNING: top-level constant definitions are INLINED everywhere.
   inlineConstDefs [] p = p
   inlineConstDefs ((vr,_ty,rhs) : cds) p =
       inlineConstDefs cds $
        mapExprs (subst vr rhs) p

   -- Processes an sexpression while accumulating data, function, and constant defs.
   go xs dds fds cds mn =
    case xs of
     [] -> return $
           inlineConstDefs cds $
           Prog (fromListDD dds) (fromListFD fds) mn

     -- IGNORED!:
     (L (A "provide":_) : rst) -> go rst dds fds cds mn
     (L (A "require":_) : rst) -> go rst dds fds cds mn

     (L (A "data": A tycon : cs) : rst) ->
         go rst (DDef (textToVar tycon) (L.map docasety cs) : dds) fds cds mn
     (L [A "define", funspec, ":", retty, bod] : rst)
        |  RSList (A name : args) <- funspec
        -> do
         let bod' = exp bod
             args' = L.map (\(RSList [id,":",t]) -> (getSym id, typ t))
                               args
         (arg,ty,bod'') <-
               case args' of
                 []   -> (,voidTy,bod') <$> gensym (toVar "void")
                 [(a,t)] -> pure (a,t,bod')
                 _    -> do let (vs,ts) = unzip args'
                            vr <- gensym (toVar (L.concat $ L.intersperse "_" $
                                                 L.map fromVar vs))
                            let ty = ProdTy ts
                                newbod = tuplizeRefs vr vs bod'
                            return (vr,ty,newbod)
         -- Here we directly desugar multiple arguments into a tuple
         -- argument.
         go rst dds (FunDef { funName  = textToVar name
                            , funArg   = (arg, ty)
                            , funRetTy = typ retty
                            , funBody  = bod''
                            } : fds)
            cds mn

     -- Top-level definition instead of a function.
     (L [A "define", A topid, ":", ty, bod] : rst) ->
         go rst dds fds ((textToVar topid,ty,exp bod) : cds) mn

     (L [A "define", _args, _bod] : _) -> error$ "Function is missing return type:\n  "++prnt (head xs)
     (L (A "define" : _) : _) -> error$ "Badly formed function:\n  "++prnt (head xs)

     (L (A "data" : _) : _) -> error$ "Badly formed data definition:\n  "++prnt (head xs)

     (L3 "module+" _ bod : rst) -> go (bod:rst) dds fds cds mn

     (ex : rst) ->
       let ex' = exp ex
       in go rst dds fds cds (case mn of
                            Nothing -> Just ex'
                            Just x  -> error$ "Two main expressions: "++
                                             sdoc x++"\nAnd:\n"++prnt ex)

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
         (A other)  -> Packed (textToDataCon other)
         (RSList (A "Vector"  : rst)) -> ProdTy $ L.map typ rst
         (RSList [A "SymDict", t]) -> SymDictTy $ typ t
         (RSList [A "Listof", t])  -> ListTy $ typ t
         _ -> error$ "SExpression encodes invalid type:\n "++prnt s

getSym :: RichSExpr HaskLikeAtom -> Var
getSym (RSAtom (HSIdent id)) = textToVar id
getSym s = error $ "expected identifier sexpr, got: "++prnt s

docasety :: Sexp -> (DataCon,[(IsBoxed,Ty)])
docasety s =
  case s of
    (RSList ((A id) : tys)) -> (textToDataCon id, L.map ((False,) . typ) tys)
    _ -> error$ "Badly formed variant of datatype:\n "++prnt s

pattern A s = RSAtom (HSIdent s)

pattern L  a         = RSList a
pattern L1 a         = RSList [a]
pattern L2 a b       = RSList [A a, b]
pattern L3 a b c     = RSList [A a, b, c]
pattern L4 a b c d   = RSList [A a, b, c, d]
-- pattern L5 a b c d e = RSList [A a, b, c, d, e]

trueE :: Exp
trueE = PrimAppE MkTrue []

falseE :: Exp
falseE = PrimAppE MkFalse []

-- -- FIXME: we cannot intern strings until runtime.
-- hackySymbol :: String -> Int
-- hackySymbol s = product (L.map ord s)

keywords :: S.Set Text
keywords = S.fromList $ L.map pack $
           [ "quote", "if", "or", "and", "time", "let"
           , "case", "vector-ref", "for/fold", "for/list"
           , "insert", "empty-dict", "lookup", "error", "ann"
           ]

isKeyword :: Text -> Bool
isKeyword s = s `S.member` keywords

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

   L2 "quote" (A v) -> LitSymE (textToVar v)

   -- Any other naked symbol is a variable:
   A v               -> VarE (textToVar v)
   RSAtom (HSInt n)  -> LitE (fromIntegral n)

   -- | This type gets replaced later in flatten:
   L2 "time" arg -> (TimeIt (exp arg) (PackedTy "DUMMY_TY" ()) False)

   -- | This variant inserts a loop, controlled by the iters argument on the command line.
   L2 "iterate" arg -> (TimeIt (exp arg) (PackedTy "DUMMY_TY" ()) True)

   L3 "let" (L bnds) bod ->
     mkLets (L.map letbind bnds) (exp bod)

   L (A "case": scrut: cases) ->
     CaseE (exp scrut) (L.map docase cases)

   L (A p : ls) | isPrim p -> PrimAppE (prim p) $ L.map exp ls

   L3 "for/list" (L1 (L4 v ":" t e)) bod ->
     S.MapE (textToVar v, typ t, exp e) (exp bod)

   -- I don't see why we need the extra type annotation:
   L4 "for/fold"
          (L1 (L4 v1 ":" t1 e1))
          (L1 (L4 v2 ":" t2 e2))
          bod ->
     S.FoldE (textToVar v1, typ t1, exp e1)
             (textToVar v2, typ t2, exp e2)
             (exp bod)

   L3 "vector-ref" evec (RSAtom (HSInt ind)) -> S.ProjE (fromIntegral ind) (exp evec)
   L (A "vector" : es) -> S.MkProdE $ L.map exp es

   -- Dictionaries require type annotations for now.  No inference!
   L3 "ann" (L1 "empty-dict") (L2 "SymDict" ty) ->
       PrimAppE (DictEmptyP $ typ ty) []

   L4 "insert" d k (L3 "ann" v ty) ->
       PrimAppE (DictInsertP $ typ ty) [(exp d),(exp k),(exp v)]

   L3 "ann" (L3 "lookup" d k) ty ->
       PrimAppE (DictLookupP $ typ ty) [(exp d),(exp k)]

   L (A "gensym" : _) -> PrimAppE Gensym []

   L3 "ann" (L3 "has-key?" d k) ty ->
     PrimAppE (DictHasKeyP $ typ ty) [(exp d),(exp k)]

   -- L [A "error",arg] ->
   L3 "ann" (L2 "error" arg) ty ->
      case arg of
        RSAtom (HSString str) -> PrimAppE (ErrorP (T.unpack str) (typ ty)) []
        _ -> error$ "bad argument to 'error' primitive: "++prnt arg

   -- Other annotations are dropped:
   L3 "ann" e _ty -> exp e

   L (A kwd : _args) | isKeyword kwd ->
      error $ "Error reading treelang.  Badly formed expression:\n "++prnt se

   ----------------------------------------
   -- If NOTHING else matches, we are an application.  Be careful we didn't miss anything:
   L (A rator : rands) ->
     let app = AppE (textToVar rator) []
     in case rands of
         [] -> app (MkProdE [])
         [rand] -> app (exp rand)
         _ -> app (MkProdE (L.map exp rands))

   _ -> error $ "Expression form not handled (yet):\n  "++
               sdoc se ++ "\nMore concisely:\n  "++ prnt se


-- | One case of a case expression
docase :: Sexp -> (DataCon,[(Var,LocVar)],Exp)
docase s =
  case s of
    RSList [ RSList (A con : args)
           , rhs ]
      -> (textToDataCon con, L.map f args, exp rhs)
    _ -> error$ "bad clause in case expression\n  "++prnt s
 where
   f x  = (getSym x, dummyLoc)

letbind :: Sexp -> (Var,[LocVar],Ty,Exp)
letbind s =
  case s of
   RSList [A vr, A ":",
           ty, rhs]
     -> (textToVar vr, [], typ ty, exp rhs)
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
  , ("size-param", SizeParam)
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

handleRequire :: FilePath -> [Sexp] -> IO [Sexp]
handleRequire _ [] = return []
handleRequire baseFile ((L2 "require" arg):ls) = do
  ls' <- handleRequire baseFile ls
  let file = case arg of
               RSAtom (HSString str) -> (takeDirectory baseFile) </> (unpack str)
               _ -> error $ "bad require line: " ++ (show arg)
  dbgPrintLn lvl $ "Including required file: "++show file
  txt <- fmap bracketHacks $ readFile file
  dbgPrintLn lvl $ "Parsing required text: "++show txt
  let res :: Either String [RichSExpr HaskLikeAtom]
      res = fmap (fmap toRich) $
            decode treelangParser txt
  case res of
    Left err -> error err
    Right l' -> return $ l' ++ ls'
handleRequire baseFile (l:ls) = do
  ls' <- handleRequire baseFile ls
  return $ l:ls'

parseFile :: FilePath -> IO (Prog, Int)
parseFile file = do
  txt    <- fmap bracketHacks $
            -- fmap stripHashLang $
            readFile file
  dbgPrintLn lvl $ "Parsing text: "++show txt
  let res :: Either String [RichSExpr HaskLikeAtom]
      res = fmap (fmap toRich) $
            decode treelangParser txt
  dbgPrintLn lvl "Result of parsing:"
  case res of
     Left err -> error err
     Right ls -> do ls' <- handleRequire file ls
                    return $ runSyM 0 $ parseSExp ls'
