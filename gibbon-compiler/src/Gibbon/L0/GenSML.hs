module Gibbon.L0.GenSML where

import Gibbon.L0.Syntax
import Gibbon.Common

import Text.PrettyPrint hiding ((<>))
import qualified Gibbon.L0.Syntax as L0
import Data.Map hiding (foldr, fold)
import Data.Symbol
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.Graph
import Control.Monad
import Data.Foldable hiding ( toList )

ppExt :: E0Ext Ty0 Ty0 -> Doc
ppExt ex = case ex of
  LambdaE x0 pe ->
    parens $ hsep
      [ hsep $ ("fn " <>) . (<> " =>") . ppVar . fst <$> x0
      , ppPreExp pe
      ]
  PolyAppE pe pe' ->
    hsep $ parens . ppPreExp <$> [pe, pe']
  FunRefE _ty0s _var -> error "FunRefE"
  BenchE _var _ty0s _pes _b -> error "BenchE"
  ParE0 _pes -> error "ParE0"
  PrintPacked _ty0 pe ->
    hsep ["print", parens $ ppPreExp pe]
  CopyPacked _ty0 _pe -> error "CopyPacked"
  TravPacked _ty0 _pe -> error "TravPacked"
  L _loc _pe -> error "L"
  LinearExt _le -> error "LinearExt"

ppPreExp :: PreExp E0Ext Ty0 Ty0 -> Doc
ppPreExp pe = case pe of
  VarE v -> ppVar v
  LitE n -> text $ show n
  CharE c -> char c
  FloatE x -> text $ show x
  LitSymE v -> doubleQuotes $ ppVar v
  AppE var _ pes -> ppApp (ppVar var) pes
  PrimAppE pr pes -> ppPrim pr pes
  LetE (v, _, _, e) pe' ->
    hsep
      [ "\n  let val", ppVar v, "="
      , ppPreExp e, "in"
      , ppPreExp pe', "end"
      ]
  IfE pe' pe2 pe3 ->
    ("\n  " <>) $ parens $ hsep
      [ "if", ppPreExp pe'
      , "then", ppPreExp pe2
      , "\n   else", ppPreExp pe3
      ]
  MkProdE pes ->
    parens $ interleave ", " $ ppPreExp <$> pes
  ProjE 0 pe' -> parens $ hsep
    [ "case", ppPreExp pe', "of"
    , "(x__, _) => x__"
    ]
  ProjE 1 pe' -> parens $ hsep
    [ "case", ppPreExp pe', "of"
    , "(_, x__) => x__"
    ]
  ProjE n pe' ->
    parens $ hsep [hcat ["#", int $ succ n], ppPreExp pe']
  CaseE pe' x0 ->
    parens $ hsep
      [ hsep ["case", ppPreExp pe', "of"]
      , interleave "\n  |" ((\(dc, vs, e) -> hsep
        [ text dc
        , case vs of
          [] -> mempty
          _ -> parens $ interleave comma $ ppVar . fst <$> vs
        , "=>", ppPreExp e
        ]) <$> x0)
      ]
  DataConE _ty0 s [] -> text s
  DataConE _ty0 s pes ->
    parens $ hsep [text s, parens $ interleave comma $ ppPreExp <$> pes]

  TimeIt _pe' _ty0 _b -> _

  WithArenaE _var _pe' -> error "WithArenaE"
  SpawnE _var _ty0s _pes -> error "SpawnE"
  SyncE -> error "SyncE"
  MapE _x0 _pe' -> error "MapE"
  FoldE _x0 _x1 _pe' -> error "FoldE"

  Ext ee -> ppExt ee

ppApp :: Doc -> [PreExp E0Ext Ty0 Ty0] -> Doc
ppApp var pes = parens $ hsep $ var : (ppPreExp <$> pes)

ppAppUncurried :: Doc -> [PreExp E0Ext Ty0 Ty0] -> Doc
ppAppUncurried var pes = 
  parens $ var <> parens (interleave "," $ ppPreExp <$> pes)

ppPrim :: Prim Ty0 -> [PreExp E0Ext Ty0 Ty0] -> Doc
ppPrim pr pes = case pr of
  AddP -> binary "+" pes
  SubP -> binary "-" pes
  MulP -> binary "*" pes
  DivP -> binary "div" pes
  ModP -> binary "mod" pes
  ExpP -> binary "**" pes
  RandP -> ppApp "MltonRandom.rand()" pes
  EqIntP -> binary "=" pes
  LtP -> binary "<" pes
  GtP -> binary ">" pes
  LtEqP -> binary "<=" pes
  GtEqP -> binary ">=" pes
  FAddP -> binary "+" pes
  FSubP -> binary "-" pes
  FMulP -> binary "*" pes
  FDivP -> binary "/" pes
  FExpP ->
    let
      (l, r) = extractBinary "pow" pes
    in
    parens $ hsep
      [ "Math.pow"
      , parens $ hcat [l, comma, r]
      ]
  FRandP -> ppApp "Random.randFloat" pes
  EqFloatP -> binary "=" pes
  EqCharP -> binary "=" pes
  FLtP -> binary "<" pes
  FGtP -> binary ">" pes
  FLtEqP -> binary "<=" pes
  FGtEqP -> binary ">=" pes
  FSqrtP -> ppApp "Math.sqrt" pes
  IntToFloatP -> ppApp "Real.fromInt" pes
  FloatToIntP -> ppApp "Int.fromReal" pes
  FTanP -> ppApp "Math.tan" pes
  EqSymP -> binary "=" pes
  EqBenchProgP _ -> error "GenSML: EqBenchProgP"
  OrP -> binary "orelse" pes
  AndP -> binary "andalso" pes
  MkTrue -> "true"
  MkFalse -> "false"
  ErrorP s _ -> ppFail s
  SizeParam -> int 1  -- ?
  IsBig -> error "IsBig"
  GetNumProcessors -> error "GetNumProcessors"
  PrintInt -> "print(Int.toString(" <> ppPreExp (head pes) <> "))"
  PrintChar -> ppApp "print" pes
  PrintFloat -> ppApp "print" pes
  PrintBool ->
    ppApp "(fn true => \"True\" | false => \"False\")" pes
  PrintSym -> ppApp "print" pes
  ReadInt -> error "ReadInt"  -- Have every program read from stdin?
  DictInsertP _ -> error "DictInsertP"
  DictLookupP _ -> error "DictLookupP"
  DictEmptyP _ -> error "DictEmptyP"
  DictHasKeyP _ -> error "DictHasKeyP"
  SymSetEmpty -> error "SymSetEmpty"
  SymSetInsert -> error "SymSetInsert"
  SymSetContains -> error "SymSetContains"
  SymHashEmpty -> error "SymHashEmpty"
  SymHashInsert -> error "SymHashInsert"
  SymHashLookup -> error "SymHashLookup"
  SymHashContains -> error "SymHashContains"
  IntHashEmpty -> error "IntHashEmpty"
  IntHashInsert -> error "IntHashInsert"
  IntHashLookup -> error "IntHashLookup"
  PDictAllocP _ty0 _ty0' -> error "PDictAllocP"
  PDictInsertP _ty0 _ty0' -> error "PDictInsertP"
  PDictLookupP _ty0 _ty0' -> error "PDictLookupP"
  PDictHasKeyP _ty0 _ty0' -> error "PDictHasKeyP"
  PDictForkP _ty0 _ty0' -> error "PDictForkP"
  PDictJoinP _ty0 _ty0' -> error "PDictJoinP"
  LLAllocP _ty0 -> error "LLAllocP"
  LLIsEmptyP _ty0 -> error "LLIsEmptyP"  -- Implement these? 
  LLConsP _ty0 -> error "LLConsP"
  LLHeadP _ty0 -> error "LLHeadP"
  LLTailP _ty0 -> error "LLTailP"
  LLFreeP _ty0 -> error "LLFreeP"
  LLFree2P _ty0 -> error "LLFree2P"
  LLCopyP _ty0 -> error "LLCopyP"
  VAllocP _ty0 -> 
    ppApp "(fn internal__ => ArraySlice.full(Array.array(internal__, 0)))" pes
  VFreeP _ty0 -> error "VFreeP"
  VFree2P _ty0 -> error "VFree2P"
  VLengthP _ty0 -> ppApp "ArraySlice.length" pes
  VNthP _ty0 -> ppAppUncurried "ArraySlice.sub" pes
  VSliceP _ty0 -> case pes of
    [pe1, pe2, pe3] -> hcat
      [ "ArraySlice.subslice"
      , parens $ interleave comma
        [ ppPreExp pe3
        , ppPreExp pe1
        , parens $ "SOME" <+> ppPreExp pe2
        ]
      ]
    _ -> _
  InplaceVUpdateP _ty0 -> hsep
      [ "let val _ ="
      , ppAppUncurried "ArraySlice.update" pes
      , "in", ppPreExp $ head pes
      , "end"
      ]
  VConcatP _ty0 -> ppFail "VConcatP"
  VSortP _ty0 -> ppFail "VSortP"
  InplaceVSortP _ty0 -> ppFail "InplaceVSortP"
  VMergeP _ty0 -> ppFail "VMergeP"
  Write3dPpmFile _s -> error "Write3dPpmFile"
  ReadPackedFile _m_s _s _m_var _ty0 -> error "ReadPackedFile"
  WritePackedFile _s _ty0 -> error "WritePackedFile"
  ReadArrayFile _ma _ty0 -> error "ReadArrayFile"
  RequestEndOf -> error "RequestEndOf"
  RequestSizeOf -> error "RequestSizeOf"
  Gensym -> error "Gensym"

ppVar :: Var -> Doc
ppVar = text . getVar

getVar :: Var -> String
getVar (Var s) = case unintern s of
  "val" -> "val_"
  "as" -> "as_"
  "open" -> "open_"
  "rec" -> "rec_"
  "fun" -> "fun_"
  "end" -> "end_"
  z -> z

interleave :: Doc -> [Doc] -> Doc
interleave sepr lst = case lst of
  [] -> mempty
  d : ds -> (d <+>) $ fold $ (sepr <+>) <$> ds

binary :: String -> [PreExp E0Ext Ty0 Ty0] -> Doc
binary opSym pes =
  parens $ hsep [l, text opSym, r]
  where
    (l, r) = extractBinary opSym pes

extractBinary :: String -> [PreExp E0Ext Ty0 Ty0] -> (Doc, Doc)
extractBinary opSym pes = case ppPreExp <$> pes of
  [l, r] -> (l, r)
  es -> error $ fold
    [ "L0 error: (", opSym, ") is provided "
    , show $ length es, " arguments"
    ]

extractUnary :: String -> [PreExp E0Ext Ty0 Ty0] -> Doc
extractUnary opSym pes = case ppPreExp <$> pes of
  [x] -> x
  es -> error $ fold
    [ "L0 error: (", opSym, ") is provided "
    , show $ length es, " arguments"
    ]

ppFail :: String -> Doc
ppFail s = hsep
  [ "raise"
  , parens $ hsep ["Fail", doubleQuotes $ text s]
  ]

ppProgram :: L0.Prog0 -> Doc
ppProgram prog =
  hcat
    [ ppDDefs $ ddefs prog
    , ppFunDefs $ fundefs prog
    , ppMainExpr $ mainExp prog
    , "\n"
    ]

ppFunDefs :: Map Var (FunDef L0.Exp0) -> Doc
ppFunDefs funDefs =
  foldMap (either ppValDef ppFunRec) (separateDefs $ sortDefs $ elems funDefs)

separateDefs :: [FunDef L0.Exp0] -> [Either (FunDef L0.Exp0) [FunDef L0.Exp0]]
separateDefs funDefs = case funDefs of
  [] -> []
  fd : fds -> case funArgs fd of
    [] -> Left fd : separateDefs fds
    _ -> case separateDefs fds of
      [] -> [Right [fd]]
      fds'@(Left _ : _) -> Right [fd] : fds'
      Right fds' : fds'' ->  Right (fd : fds') : fds''

ppValDef :: FunDef L0.Exp0 -> Doc
ppValDef funDef =
  hsep
    [ "val"
    , ppVar $ funName funDef
    , "="
    , ppPreExp $ funBody funDef
    ] <> semi

ppFunRec :: [FunDef L0.Exp0] -> Doc
ppFunRec fdefs =
  reduceFunDefs "fun" (head fdefs) $
    foldr (reduceFunDefs "and") ";\n" (tail fdefs)

reduceFunDefs :: Doc -> FunDef L0.Exp0 -> Doc -> Doc
reduceFunDefs keyword funDef doc =
  "\n" <> case funArgs funDef of
    [] -> hsep
      [ keyword
      , ppVar $ funName funDef
      , "="
      , ppPreExp $ funBody funDef
      ] <> doc
    fargs -> hsep
      [ keyword
      , ppVar name
      , hsep $ ppVar <$> fargs
      , "="
      , case name of
        "print_check" -> parens mempty
        "print_space" -> "print \" \""
        "print_newline" -> "print \"\\n\""
        _ -> ppPreExp $ funBody funDef
      ] <> doc
      where name = funName funDef


addFunBinding :: FunDef ex -> Map String (FunDef ex) -> Map String (FunDef ex)
addFunBinding funDef = Map.insert (getVar $ funName funDef) funDef

allFunEntries :: [FunDef L0.Exp0] -> Map String (FunDef L0.Exp0)
allFunEntries = foldr addFunBinding Map.empty

allFunNames :: [FunDef ex] -> Set.Set String
allFunNames = Set.fromList . fmap (getVar . funName)

ppMainExpr :: Maybe (L0.Exp0, L0.Ty0) -> Doc
ppMainExpr opt = case opt of
  Nothing -> mempty
  Just (exp0, _) -> "val _ = " <> ppPreExp exp0 <> semi

ppDDefs :: DDefs0 -> Doc
ppDDefs ddefs = case Map.elems ddefs of
  [] -> mempty
  h : t -> hsep
    [ "datatype"
    , ppDDef h
    , hcat $ ("\nand" <+>) . ppDDef <$> t
    , ";\n"
    ]

ppDDef :: DDef0 -> Doc
ppDDef ddef = hsep
  [ hsep $ ppTyVar <$> tyArgs ddef
  , ("dat_" <>) $ ppVar $ tyName ddef
  , "="
  , interleave
      "|"
      (ppBody <$> dataCons ddef)
  ]
  where
    ppBody (s, lst) = text s <+> case lst of
      [] -> mempty
      _ -> "of" <+> parens (interleave " *" $ ppTy0 . snd <$> lst)

ppTyVar :: TyVar -> Doc
ppTyVar tyVar = case tyVar of
  BoundTv var -> "'" <> ppVar var
  SkolemTv _s _n -> _
  UserTv var -> "'" <> ppVar var

ppTy0 :: Ty0 -> Doc
ppTy0 ty0 = case ty0 of
  IntTy -> "int"
  CharTy -> "char"
  FloatTy -> "real"
  SymTy0 -> "string"
  BoolTy -> "bool"
  TyVar tv -> ppTyVar tv
  MetaTv _mt -> _
  ProdTy ty0s -> interleave " * " $ ppTy0 <$> ty0s
  SymDictTy _m_var _ty0' -> _
  PDictTy _ty0' _ty02 -> _
  SymSetTy -> _
  SymHashTy -> _
  IntHashTy -> _
  ArrowTy ty0s ty0' -> hsep ((<+> "->") . ppTy0 <$> ty0s) <+> ppTy0 ty0'
  PackedTy "Maybe" [ty0'] -> ppTy0 ty0' <+> "option"
  PackedTy s [] -> " dat_" <> text s
  PackedTy s ty0s -> fold [interleave comma $ ppTy0 <$> ty0s, " dat_", text s]
  VectorTy _ty0' -> _
  ListTy ty0' -> ppTy0 ty0' <+> "list"
  ArenaTy -> _

varsExt :: Set.Set String -> E0Ext Ty0 Ty0 -> Set.Set String
varsExt m ext0 = case ext0 of
  LambdaE _ pe -> varsPreExp m pe
  PolyAppE pe pe' -> Set.union (varsPreExp m pe) (varsPreExp m pe')
  PrintPacked _ pe -> varsPreExp m pe
  _ -> mempty

varsPreExps :: Set.Set String -> [PreExp E0Ext Ty0 Ty0] -> Set.Set String
varsPreExps = foldMap . varsPreExp

varsPreExp :: Set.Set String -> PreExp E0Ext Ty0 Ty0 -> Set.Set String
varsPreExp vs pe0 = case pe0 of
  VarE _ -> mempty
  AppE var _ pes -> vpes pes <>
    if Set.member s vs then Set.singleton s
    else mempty
    where s = getVar var
  PrimAppE _ pes -> vpes pes
  LetE (_, _, _, pe') pe -> vpe pe <> vpe pe'
  IfE pe pe' pe3 -> vpes [pe, pe', pe3]
  MkProdE pes -> vpes pes
  ProjE _ pe -> vpe pe
  CaseE pe x0 -> vpe pe <> foldMap (\(_, _, pe') -> vpe pe') x0
  DataConE _ _ pes -> vpes pes
  TimeIt pe _ _ -> vpe pe
  WithArenaE _ pe -> vpe pe
  SpawnE _ _ pes -> vpes pes
  SyncE -> _
  MapE _ _ -> _
  FoldE {} -> _
  Ext ee -> varsExt vs ee
  _ -> mempty
  where
    vpe = varsPreExp vs
    vpes = varsPreExps vs

getDependencies :: [FunDef L0.Exp0] -> Map String [FunDef L0.Exp0]
getDependencies funDefs =
  foldr reduceDeps Map.empty funDefs
  where
    funMap = allFunEntries funDefs
    funSet = allFunNames funDefs
    toNode = fromMaybe _ . flip Map.lookup funMap
    toDep = fmap toNode . Set.toList . varsPreExp funSet . funBody
    reduceDeps = Map.insert . getVar . funName <*> toDep

definitionSort :: Map String (FunDef L0.Exp0) -> Map String [FunDef L0.Exp0] -> [FunDef L0.Exp0]
definitionSort m1 m2 =
  reverse $ (\(_, n, _) -> n) . back <$> topSort gr
  where
    (gr, back, _) = graphFromEdges $ mkNode <$> toList m2
    mkNode (s, lst) = (s, fromMaybe _ (Map.lookup s m1), lst)

sortDefs :: [FunDef L0.Exp0] -> [FunDef L0.Exp0]
sortDefs defs =
  definitionSort nameMap depMap
  where
    depMap = getDependencies defs
    nameMap = fromList $ join ((,) . getVar . funName) <$> defs
