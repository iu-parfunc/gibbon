module Gibbon.L0.GenSML where

import Gibbon.L0.Syntax
import Gibbon.Common

import Text.PrettyPrint hiding ((<>))
import qualified Gibbon.L0.Syntax as L0
import Data.Map hiding (foldr)
import Data.Symbol
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.Graph
import Data.Tuple

ppExt :: E0Ext Ty0 Ty0 -> Doc
ppExt ex = case ex of
  LambdaE x0 pe ->
    parens $ hsep
      [ hsep $ ppVar . fst <$> x0
      , text "=>"
      , ppPreExp pe
      ]
  PolyAppE pe pe' ->
    hsep $ parens . ppPreExp <$> [pe, pe']
  FunRefE _ty0s _var -> error "FunRefE"
  BenchE _var _ty0s _pes _b -> error "BenchE"
  ParE0 _pes -> error "ParE0"
  PrintPacked _ty0 pe ->
    hsep [text "print", parens $ ppPreExp pe]
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
  LitSymE v -> ppVar v
  AppE var _ pes -> ppApp (ppVar var) pes
  PrimAppE pr pes -> ppPrim pr pes
  LetE (v, _, _, e) pe' ->
    hsep
      [ text "let val", ppVar v, "="
      , ppPreExp e, text "in"
      , ppPreExp pe', text "end"
      ]
  IfE pe' pe2 pe3 ->
    parens $ hsep
      [ "if", ppPreExp pe'
      , "then", ppPreExp pe2
      , "else", ppPreExp pe3
      ]
  MkProdE pes ->
    parens $ interleave (text ", ") $ ppPreExp <$> pes
  ProjE n pe' ->
    parens $ hsep [hcat [text "#", int n], ppPreExp pe']
  CaseE pe' x0 ->
    parens $ hsep
      [ hsep [text "case", ppPreExp pe', text "of"]
      , interleave (text "|") ((\(dc, vs, e) -> hsep
        [ text dc
        , case vs of
          [] -> mempty
          _ -> parens $ interleave comma $ ppVar . fst <$> vs
        , "=>", ppPreExp e
        ]) <$> x0)
      ]
  DataConE _ty0 "Nothing" [] -> text "NONE"
  DataConE _ty0 "Just" [t] -> text "SOME" <> parens (ppPreExp t)
  DataConE _ty0 s [] -> text s
  DataConE _ty0 s pes ->
    hsep [text s, parens $ interleave comma (ppPreExp <$> pes)]

  TimeIt _pe' _ty0 _b -> _

  WithArenaE _var _pe' -> error "WithArenaE"
  SpawnE _var _ty0s _pes -> error "SpawnE"
  SyncE -> error "SyncE"
  MapE _x0 _pe' -> error "MapE"
  FoldE _x0 _x1 _pe' -> error "FoldE"

  Ext ee -> ppExt ee

ppApp :: Doc -> [PreExp E0Ext Ty0 Ty0] -> Doc
ppApp var pes = hsep $ var : (ppPreExp <$> pes)

ppPrim :: Prim Ty0 -> [PreExp E0Ext Ty0 Ty0] -> Doc
ppPrim pr pes = case pr of
  AddP -> binary "+" pes
  SubP -> binary "-" pes
  MulP -> binary "*" pes
  DivP -> binary "div" pes
  ModP -> binary "mod" pes
  ExpP -> binary "**" pes
  RandP -> ppApp (text "Random.randInt") pes
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
  FRandP -> ppApp (text "Random.randFloat") pes
  EqFloatP -> binary "=" pes
  EqCharP -> binary "=" pes
  FLtP -> binary "<" pes
  FGtP -> binary ">" pes
  FLtEqP -> binary "<=" pes
  FGtEqP -> binary ">=" pes
  FSqrtP -> ppApp (text "Math.sqrt") pes
  IntToFloatP -> ppApp (text "Real.fromInt") pes
  FloatToIntP -> ppApp (text "Int.fromReal") pes
  FTanP -> ppApp (text "Math.tan") pes
  EqSymP -> binary "=" pes
  EqBenchProgP _ -> error "GenSML: EqBenchProgP"
  OrP -> binary "orelse" pes
  AndP -> binary "andalso" pes
  MkTrue -> text "true"
  MkFalse -> text "false"
  ErrorP s _ -> hsep [text "raise", doubleQuotes $ text s]
  SizeParam -> error "SizeParam"
  IsBig -> error "IsBig"
  GetNumProcessors -> error "GetNumProcessors"
  PrintInt -> ppApp (text "print") pes
  PrintChar -> ppApp (text "print") pes
  PrintFloat -> ppApp (text "print") pes
  PrintBool ->
    ppApp (text "(fn true => \"True\" | false => \"False\")") pes
  PrintSym -> ppApp (text "print") pes
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
  VAllocP _ty0 -> error "VAllocP"
  VFreeP _ty0 -> error "VFreeP"
  VFree2P _ty0 -> error "VFree2P"
  VLengthP _ty0 -> error "VLengthP"
  VNthP _ty0 -> error "VNthP"
  VSliceP _ty0 -> error "VSliceP"
  InplaceVUpdateP _ty0 -> error "InplaceVUpdateP"
  VConcatP _ty0 -> error "VConcatP"
  VSortP _ty0 -> error "VSortP"
  InplaceVSortP _ty0 -> error "InplaceVSortP"
  VMergeP _ty0 -> error "VMergeP"
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
  z -> z

interleave :: Doc -> [Doc] -> Doc
interleave sepr lst = case lst of
  [] -> mempty
  d : ds -> d <+> foldr (\x -> (sepr <+> x <>)) mempty ds

binary :: String -> [PreExp E0Ext Ty0 Ty0] -> Doc
binary opSym pes =
  parens $ hsep [l, text opSym, r]
  where
    (l, r) = extractBinary opSym pes

extractBinary :: String -> [PreExp E0Ext Ty0 Ty0] -> (Doc, Doc)
extractBinary opSym pes = case ppPreExp <$> pes of
  [l, r] -> (l, r)
  es -> error $ mconcat
    [ "L0 error: (", opSym, ") is provided "
    , show $ length es, " arguments"
    ]

extractUnary :: String -> [PreExp E0Ext Ty0 Ty0] -> Doc
extractUnary opSym pes = case ppPreExp <$> pes of
  [x] -> x
  es -> error $ mconcat
    [ "L0 error: (", opSym, ") is provided "
    , show $ length es, " arguments"
    ]

ppProgram :: L0.Prog0 -> Doc
ppProgram prog =
  hcat
    [ ppDDefs $ ddefs prog
    , ppFunDefs $ fundefs prog
    , ppMainExpr $ mainExp prog
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
    [ text "val"
    , ppVar $ funName funDef
    , text "="
    , ppPreExp $ funBody funDef
    ] <> semi

ppFunRec :: [FunDef L0.Exp0] -> Doc
ppFunRec fdefs =
  reduceFunDefs "fun" (head fdefs) $
    foldr (reduceFunDefs "and") (text ";\n") (tail fdefs)

reduceFunDefs :: String -> FunDef L0.Exp0 -> Doc -> Doc
reduceFunDefs keyword funDef doc =
  text "\n" <> case funArgs funDef of
    [] -> hsep
      [ text keyword
      , ppVar $ funName funDef
      , text "="
      , ppPreExp $ funBody funDef
      ] <> doc
    fargs -> hsep
      [ text keyword
      , ppVar $ funName funDef
      , hsep $ ppVar <$> fargs
      , text "="
      , ppPreExp $ funBody funDef
      ] <> doc


addFunBinding :: FunDef ex -> Map String (FunDef ex) -> Map String (FunDef ex)
addFunBinding funDef = Map.insert (getVar $ funName funDef) funDef

allFunEntries :: [FunDef L0.Exp0] -> Map String (FunDef L0.Exp0)
allFunEntries = foldr addFunBinding Map.empty

allFunNames :: [FunDef ex] -> Set.Set String
allFunNames = Set.fromList . fmap (getVar . funName)

ppMainExpr :: Maybe (L0.Exp0, L0.Ty0) -> Doc
ppMainExpr opt = case opt of
  Nothing -> mempty
  Just (exp0, _) -> text "val () = " <> ppPreExp exp0 <> semi

ppDDefs :: DDefs0 -> Doc
ppDDefs ddefs = case Map.elems ddefs of
  [] -> mempty
  h : t -> hsep
    [ "datatype"
    , ppDDef h
    , hsep $ ("and" <+>) . ppDDef <$> t
    , semi
    ]

ppDDef :: DDef0 -> Doc
ppDDef ddef = hsep
  [ hsep $ ppTyVar <$> tyArgs ddef
  , (text "dat_" <>) $ ppVar $ tyName ddef
  , text "="
  , interleave
      (text " | ")
      ((\(s, lst) -> text s <+> case lst of
        [] -> mempty
        _ -> text "of" <+> parens (interleave (text " * ") (ppTy0 . snd <$> lst))) <$> dataCons ddef)
  ]

ppTyVar :: TyVar -> Doc
ppTyVar tyVar = case tyVar of
  BoundTv var -> text "'" <> ppVar var
  SkolemTv _s _n -> _
  UserTv var -> text "'" <> ppVar var

ppTy0 :: Ty0 -> Doc
ppTy0 ty0 = case ty0 of
  IntTy -> text "int"
  CharTy -> text "char"
  FloatTy -> text "real"
  SymTy0 -> text "string"
  BoolTy -> text "bool"
  TyVar tv -> ppTyVar tv
  MetaTv _mt -> _
  ProdTy ty0s -> interleave (text " * ") (ppTy0 <$> ty0s)
  SymDictTy _m_var _ty0' -> _
  PDictTy _ty0' _ty02 -> _
  SymSetTy -> _
  SymHashTy -> _
  IntHashTy -> _
  ArrowTy ty0s ty0' -> hsep ((\x -> ppTy0 x <+> text "->") <$> ty0s) <+> ppTy0 ty0'
  PackedTy "Maybe" [ty0'] -> ppTy0 ty0' <+> text "option"
  PackedTy s [] -> text " dat_" <> text s
  PackedTy s ty0s -> interleave comma (ppTy0 <$> ty0s) <> text " dat_" <> text s
  VectorTy _ty0' -> _
  ListTy ty0' -> ppTy0 ty0' <+> text "list"
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
  VarE var ->
    if Set.member s vs then Set.insert s vs
    else Set.empty
    where s = getVar var
  AppE _ _ pes -> vpes pes
  PrimAppE _ pes -> vpes pes
  LetE _ pe -> vpe pe
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
  foldr (\s ->
    Map.insert
      (getVar $ funName s)
      (fmap toNode $ Set.toList $ varsPreExp funSet $ funBody s)
  ) Map.empty funDefs
  where
    funMap = allFunEntries funDefs
    funSet = allFunNames funDefs
    toNode = fromMaybe _ . flip Map.lookup funMap

definitionSort :: Map String (FunDef L0.Exp0) -> Map String [FunDef L0.Exp0] -> [FunDef L0.Exp0]
definitionSort m1 m2 =
  (\(_, n, _) -> n) . back <$> topSort gr
  where
    (gr, back, _) = graphFromEdges $ (\(s, lst) -> 
      (s, fromMaybe _ (Map.lookup s m1), lst)) <$> toList m2

sortDefs :: [FunDef L0.Exp0] -> [FunDef L0.Exp0]
sortDefs defs = 
  definitionSort nameMap depMap
  where
    depMap = getDependencies defs
    nameMap = fromList ((\d -> (getVar (funName d), d)) <$> defs)
