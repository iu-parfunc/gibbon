module Gibbon.L0.GenSML where

import Gibbon.L0.Syntax
import Gibbon.Common

import Text.PrettyPrint hiding ((<>))

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
  PrintPacked _ty0 _pe -> error "PrintPacked"  -- Todo
  CopyPacked _ty0 _pe -> error "CopyPacked"
  TravPacked _ty0 _pe -> error "TravPacked"
  L _loc _pe -> error "L"
  LinearExt _le -> error "LinearExt"

ppPreExp :: PreExp E0Ext Ty0 Ty0 -> Doc
ppPreExp pe = case pe of
  VarE (Var s) -> text $ show s
  LitE n -> text $ show n
  CharE c -> char c
  FloatE x -> text $ show x
  LitSymE (Var s) -> quotes $ text $ show s
  AppE var _ pes -> ppApp (ppVar var) pes
  PrimAppE pr pes -> ppPrim pr pes
  LetE (v, _, _, e) pe' ->
    hsep
      [ "let val", ppVar v, "="
      , ppPreExp e, "in"
      , ppPreExp pe'
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
        , parens $ hsep $ ppVar . fst <$> vs
        , "=>", ppPreExp e
        ]) <$> x0)
      ]
  DataConE _ty0 s pes -> 
    hsep [text s, parens $ hsep $ ppPreExp <$> pes]

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
  ErrorP s _ -> hsep [text "raise", quotes $ text s]
  SizeParam -> error "SizeParam"
  IsBig -> error "IsBig"
  GetNumProcessors -> error "GetNumProcessors"
  PrintInt -> ppApp (text "print") pes <> semi
  PrintChar -> ppApp (text "print") pes <> semi
  PrintFloat -> ppApp (text "print") pes <> semi
  PrintBool ->
    ppApp (text "(fn true => \"True\" | false => \"False\")") pes <> semi
  PrintSym -> ppApp (text "print") pes <> semi
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
ppVar (Var s) = text $ show s

interleave :: Doc -> [Doc] -> Doc
interleave sepr lst = case lst of
  [] -> empty
  d : ds -> d <+> foldr (\x -> (sepr <+> x <>)) empty ds

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
