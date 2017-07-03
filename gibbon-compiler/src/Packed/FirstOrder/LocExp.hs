{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Packed.FirstOrder.LocExp where

import Packed.FirstOrder.Common
import Data.Map as M
import Data.Set as S
import Data.List as L
import GHC.Generics
import Text.PrettyPrint.GenericPretty
import Control.DeepSeq (NFData)

type Loc = LocVar

type Reg = Var
    
data Program = Program (Map Var Fdef) (Maybe Exp)
               deriving (Read,Show,Eq,Ord, Generic, NFData)
                        
data Fdef = Fdef Var Ftype [Rp] [Var] Exp
            deriving (Read,Show,Eq,Ord, Generic, NFData)
                     
data Rp = LocIn Loc Reg
        | LocOut Loc Reg
          deriving (Read,Show,Eq,Ord, Generic, NFData)

data Ftype = PrimType Ptype
           | FunType [Rp] Ptype Ptype
             deriving (Read,Show,Eq,Ord, Generic, NFData)

data Ptype = ProdType [Ptype]
           | IntType
           | BoolType
           | SymType
           | DictType Ptype
           | PackedType Var
             deriving (Read,Show,Eq,Ord, Generic, NFData)

data Exp = VarE Var
         | LitE Int
         | LitSymE Var
         | AppE Var [Rp] [Exp]
         | PrimAppE Prim [Exp]
         | LetPackedE Var Ptype Exp Exp
         | LetRegionE Reg Exp
         | LetLocE Var LocExp Exp
         | LetE Var Ptype Exp Exp
         | MkProdE [Exp]
         | MkPackedE Var Loc [Exp]
         | ProjE Int Exp
         | IfE Exp Exp Exp
           deriving (Read,Show,Eq,Ord, Generic, NFData)

data LocExp = StartL Reg
            | PlusCL Int Loc
            | PlusSizeOfL Var Loc
              deriving (Read,Show,Eq,Ord, Generic, NFData)

data Prim = AddP | SubP | MulP -- ^ May need more numeric primitives...
          | EqSymP          -- ^ Equality on Sym
          | EqIntP       -- ^ Equality on Int
          | DictInsertP Ptype  -- ^ takes dict, k,v; annotated with element type
          | DictLookupP Ptype  -- ^ takes dict,k errors if absent; annotated with element type
          | DictEmptyP Ptype   -- ^ annotated with element type to avoid ambiguity
          | DictHasKeyP Ptype  -- ^ takes dict,k; returns a Bool, annotated with element type
          | Gensym
          | ErrorP String Ptype
              -- ^ crash and issue a static error message.
              --   To avoid needing inference, this is labeled with a return type.
          | MkTrue -- ^ Zero argument constructor.
          | MkFalse -- ^ Zero argument constructor.
            deriving (Read,Show,Eq,Ord, Generic, NFData)

voidType :: Ptype
voidType = ProdType []


hasPacked :: Ptype -> Bool
hasPacked (ProdType ls) = any hasPacked ls
hasPacked (PackedType _) = True
hasPacked IntType = False
hasPacked BoolType = False
hasPacked SymType = False
hasPacked (DictType ty) = hasPacked ty


freeVars :: Exp -> Set Var
freeVars (VarE v) = S.singleton v
freeVars (LitE _) = S.empty
freeVars (LitSymE _) = S.empty
freeVars (AppE _v _ ls) = S.unions (L.map freeVars ls)
freeVars (PrimAppE _ ls) = S.unions (L.map freeVars ls)
freeVars (LetPackedE v _ e1 e2) = S.union (freeVars e1) (S.delete v (freeVars e2))
freeVars (LetRegionE _ e) = freeVars e
freeVars (LetLocE v1 (PlusSizeOfL v2 _) e) = S.union (S.singleton v2) (S.delete v1 (freeVars e))
freeVars (LetLocE v _ e) = S.delete v (freeVars e)
freeVars (LetE v _ e1 e2) = S.union (freeVars e1) (S.delete v (freeVars e2))
freeVars (MkProdE ls) = S.unions (L.map freeVars ls)
freeVars (MkPackedE _ _ ls) = S.unions (L.map freeVars ls)
freeVars (ProjE _ e) = freeVars e
freeVars (IfE e1 e2 e3) = S.unions [freeVars e1, freeVars e2, freeVars e3]

data Constr = StartOfC Loc Reg
            | AfterC Loc Loc
            | InC Loc Reg

data LocState = InLS Loc
              | OutLS Loc
              | AfterLS Loc
              | StartS Reg

typeofE :: DDefs Var -> (Map Var Ptype) -> (Set Constr) -> (Set Reg) -> (Set LocState) -> Exp ->
           (Ptype,Set LocState)
typeofE dd g c r ls exp =
    case exp of
      VarE v -> case M.lookup v g of
                  Just t -> (t, ls)
                  Nothing -> error ("Failed to lookup variable " ++ (show v)
                                   ++ " in gamma: " ++ (show g))
      LitE _ -> (IntType, ls)
      LitSymE _ -> (SymType, ls)
      AppE v rps exps -> undefined
      PrimAppE p exps -> undefined
      LetPackedE v pt e1 e2 -> let (t1, ls1) = typeofE dd g c r ls e1
                               in if (t1 == pt) then typeofE dd (M.insert v pt g) c r ls1 e2
                                  else error ("Type of let bound expression was " ++ (show t1)
                                             ++ " but expected " ++ (show pt) ++ " for exp: "
                                             ++ (show e1))
      LetRegionE r' exp -> typeofE dd g c (S.insert r' r) ls exp  
      LetLocE v le exp -> case le of
                            StartL r' -> undefined
                            PlusCL _ l -> undefined
                            PlusSizeOfL v l -> undefined
      LetE v pt e1 e2 ->
          if hasPacked pt then error ("Expected unpacked type, found " ++ (show pt))
          else let (t1, ls1) = typeofE dd g c r ls e1
               in if (t1 == pt) then typeofE dd (M.insert v pt g) c r ls1 e2
                  else error ("Type of let bound expression was " ++ (show t1)
                             ++ " but expected " ++ (show pt) ++ " for exp: "
                             ++ (show e1))
      MkProdE exps -> undefined
      MkPackedE v l exps -> undefined
      ProjE i exp -> undefined
      IfE e1 e2 e3 -> undefined
