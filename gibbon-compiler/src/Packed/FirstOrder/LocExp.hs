{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}


--- Rough translation of L1 language from Redex model to Haskell.
---
--- Soon this can either be merged with the existing L1_Source module,
--- discarded in favor of direcly modifying L1_Source, or expanded
--- to replace L1_Source.
---
--- Design goals:
---  * Mirror the design of the language in the redex model reasonably closely.
---  * Share some structure with existing AST types (ie. use Common module).
---  * Avoid fancy parameterized types for now.
---
--- Overall picture:
--- A program consists of a series of data type definitions (DDefs, unchanged),
--- a series of function definitions, and an optional expression. Functions
--- now separately take both a normal argument list and a list of in/out
--- locations. Expressions have different let binding forms for locations,
--- packed data, non-packed data, and regions.
---
--- Big differences from model (so far): arbitrary tuple sizes, and arbitrary
--- data types (not just leaf/node).
---
--- Since this language requires that we thread through location information
--- and do computation in the right order, it's not suitable as a direct
--- target from Racket (yet). This can be worked out as we integrate it
--- into the full compiler. Possibilities:
---  * AST will be translated to this IR once we infer location info.
---  * This language is extended with indirection and all source programs
---    start out using only indirection, to be changed by later optimization.

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
    
data LocProgram = LocProgram (Map Var Fdef) (Maybe Exp)
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

--- TODO: share Prim (put in Common maybe?)
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

--- TODO: finish typechecker
--- does it need to be monadic, or can it be pure?
typeofE :: DDefs Var -> (Map Var Ftype) -> (Set Constr) -> (Set Reg) -> (Set LocState) -> Exp ->
           (Ftype,Set LocState)
typeofE dd g c r ls exp =
    case exp of
      VarE v -> case M.lookup v g of
                  Just t -> (t, ls)
                  Nothing -> error ("Failed to lookup variable " ++ (show v)
                                   ++ " in gamma: " ++ (show g))
      LitE _ -> (PrimType IntType, ls)
      LitSymE _ -> (PrimType SymType, ls)
      LetPackedE v pt e1 e2 -> let (t1, ls1) = typeofE dd g c r ls e1
                               in if (t1 == (PrimType pt)) then typeofE dd (M.insert v (PrimType pt) g) c r ls1 e2
                                  else error ("Type of let bound expression was " ++ (show t1)
                                             ++ " but expected " ++ (show pt) ++ " for exp: "
                                             ++ (show e1))
      LetRegionE r' exp -> typeofE dd g c (S.insert r' r) ls exp  
      LetE v pt e1 e2 ->
          if hasPacked pt then error ("Expected unpacked type, found " ++ (show pt))
          else let (t1, ls1) = typeofE dd g c r ls e1
               in if (t1 == (PrimType pt)) then typeofE dd (M.insert v (PrimType pt) g) c r ls1 e2
                  else error ("Type of let bound expression was " ++ (show t1)
                             ++ " but expected " ++ (show pt) ++ " for exp: "
                             ++ (show e1))
      AppE v rps exps -> case M.lookup v g of
                           Just t -> undefined
                           Nothing -> error ("Failed to lookup variable " ++ (show v)
                                            ++ " in gamma: " ++ (show g))
      LetLocE v le exp -> case le of
                            StartL r' -> undefined
                            PlusCL _ l -> undefined
                            PlusSizeOfL v l -> undefined
      MkProdE exps -> undefined
      MkPackedE v l exps -> undefined
      ProjE i exp -> undefined
      IfE e1 e2 e3 -> undefined
      PrimAppE p exps -> undefined

--- TODO: finish interpreter

interpE :: DDefs Var -> (Map Var Fdef) -> (Map Var Exp) -> Exp -> SyM Exp
interpE dd fenv env exp =
    case exp of
      VarE v -> case M.lookup v env of
                  Just e -> return e
                  Nothing -> undefined
      LitE i -> return $ LitE i
      LitSymE s -> return $ LitSymE s
      AppE v _ exps -> case M.lookup v fenv of
                         Just (Fdef _ _ _ vs e) ->
                             do exps' <- mapM (interpE dd fenv env) exps
                                let env' = M.union env (M.fromList (zip vs exps'))
                                interpE dd fenv env' e
                         Nothing -> undefined
      LetPackedE v _ e1 e2 -> do e1' <- interpE dd fenv env e1
                                 let env' = M.insert v e1' env
                                 interpE dd fenv env' e2
      LetRegionE _ exp -> interpE dd fenv env exp
      LetLocE _ _ exp -> interpE dd fenv env exp
      MkProdE exps -> do exps' <- mapM (interpE dd fenv env) exps
                         return $ MkProdE exps'
      MkPackedE v _ exps -> undefined
      ProjE i exp -> do exp' <- interpE dd fenv env exp
                        case exp' of
                          MkProdE exps -> return $ exps !! i
                          _ -> undefined
      IfE e1 e2 e3 -> do e1' <- interpE dd fenv env e1
                         case e1' of
                           PrimAppE MkTrue [] -> interpE dd fenv env e2
                           PrimAppE MkFalse [] -> interpE dd fenv env e3
                           _ -> undefined
      PrimAppE p exps ->
          case p of
            AddP -> do let [e1,e2] = exps
                       (LitE i1) <- interpE dd fenv env e1
                       (LitE i2) <- interpE dd fenv env e2
                       return $ LitE (i1 + i2)
            SubP -> do let [e1,e2] = exps
                       (LitE i1) <- interpE dd fenv env e1
                       (LitE i2) <- interpE dd fenv env e2
                       return $ LitE (i1 - i2)
            MulP -> do let [e1,e2] = exps
                       (LitE i1) <- interpE dd fenv env e1
                       (LitE i2) <- interpE dd fenv env e2
                       return $ LitE (i1 * i2)
            EqIntP -> do let [e1,e2] = exps
                         (LitE i1) <- interpE dd fenv env e1
                         (LitE i2) <- interpE dd fenv env e2
                         return $ if (i1 == i2) then PrimAppE MkTrue [] else PrimAppE MkFalse []
            EqSymP -> do let [e1,e2] = exps
                         (LitSymE i1) <- interpE dd fenv env e1
                         (LitSymE i2) <- interpE dd fenv env e2
                         return $ if (i1 == i2) then PrimAppE MkTrue [] else PrimAppE MkFalse []
            MkTrue -> return $ PrimAppE MkTrue []
            MkFalse -> return $ PrimAppE MkFalse []
