{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wall #-}

-- | The source language for recursive tree traversals.
--   This is a first-order language for the "closed world" scenario:
--   not integrating with a functional host language, but rather
--   genarating C code like a DSL.

module Packed.FirstOrder.L1_Source
    ( Prog(..), DDef(..), FunDefs, FunDef(..), Exp(..), Ty(..), Prim(..),
     add1Prog
    )
    where

import Packed.FirstOrder.Common
import Data.Map as M
-- import Data.List as L
import GHC.Generics
import Text.PrettyPrint.GenericPretty

--------------------------------------------------------------------------------

-- | Complete programs include datatype definitions:
--
-- For evaluating a complete program, main's type will be an Int or a
-- datatype.  For running a pass benchmark, main will be Nothing and
-- we will expect a "benchmark" function definition which consumes an
-- appropriate packed AST datatype.
data Prog = Prog { ddefs    :: DDefs Ty
                 , fundefs  :: FunDefs Ty Exp
                 , mainExp  :: Maybe Exp
                 }
  deriving (Read,Show,Eq,Ord, Generic)


-- | The source language.  It has pointer based sums and products, as
-- well as packed algebraic datatypes.
data Exp = VarE Var
         | LitE Int
         | AppE Var [Exp] -- Only apply top-level / first-order functions
         | PrimAppE Prim [Exp]
         | LetE (Var,Ty,Exp) Exp
           -- ^ One binding at a time, but could bind a tuple for
           -- mutual recursion.
         | ProjE Int Exp
         | MkProdE [Exp]
         | CaseE Exp (M.Map Constr ([Var], Exp))
           -- ^ Case on a PACKED datatype.
         | MkPackedE Constr [Exp]
  deriving (Read,Show,Eq,Ord, Generic)

data Prim = AddP | SubP | MulP -- ^ May need more numeric primitives...
          | DictInsertP -- ^ takes k,v,dict
          | DictLookupP -- ^ takes k dict, errors if absent
  deriving (Read,Show,Eq,Ord, Generic)

instance Out Prim
instance Out Ty
-- Do this manually to get prettier formatting:
-- instance Out Ty where  doc x = undefined

instance Out Exp
instance Out Prog

-- type TEnv = Map Var Ty

-- | Types include boxed/pointer-based products as well as unpacked
-- algebraic datatypes.
data Ty = IntTy
        | SymTy -- ^ Symbols used in writing compiler passes.
                --   It's an alias for Int, an index into a symbol table.
        | ProdTy [Ty]   -- ^ An N-ary tuple 
        | SymDictTy Ty  -- ^ A map from SymTy to Ty
        | Packed Constr -- ^ No type arguments to TyCons for now.
          -- ^ We allow built-in dictionaries from symbols to a value type.
  deriving (Show, Read, Ord, Eq, Generic)

           

--------------------------------------------------------------------------------

{-
-- | Promote a value to a term that evaluates to it.
l1FromValue :: Value Exp -> Exp
l1FromValue x =
  case x of
    VLeft x -> InL $ l1FromValue x
    (VInt y) -> undefined
    (VLam y1 y2 y3) -> undefined
    (VProd y1 y2) -> undefined
    (VLeft y) -> undefined
    (VRight y) -> undefined
    (VPacked y1 y2) -> undefined

-- | To keep things simple we evaluate directly to a string.
interp :: DDefs Ty -> Exp -> Value Exp
interp _ddefs = go M.empty
    where
      go env x =
          case x of
            Lit c    -> VInt c
            VarE v -> env ! v
            App a b ->
              let rand = go env b
                  (VLam env' v bod) = go env a
              in go (M.insert v rand env') bod
            Lam (v,_ty) bod -> VLam env v bod
            CaseEither a b c -> case go env a of
                                  VLeft  v -> go env $ App b $ l1FromValue v
                                  VRight v -> go env $ App c $ l1FromValue v
                                  _ -> error "L1 interp: type error"
            (CasePacked x1 mp) -> case go env x1 of
                                    VPacked k ls ->
                                      let Just (vs,rhs) = M.lookup k mp
                                          env' = M.union (M.fromList (zip vs ls))
                                                         env
                                      in go env' rhs
                                    _ -> error "L1 interp: type error"
            (Add a b) -> case (go env a, go env b) of
                           (VInt c, VInt d) -> VInt $ c+d
                           _ -> error "L1 interp: type error"
            (Letrec (v,ty,rhs) bod) ->
              let env' = M.insert v rhs' env
                  rhs' = go env' rhs
              in go env' bod

            (InL x) -> VLeft  $ go env x
            (InR x) -> VRight $ go env x
            (MkProdE a b) -> VProd (go env a) (go env b)
            -- TODO: Should check this against the ddefs.
            (MkPacked k ls) -> VPacked k $ L.map (go env) ls

interpProg :: Prog -> Value Exp
interpProg Prog {defs,mainProg} = interp defs mainProg

tyc :: TEnv -> Exp -> Ty
tyc = undefined

--------------------------------------------------------------------------------

p1 :: Prog
p1 = Prog emptyDD (Letrec ("x",TInt,Lit 3) (Varref "x")) TInt

main :: IO ()
main = print (interpProg p1)
-}


treeTy :: Ty
treeTy = Packed "Tree"

add1Prog :: Prog
add1Prog = Prog (fromListDD [DDef "Tree" [ ("Leaf",[IntTy])
                                         , ("Node",[Packed "Tree", Packed "Tree"])]])
                (M.fromList [("add1",exadd1)])
                Nothing
         
exadd1 :: FunDef Ty Exp
exadd1 = FunDef "add1" treeTy [("tr",treeTy)] exadd1Bod

exadd1Bod :: Exp
exadd1Bod =
    CaseE (VarE "tr") $ M.fromList $ 
      [ ("Leaf", (["n"], PrimAppE AddP [VarE "n", LitE 1]))
      , ("Node", (["x","y"], MkPackedE "Node"
                              [ AppE "add1" [VarE "x"]
                              , AppE "add1" [VarE "y"]]))
      ]
