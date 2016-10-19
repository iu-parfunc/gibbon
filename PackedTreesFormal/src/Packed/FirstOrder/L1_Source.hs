{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wall #-}

-- | The source language for recursive tree traversals.
--   This is a first-order language for the "closed world" scenario:
--   not integrating with a functional host language, but rather
--   genarating C code like a DSL.

module Packed.FirstOrder.L1_Source where

import Data.Map as M
-- import Data.List as L
import GHC.Generics
import Text.PrettyPrint.GenericPretty

--------------------------------------------------------------------------------
-- Should be common bits:

type Var    = String
type Constr = String
type DDefs a = Map Var (DDef a)

-- | In the extreme case we can strip packed datatypes of all type
-- parameters, or we can allow them to retain type params but require
-- that they always be fully instantiated to monomorphic types in the
-- context of our monomorphic programs.
data DDef a = DDef { tyName:: Var
--                   , tyArgs:: [Var] 
                   , dataCons :: [(Constr,[a])] }
  deriving (Read,Show,Eq,Ord, Functor, Generic)

instance Out a => Out (DDef a)
instance (Out k,Out v) => Out (Map k v) where
  doc         = doc . M.toList
  docPrec n v = docPrec n (M.toList v)

-- | A set of top-level recursive function definitions
type FunDefs ty ex = Map Var (FunDef ty ex)
                   
data FunDef ty ex = FunDef { funname :: Var
                           , funargs :: [(Var,ty)]
                           , funbod  :: ex }
  deriving (Read,Show,Eq,Ord, Generic)

instance (Out a, Out b) => Out (FunDef a b)

--------------------------------------------------------------------------------

-- | Complete programs include datatype definitions:
-- 
-- For evaluating a complete program, main's type will be an Int or a
-- datatype.  For running a pass benchmark, main will be Nothing and
-- we will expect a "benchmark" function definition which consumes an
-- appropriate packed AST datatype.
data P1 = P1 { ddefs    :: DDefs T1
             , fundefs  :: FunDefs T1 L1                                         
             , mainExp  :: Maybe (L1,T1)
             }
  deriving (Read,Show,Eq,Ord, Generic)

           
-- | The source language.  It has pointer based sums and products, as
-- well as packed algebraic datatypes.
data L1 = Varref Var | Lit Int 
        | App Var L1 -- Only apply top-level / first-order functions
        | PrimApp Prim [L1]
        | Letrec (Var,T1,L1) L1
          -- ^ One binding at a time, but could bind a tuple for
          -- mutual recursion.
        | Fst L1 | Snd L1 | MkProd L1 L1
        | CasePacked L1 (M.Map Constr ([Var], L1))
        | MkPacked Constr [L1]
  deriving (Read,Show,Eq,Ord, Generic)

data Prim = Add | Sub | Mul -- ^ Need more numeric primitives...
          | DictInsert -- ^ takes k,v,dict
          | DictLookup -- ^ takes k dict, errors if absent
  deriving (Read,Show,Eq,Ord, Generic)


instance Out Prim
instance Out T1
-- Do this manually to get prettier formatting:
-- instance Out T1 where  doc x = undefined

instance Out L1
instance Out P1
           
type TEnv = Map Var T1
           
-- | Types include boxed/pointer-based products as well as unpacked
-- algebraic datatypes.
data T1 = TInt | TSym -- Symbols used in writing compiler passes
        | Prod T1 T1
        | TDict T1 -- We allow built-in dictionaries from symbols to a value type.
        | Packed Constr [T1]
  deriving (Read,Show,Eq,Ord, Generic)
           
  
--------------------------------------------------------------------------------

{-
-- | Promote a value to a term that evaluates to it.
l1FromValue :: Value L1 -> L1
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
interp :: DDefs T1 -> L1 -> Value L1
interp _ddefs = go M.empty
    where
      go env x = 
          case x of
            Lit c    -> VInt c
            Varref v -> env ! v
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
            (MkProd a b) -> VProd (go env a) (go env b)
            -- TODO: Should check this against the ddefs.
            (MkPacked k ls) -> VPacked k $ L.map (go env) ls

interpProg :: P1 -> Value L1
interpProg P1 {defs,mainProg} = interp defs mainProg

tyc :: TEnv -> L1 -> T1
tyc = undefined
       
--------------------------------------------------------------------------------

p1 :: P1
p1 = P1 emptyDD (Letrec ("x",TInt,Lit 3) (Varref "x")) TInt

main :: IO ()
main = print (interpProg p1)
-}
