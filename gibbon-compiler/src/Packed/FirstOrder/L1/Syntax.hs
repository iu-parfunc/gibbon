{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP      #-}

-- | The source language for recursive tree traversals.
--   This is a first-order language for the "closed world" scenario:
--   not integrating with a functional host language, but rather
--   genarating C code like a DSL.

module Packed.FirstOrder.L1.Syntax
    (
     -- * Core types
      Prog(..), DDef(..), FunDefs, FunDef(..),
      Exp1, Exp, PreExp(..)
    , progToEnv

      -- * Primitive operations
    , Prim(..), primArgsTy

      -- * Types and helpers
    , Ty1, Ty, UrTy(..), pattern Packed, pattern SymTy
    , voidTy, hasPacked, sizeOf

    -- * Expression and Prog helpers
    , freeVars, subst, substE, getFunTy
    , mapExprs
    , mapExt
   , mapLocs

      -- * Trivial expressions
    , assertTriv, assertTrivs, isTriv, hasTimeIt
    , projNonFirst, mkProj, mkProd, mkProdTy, mkLets

      -- * Examples
    , add1Prog
    , add1ProgLetLeft
    , add1ProgLetRight
    , add1ProgChallenge
    , add1ProgSharing
    )
    where

import Control.DeepSeq (NFData, rnf)
import Data.List as L
import Data.Loc
import Data.Map as M
import Data.Set as S
import GHC.Generics
import Text.PrettyPrint.GenericPretty

import Packed.FirstOrder.Common as C
import Packed.FirstOrder.GenericOps

--------------------------------------------------------------------------------

-- | Complete programs include datatype definitions:
--
-- For evaluating a complete program, main's type will be an Int or a
-- datatype.  For running a pass benchmark, main will be Nothing and
-- we will expect a "benchmark" function definition which consumes an
-- appropriate packed AST datatype.
data Prog = Prog { ddefs    :: DDefs Ty1
                 , fundefs  :: FunDefs Ty1 (L Exp1)
                 , mainExp  :: Maybe (L Exp1)
--                 , constraints :: [Constraint]
                 }
  deriving (Show,Eq,Ord, Generic, NFData)

-- FIXME: Finish this and merge with L2.Constraint
data Constraint = Constraint
  deriving (Read,Show,Eq,Ord, Generic, NFData)
instance Out Constraint

-- | Abstract some of the differences of top level program types, by
--   having a common way to extract an initial environment.
progToEnv :: Prog -> Env2 (UrTy ())
progToEnv Prog{fundefs} =
    Env2 M.empty
         (M.fromList [ (n,(fmap (\_->()) a, fmap (\_->()) b))
                     | FunDef n (_,a) b _ <- M.elems fundefs ])


-- | A convenient, default instantiation of the L1 expression type.
type Exp1 = Exp

-- | Deprecated alias for Exp1
type Exp = PreExp NoExt () Ty1
{-# DEPRECATED Exp "use Exp1 instead" #-}

-- Shorthand to make the below definition more readable.
-- I.e., this covers all the verbose recursive fields.
#define EXP (L (PreExp ext loc dec))

-- | The source language.  It has pointer-based sums and products, as
-- well as packed algebraic datatypes.
--
-- (1) It is parameterized by an a potential extension point.
--
-- (2) It is parameterized by 'loc', the type of locations.
--
-- (3) It is parameterized by a decoration, d, attached to every binder.
--
data PreExp (ext :: * -> * -> *) loc dec =
     VarE Var              -- ^ Variable reference
   | LitE Int              -- ^ Numeric literal
   | LitSymE Var           -- ^ A quoted symbol literal.
   | AppE Var [loc] EXP
     -- ^ Apply a top-level / first-order function.  Instantiate
     -- its type schema by providing location-variable arguments,
     -- if applicable.
   | PrimAppE Prim [EXP]
     -- ^ Primitive applications don't manipulate locations.
   | LetE (Var,[loc],dec, EXP) -- binding
          EXP                  -- body
    -- ^ One binding at a time.  Allows binding a list of
    -- implicit *location* return vales from the RHS, plus a single "real" value.
    -- This list of implicit returnsb

   | IfE EXP EXP EXP

   -- TODO: eventually tuples will just be a wired-in datatype.
   | MkProdE   [EXP] -- ^ Tuple construction
   | ProjE Int EXP   -- ^ Tuple projection.

   | CaseE EXP [(DataCon, [(Var,loc)], EXP)]
     -- ^ Case on a datatype.  Each bound, unpacked variable lives at
     -- a fixed, read-only location.

   | DataConE loc DataCon [EXP]
     -- ^ Construct data that may unpack some fields.  The location
     -- argument, if applicable, is the byte location at which to
     -- write the tag for the sum type.

   | TimeIt EXP dec Bool
    -- ^ The boolean being true indicates this TimeIt is really (iterate _)
    -- This iterate form is used for criterion-style benchmarking.

   -- Limited list handling:
   -- TODO: RENAME to "Array".
   | MapE  (Var,Ty, EXP) EXP  -- TODO: Replace with Generate, add array reference.
   | FoldE { initial  :: (Var,dec,EXP)
           , iterator :: (Var,dec,EXP)
           , body     :: EXP }

   ----------------------------------------
  | Ext (ext loc dec) -- ^ Extension point for downstream language extensions.

  deriving (Show,Eq,Ord, Generic, NFData, Functor)

instance NFData a => NFData (L a) where
  rnf (L loc a) = seq loc (rnf a)

instance Out a => Out (L a) where
  doc (L _ a) = doc a
  docPrec n (L _ a) = docPrec n a


instance (Out l, Show l, Show d, Out d, Expression (e l d))
         => Expression (PreExp e l d) where
  type (TyOf (PreExp e l d))  = d
  type (LocOf (PreExp e l d)) = l

instance Expression (PreExp e l d) => Expression (L (PreExp e l d)) where
  type (TyOf (L (PreExp e l d)))  = d
  type (LocOf (L (PreExp e l d))) = l

-- | Apply a function to the extension points only.
mapExt :: (e1 l d -> e2 l d) -> PreExp e1 l d -> PreExp e2 l d
mapExt fn = visitExp id fn id

-- | Apply a function to the locations only.
mapLocs :: (e l2 d -> e l2 d) -> PreExp e l2 d -> PreExp e l2 d
mapLocs fn = visitExp id fn id


-- | Apply a function to the locations, extensions, and
-- binder-decorations, respectively.
visitExp :: forall l1 l2 e1 e2 d1 d2 .
            (l1 -> l2) -> (e1 l1 d1 -> e2 l2 d2) -> (d1 -> d2) ->
            PreExp e1 l1 d1 -> PreExp e2 l2  d2
visitExp fl fe fd exp = fin
 where
   L _ fin = go (L NoLoc exp)

   go :: L (PreExp e1 l1  d1) -> L (PreExp e2 l2 d2)
   go (L p ex) = L p $
     case ex of
       Ext  x        -> Ext (fe x)
       VarE v        -> VarE v
       LitE n        -> LitE n
       LitSymE x     -> LitSymE x
       AppE v l e    -> AppE v (L.map fl l) (go e)
       PrimAppE p ls -> PrimAppE p $ L.map go ls
       LetE (v,l,t,rhs) bod -> LetE (v,L.map fl l,fd t,go rhs) (go bod)
       ProjE i e  -> ProjE i (go e)
       CaseE e ls -> CaseE (go e)
                     [ (c, [ (v,fl l) | (v,l) <- vs ],go er)
                     | (c,vs,er) <- ls ]
       MkProdE ls         -> MkProdE $ L.map go ls
       DataConE loc k ls  -> DataConE (fl loc) k $ L.map go ls
       TimeIt e t b       -> TimeIt (go e) (fd t) b
       IfE a b c          -> IfE (go a) (go b) (go c)
       MapE (v,t,rhs) bod -> MapE (v,t, go rhs) (go bod)
       FoldE (v1,t1,r1) (v2,t2,r2) bod ->
         FoldE (v1,fd t1,go r1) (v2,fd t2,go r2) (go bod)


-- | Some of these primitives are (temporarily) tagged directly with
-- their return types.
data Prim = AddP | SubP | MulP -- ^ May need more numeric primitives...
          | EqSymP          -- ^ Equality on Sym
          | EqIntP       -- ^ Equality on Int
          | DictInsertP Ty1  -- ^ takes dict, k,v; annotated with element type
          | DictLookupP Ty1  -- ^ takes dict,k errors if absent; annotated with element type
          | DictEmptyP  Ty1  -- ^ annotated with element type to avoid ambiguity
          | DictHasKeyP Ty1  -- ^ takes dict,k; returns a Bool, annotated with element type
          | Gensym
          | ErrorP String Ty1
              -- ^ crash and issue a static error message.
              --   To avoid needing inference, this is labeled with a return type.

--          | GetLoc Var
--          | AddLoc Int Var
          | SizeParam

          | MkTrue -- ^ Zero argument constructor.
          | MkFalse -- ^ Zero argument constructor.

          | MkNullCursor -- ^ Zero argument constructor.
          | ReadPackedFile (Maybe FilePath) TyCon Ty
            -- ^ Read (mmap) a binary file containing packed data.  This must be annotated with the
            -- type of the file being read.  The `Ty` tracks the type as the program evolvels
            -- (first PackedTy then CursorTy).  The TyCon tracks the original type name.

  deriving (Read,Show,Eq,Ord, Generic, NFData)

instance Out Prim
instance Out a => Out (UrTy a)
-- Do this manually to get prettier formatting:
-- instance Out Ty where  doc x = __

instance (Out l, Out d, Out (e l d)) => Out (PreExp e l d)

instance Out Prog

-- type TEnv = Map Var Ty

-- TEMP/FIXME: leaving out these for now.
pattern SymTy :: forall a. UrTy a
pattern SymTy = IntTy

-- | The type rperesentation used in L1.
type Ty1 = UrTy ()

type Ty = Ty1
{-# DEPRECATED Ty "Moving away from generically named Ty/Exp/Prog" #-}

pattern Packed :: TyCon -> UrTy ()
pattern Packed c = PackedTy c ()

-- | Types include boxed/pointer-based products as well as unpacked
-- algebraic datatypes.  This data is parameterized to allow
-- annotation later on.
data UrTy a =
          IntTy
--        | SymTy -- ^ Symbols used in writing compiler passes.
--                --   It's an alias for Int, an index into a symbol table.
        | BoolTy
        | ProdTy [UrTy a]     -- ^ An N-ary tuple
        | SymDictTy (UrTy a)  -- ^ A map from SymTy to Ty
          -- ^ We allow built-in dictionaries from symbols to a value type.

        | PackedTy TyCon a    -- ^ No type arguments to TyCons for now.  (No polymorphism.)

        | ListTy (UrTy a) -- ^ These are not fully first class.  They are onlyae
                         -- allowed as the fields of data constructors.

        ---------- These are not used initially ----------------
        -- (They could be added by a later IR instead:)

        | PtrTy LRM (UrTy a) -- ^ A machine pointer to a complete value in memory.
                             -- This is decorated with the region it points into, which
                             -- may affect the memory layout.
        | CursorTy LRM -- ^ A cursor for reading or writing, which may point
                       -- to an unkwown type or to a fraction of a complete value.
                       -- It is a machine pointer that can point to any byte.

  deriving (Show, Read, Ord, Eq, Generic, NFData, Functor)


voidTy :: Ty
voidTy = ProdTy []

-- | Do values of this type contain packed data?
hasPacked :: Show a => UrTy a -> Bool
hasPacked t =
  case t of
    PackedTy{}     -> True
    ProdTy ls      -> any hasPacked ls
    SymTy          -> False
    BoolTy         -> False
    IntTy          -> False
    SymDictTy ty   -> hasPacked ty
    ListTy _       -> error "FINISHLISTS"
    PtrTy _lrm _ty -> error$ "hasPacked: should not be using this when PtrTy is introduced: "++show t
    CursorTy _lrm  -> error$ "hasPacked: should not be using this when CursorTy is introduced: "++show t

-- | Provide a size in bytes, if it is statically known.
sizeOf :: UrTy a -> Maybe Int
sizeOf t =
  case t of
    PackedTy{}  -> Nothing
    ProdTy ls   -> sum <$> mapM sizeOf ls
    SymDictTy _ -> Just 8 -- Always a pointer.
    IntTy       -> Just 8
    BoolTy      -> sizeOf IntTy
    ListTy _    -> error "FINISHLISTS"
    PtrTy{}     -> Just 8 -- Assuming 64 bit
    CursorTy{}  -> Just 8

-- | Transform the expressions within a program.
mapExprs :: (L Exp1 -> L Exp1) -> Prog -> Prog
mapExprs fn prg@Prog{fundefs,mainExp} =
  prg{ fundefs = fmap (fmap fn) fundefs
     , mainExp = fmap fn mainExp }


--------------------------------------------------------------------------------

-- | Look up the input/output type of a top-level function binding.
getFunTy :: Var -> Prog -> (Ty,Ty)
getFunTy fn Prog{fundefs} =
    case M.lookup fn fundefs of
      Just FunDef{funArg=(_vr,argty), funRetTy} -> (argty,funRetTy)
      Nothing -> error $ "getFunTy: L1 program does not contain binding for function: "++show fn

-- | Free data variables.  Does not include function variables, which
-- currently occupy a different namespace.  Does not include location/region variables.
freeVars :: Exp -> S.Set Var
freeVars = gFreeVars
{-# DEPRECATED freeVars "Use gFreeVars instead" #-}

instance FreeVars a => FreeVars (L a) where
  gFreeVars (L _ a) = gFreeVars a

instance FreeVars (e l d) => FreeVars (PreExp e l d) where
  gFreeVars ex = case ex of
      VarE v    -> S.singleton v
      LitE _    -> S.empty
      LitSymE _ -> S.empty
      ProjE _ e -> gFreeVars e
      IfE a b c -> gFreeVars a `S.union` gFreeVars b `S.union` gFreeVars c
      AppE _v _ e          -> gFreeVars e  -- S.insert v (gFreeVars e)
      PrimAppE _ ls        -> S.unions (L.map gFreeVars ls)
      LetE (v,_,_,rhs) bod -> gFreeVars rhs `S.union`
                              S.delete v (gFreeVars bod)
      CaseE e ls -> S.union (gFreeVars e)
                    (S.unions $ L.map (\(_, _, ee) -> gFreeVars ee) ls)
      MkProdE ls          -> S.unions $ L.map gFreeVars ls
      DataConE _ _ ls     -> S.unions $ L.map gFreeVars ls
      TimeIt e _ _        -> gFreeVars e
      MapE (v,_t,rhs) bod -> gFreeVars rhs `S.union`
                             S.delete v (gFreeVars bod)
      FoldE (v1,_t1,r1) (v2,_t2,r2) bod ->
          gFreeVars r1 `S.union` gFreeVars r2 `S.union`
          (S.delete v1 $ S.delete v2 $ gFreeVars bod)

      Ext q -> gFreeVars q


subst :: Var -> L Exp1 -> L Exp1 -> L Exp1
subst old new (L p ex) = L p $
  let go = subst old new in
  case ex of
    VarE v | v == old  -> unLoc new
           | otherwise -> VarE v
    LitE _             -> ex
    LitSymE _          -> ex
    AppE v l e         -> AppE v l (go e)
    PrimAppE p ls      -> PrimAppE p $ L.map go ls
    LetE (v,l,t,rhs) bod | v == old  -> LetE (v,l,t,go rhs) bod
                         | otherwise -> LetE (v,l,t,go rhs) (go bod)
    ProjE i e  -> ProjE i (go e)
    CaseE e ls ->
                  CaseE (go e) (L.map f ls)
                      where f (c,vs,er) = if L.elem old (L.map fst vs)
                                          then (c,vs,er)
                                          else (c,vs,go er)
    MkProdE ls        -> MkProdE $ L.map go ls
    DataConE loc k ls -> DataConE loc k $ L.map go ls
    TimeIt e t b      -> TimeIt (go e) t b
    IfE a b c         -> IfE (go a) (go b) (go c)
    MapE (v,t,rhs) bod | v == old  -> MapE (v,t, rhs)    (go bod)
                       | otherwise -> MapE (v,t, go rhs) (go bod)
    FoldE (v1,t1,r1) (v2,t2,r2) bod ->
        let r1' = if v1 == old then r1 else go r1
            r2' = if v2 == old then r2 else go r2
        in FoldE (v1,t1,r1') (v2,t2,r2') (go bod)

    Ext _ -> ex

-- | Expensive subst that looks for a whole matching sub-EXPRESSION.
--   If the old expression is a variable, this still avoids going under binder.
substE :: L Exp1 -> L Exp1 -> L Exp1 -> L Exp1
substE old new (L p ex) = L p $
  let go = substE old new in
  case ex of
    -- TODO(cskksc): this would return incorrect Loc
    _ | ex == unLoc old -> unLoc new

    VarE v          -> VarE v
    LitE _          -> ex
    LitSymE _       -> ex
    AppE v l e      -> AppE v l (go e)
    PrimAppE p ls   -> PrimAppE p $ L.map go ls
    LetE (v,l,t,rhs) bod | (VarE v) == unLoc old  -> LetE (v,l,t,go rhs) bod
                         | otherwise -> LetE (v,l,t,go rhs) (go bod)

    ProjE i e         -> ProjE i (go e)
    CaseE e ls        -> CaseE (go e) (L.map (\(c,vs,er) -> (c,vs,go er)) ls)
    MkProdE ls        -> MkProdE $ L.map go ls
    DataConE loc k ls -> DataConE loc k $ L.map go ls
    TimeIt e t b      -> TimeIt (go e) t b
    IfE a b c         -> IfE (go a) (go b) (go c)
    MapE (v,t,rhs) bod | VarE v == unLoc old  -> MapE (v,t, rhs)    (go bod)
                       | otherwise -> MapE (v,t, go rhs) (go bod)
    FoldE (v1,t1,r1) (v2,t2,r2) bod ->
        let r1' = if VarE v1 == unLoc old then r1 else go r1
            r2' = if VarE v2 == unLoc old then r2 else go r2
        in FoldE (v1,t1,r1') (v2,t2,r2') (go bod)

    Ext _ -> ex

primArgsTy :: Prim -> [Ty]
primArgsTy p =
  case p of
    AddP    -> [IntTy, IntTy]
    SubP    -> [IntTy, IntTy]
    MulP    -> [IntTy, IntTy]
    EqSymP  -> [SymTy, SymTy]
    EqIntP  -> [IntTy, IntTy]
    MkTrue  -> []
    MkFalse -> []
    MkNullCursor     -> []
    SizeParam        -> []
    Gensym           -> []
    DictEmptyP _ty   -> []
    DictInsertP _ty  -> error "primArgsTy: dicts not handled yet"
    DictLookupP _ty  -> error "primArgsTy: dicts not handled yet"
    DictHasKeyP _ty  -> error "primArgsTy: dicts not handled yet"
    ReadPackedFile{} -> []
    (ErrorP _ _) -> []


--------------------------------------------------------------------------------

-- Simple invariant assertions:

assertTriv :: L Exp1 -> a -> a
assertTriv e =
  if isTriv e
  then id
  else error$ "Expected trivial argument, got: "++sdoc e

assertTrivs :: [L Exp1] -> a -> a
assertTrivs [] = id
assertTrivs (a:b) = assertTriv a . assertTrivs b

-- | Is an expression considered trivial (duplicatable by the compiler)?
isTriv :: (Show l, Show d, Show (e l d)) => L (PreExp e l d) -> Bool
isTriv (L _ e) =
   case e of
     VarE _    -> True
     LitE _    -> True
     LitSymE _ -> True
     -- These should really turn to literalS:
     PrimAppE MkTrue  [] -> True
     PrimAppE MkFalse [] -> True
     PrimAppE _ _        -> False
     ----------------- POLICY DECISION ---------------
     -- Leave these tuple ops as trivial for now:
     ProjE _ et | isTriv et -> True
                | otherwise -> False
     MkProdE ls -> all isTriv ls

     Ext _      -> error$ "isTriv, got extension point, cannot handle: "++show e
     IfE{}      -> False
     CaseE{}    -> False
     LetE {}    -> False
     MapE {}    -> False
     FoldE {}   -> False
     AppE  {}   -> False
     TimeIt {}  -> False
     DataConE{} -> False

-- | Does the expression contain a TimeIt form?
hasTimeIt :: L Exp1 -> Bool
hasTimeIt (L _ rhs) =
    case rhs of
      TimeIt _ _ _ -> True
      DataConE{}   -> False
      VarE _       -> False
      LitE _       -> False
      LitSymE _    -> False
      AppE _ _ _   -> False
      PrimAppE _ _ -> False
      ProjE _ e    -> hasTimeIt e
      MkProdE ls   -> any hasTimeIt ls
      IfE a b c    -> hasTimeIt a || hasTimeIt b || hasTimeIt c
      CaseE _ ls   -> any hasTimeIt [ e | (_,_,e) <- ls ]
      LetE (_,_,_,e1) e2 -> hasTimeIt e1 || hasTimeIt e2
      MapE (_,_,e1) e2   -> hasTimeIt e1 || hasTimeIt e2
      FoldE (_,_,e1) (_,_,e2) e3 -> hasTimeIt e1 || hasTimeIt e2 || hasTimeIt e3
      Ext _ -> False

-- | Project something which had better not be the first thing in a tuple.
projNonFirst :: Int -> L Exp1 -> L Exp1
projNonFirst 0 e = error $ "projNonFirst: expected nonzero index into expr: " ++ sdoc e
projNonFirst i e = L (locOf e) $ ProjE i e

-- | Project position K of N, unless (K,N) = (0,1) in which case no
-- projection is necessary.
mkProj :: (Eq a, Num a) => Int -> a -> L Exp1 -> L Exp1
mkProj 0 1 e  = e
mkProj ix _ e = L (locOf e) $ ProjE ix e

-- | Make a product type while avoiding unary products.
mkProd :: [L Exp1]-> L Exp1
mkProd [e] = e
-- TODO(cskksc): this or NoLoc ?
mkProd ls  = L (locOf $ head ls) $ MkProdE ls

-- | Same as mkProd, at the type level
mkProdTy :: [Ty]-> Ty
mkProdTy [t] = t
mkProdTy ls  = ProdTy ls

-- | Make a nested series of lets.
mkLets :: [(Var,[l],Ty,L (PreExp NoExt l Ty))] ->
          L (PreExp NoExt l Ty) -> L (PreExp NoExt l Ty)
mkLets [] bod     = bod
mkLets (b:bs) bod = L NoLoc $ LetE b (mkLets bs bod)



--------------------------------------------------------------------------------

treeTy :: Ty
treeTy = Packed "Tree"

treeDD :: DDefs (UrTy ())
treeDD = (fromListDD [DDef "Tree"
                      [ ("Leaf",[(False,IntTy)])
                      , ("Node",[(False,Packed "Tree")
                                ,(False,Packed "Tree")])]])

mkAdd1Prog :: L Exp1 -> Maybe (L Exp1) -> Prog
mkAdd1Prog bod mainExp = Prog treeDD
                              (M.fromList [("add1",mkAdd1Fun bod)])
                              mainExp

mkAdd1Fun :: ex -> FunDef Ty ex
mkAdd1Fun bod = FunDef "add1" ("tr",treeTy) treeTy bod

----------------

-- TODO(cskksc): a function from Exp1 to (L Exp1) to avoid writing
-- (L NoLoc exp) everywhere

-- | The basic form of the add1 program where recursions happen
-- immediately as arguments to the data-constructor.
add1Prog :: Prog
add1Prog = mkAdd1Prog exadd1Bod Nothing

exadd1Bod :: L Exp1
exadd1Bod = L NoLoc $
    CaseE (L NoLoc $ VarE "tr") $
      [ ("Leaf", [("n",())],
         L NoLoc $ PrimAppE AddP [L NoLoc $ VarE "n", L NoLoc $ LitE 1])
      , ("Node", [("x",()),("y",())],
         L NoLoc $ DataConE () "Node"
          [ L NoLoc $ AppE "add1" [] (L NoLoc $ VarE "x")
          , L NoLoc $ AppE "add1" [] (L NoLoc $ VarE "y")])
      ]

exadd1BodLetLeft :: L Exp1
exadd1BodLetLeft = L NoLoc $
    CaseE (L NoLoc $ VarE "tr") $
      [ ("Leaf", [("n",())], L NoLoc $ PrimAppE AddP [L NoLoc $ VarE "n", L NoLoc $ LitE 1])
      , ("Node", [("x",()),("y",())],
         L NoLoc $ LetE ("x2",[], Packed "Tree", L NoLoc $ AppE "add1" [] (L NoLoc $ VarE "x")) $
         L NoLoc $ LetE ("y2",[], Packed "Tree", L NoLoc $ AppE "add1" [] (L NoLoc $ VarE "y")) $
         L NoLoc $ DataConE () "Node"
          [ L NoLoc $ VarE "x2", L NoLoc $ VarE "y2"])
      ]

-- | A more challenging case where recursions are performed right-to-left
exadd1BodLetRight :: L Exp1
exadd1BodLetRight = L NoLoc $
    CaseE (L NoLoc $ VarE "tr") $
      [ ("Leaf", [("n",())], L NoLoc $ PrimAppE AddP [L NoLoc $ VarE "n", L NoLoc $ LitE 1])
      , ("Node", [("x",()),("y",())],
         L NoLoc $ LetE ("y2",[], Packed "Tree", L NoLoc $ AppE "add1" [] (L NoLoc $ VarE "y")) $
         L NoLoc $ LetE ("x2",[], Packed "Tree", L NoLoc $ AppE "add1" [] (L NoLoc $ VarE "x")) $
         L NoLoc $ DataConE () "Node"
          [ L NoLoc $ VarE "x2", L NoLoc $ VarE "y2"])
      ]

-- | Add1 program with let bindings, recurring in left-to-right order.
add1ProgLetLeft :: Prog
add1ProgLetLeft = mkAdd1Prog exadd1BodLetLeft Nothing

-- | Add1 program with let bindings, recurring in right-to-left order.
add1ProgLetRight :: Prog
add1ProgLetRight = mkAdd1Prog exadd1BodLetRight Nothing


-- | An even more challenging case where there is an (apparent) data
-- dependency where x2 depends on y2.
add1ProgChallenge :: Prog
add1ProgChallenge =
    Prog treeDD
         (M.fromList [ ("add1",mkAdd1Fun bod)
                     , ("pred", FunDef "pred" ("tr", treeTy) BoolTy
                        (L NoLoc $ CaseE (L NoLoc $ VarE "tr") $
                         [ ("Leaf", [("n",())], L NoLoc $ PrimAppE MkTrue [])
                         , ("Node", [("x",()),("y",())], L NoLoc $ PrimAppE MkFalse [])]))])
         Nothing
  where
   bod = L NoLoc $
    CaseE (L NoLoc $ VarE "tr") $
      [ ("Leaf", [("n",())], L NoLoc $ PrimAppE AddP [L NoLoc $ VarE "n", L NoLoc $ LitE 1])
      , ("Node", [("x",()),("y",())],
         L NoLoc $ LetE ("y2",[], Packed "Tree", L NoLoc $ AppE "add1" [] (L NoLoc $ VarE "y")) $
         L NoLoc $ LetE ("x2",[], Packed "Tree",
              (L NoLoc $ IfE (L NoLoc $ AppE "pred" [] (L NoLoc $ VarE "y2"))
                   (L NoLoc $ AppE "add1" [] (L NoLoc $ VarE "x"))
                   (L NoLoc $ AppE "add1" [] (L NoLoc $ VarE "x")))) $
         L NoLoc $ DataConE () "Node" [ L NoLoc $ VarE "x2", L NoLoc $ VarE "y2"])
      ]

-- | This program is a challenge because a packed value flows to two destinations.
add1ProgSharing :: Prog
add1ProgSharing = Prog treeDD (M.fromList [("add1",mkAdd1Fun bod)]) Nothing
  where
   bod = L NoLoc $
    CaseE (L NoLoc $ VarE "tr") $
      [ ("Leaf", [("n",())], L NoLoc $ PrimAppE AddP [L NoLoc $ VarE "n", L NoLoc $ LitE 1])
      , ("Node", [("x",()),("y",())],
         L NoLoc $ LetE ("x2",[], Packed "Tree", L NoLoc $ AppE "add1" [] (L NoLoc $ VarE "x")) $
         L NoLoc $ DataConE () "Node" [ L NoLoc $ VarE "x2", L NoLoc $ VarE "x2"])
      ]
