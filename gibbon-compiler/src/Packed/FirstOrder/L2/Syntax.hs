{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

-- | An intermediate language with an effect system that captures traversals.

module Packed.FirstOrder.L2.Syntax
    ( Prog(..), FunDef(..), Effect(..), ArrowTy(..)
    , LocRet(..), LocExp, PreLocExp(..)
    , NewFuns, getFunTy
    , mapMExprs
    , progToEnv

    -- * Temporary backwards compatibility, plus rexports
    , UrTy(..)
    , PreExp(..)
    , pattern SymTy
    , primRetTy

    -- * Extended language L2.0 with location types.
    , Exp2, E2Ext(..), Ty2

    -- * Conversion back to L1
    , revertToL1


    -- The following will be removed:

-- (Maybe) DEPRECATED:
    -- * Extended "L2.1", for inline packed:
    , pattern NamedVal

-- DEPRECATED:
    -- * Extended "L2.2", for after cursor insertion:
    , pattern WriteInt, pattern ReadInt, pattern NewBuffer
    , pattern ScopedBuffer, pattern AddCursor
    , isExtendedPattern
    , builtinTEnv
    , includeBuiltins

    -- * Example
    , add1Prog, withAdd1Prog
    )
    where

import Control.DeepSeq
import Data.List as L
import Data.Loc
import Data.Set as S
import Data.Map as M
import Text.PrettyPrint.GenericPretty

import Packed.FirstOrder.Common hiding (FunDef)
import Packed.FirstOrder.GenericOps
import Packed.FirstOrder.L1.Syntax hiding
       (FunDef, Prog, mapExprs, progToEnv, fundefs, getFunTy, add1Prog)
import qualified Packed.FirstOrder.L1.Syntax as L1

--------------------------------------------------------------------------------

-- | Extended expressions, L2.  Monomorphic.
type Exp2 = E2 LocVar Ty2

-- | The extension that turns L1 into L2.
data E2Ext loc dec =
    LetRegionE Region                 (L (E2 loc dec)) -- ^ Not used until later on.
  | LetLocE    loc    (PreLocExp loc) (L (E2 loc dec)) -- ^ Bind a new location.
  | RetE [loc] Var     -- ^ Return a value together with extra loc values.
  | FromEndE loc -- ^ Bind a location from an EndOf location (for RouteEnds and after)
 deriving (Show, Ord, Eq, Generic, NFData)

-- | L1 expressions extended with L2.  This is the polymorphic version.
-- Shorthand for recursions above.
type E2 l d = PreExp E2Ext l d

instance Read (E2 l d) where
instance Read (L (E2 l d)) where

-- | Define a location in terms of a different location.
data PreLocExp loc = StartOfLE Region
                   | AfterConstantLE Int -- ^ Number of bytes after.
                                    loc  -- ^ Location which this location is offset from.
                   | AfterVariableLE Var -- ^ Name of variable v. This loc is size(v) bytes after.
                                    loc  -- ^ Location which this location is offset from.
                   | InRegionLE Region
                   | FromEndLE  loc
                     deriving (Read, Show, Eq, Ord, Generic, NFData)

type LocExp = PreLocExp LocVar

-- | Locations (end-witnesses) returned from functions after RouteEnds.
data LocRet = EndOf LRM
              deriving (Read, Show, Eq, Ord, Generic, NFData)

instance (Out l, Out d, Show l, Show d) => Expression (E2Ext l d) where
  type LocOf (E2Ext l d) = l
  type TyOf (E2Ext l d)  = UrTy l


----------------------------------------------------------------------------------------------------

-- | Our type for functions grows to include effects, and explicit universal
-- quantification over location/region variables.
data ArrowTy t = ArrowTy { locVars :: [LRM]       -- ^ Universally-quantified location params.
                                                  -- Only these should be referenced in arrIn/arrOut.
                         , arrIn :: t             -- ^ Input type for the function.
                         , arrEffs:: (Set Effect) -- ^ These are present-but-empty initially,
                                                  -- and the populated by InferEffects.
                         , arrOut:: t             -- ^ Output type for the function.
                         , locRets :: [LocRet]    -- ^ L2B feature: multi-valued returns.
                         }
  deriving (Read,Show,Eq,Ord, Generic, NFData)

-- | The side-effect of evaluating a function.
data Effect = Traverse LocVar
              -- ^ The function, during its execution, traverses all
              -- of the value living at this location.
  deriving (Read,Show,Eq,Ord, Generic, NFData)

-- instance Out Ty
instance Out t => Out (ArrowTy t)
instance Out Effect
instance Out a => Out (Set a) where
  docPrec n x = docPrec n (S.toList x)
  doc x = doc (S.toList x)
instance Out FunDef
instance Out Prog
instance (Out l, Out d) => Out (E2Ext l d)
instance Out l => Out (PreLocExp l)
instance Out LocRet


-- | L1 Types extended with abstract Locations.
type Ty2 = L1.UrTy LocVar


type NewFuns = M.Map Var FunDef

-- | Here we only change the types of FUNCTIONS:
data Prog = Prog { ddefs    :: DDefs Ty2
                 , fundefs  :: NewFuns
                 , mainExp  :: Maybe (L Exp2, Ty2)
                 }
  deriving (Show, Ord, Eq, Generic, NFData)

----------------------------------------------------------------------------------------------------

-- | Abstract some of the differences of top level program types, by
--   having a common way to extract an initial environment.  The
--   initial environment has types only for functions.
progToEnv :: Prog -> Env2 (UrTy ())
progToEnv Prog{fundefs} =
    Env2 M.empty
         (M.fromList [ (n,(fmap (\_->()) a, fmap (\_->()) b))
                     | FunDef n (ArrowTy _ a _ b _) _ _ <- M.elems fundefs ])


-- | A function definition with the function's effects.
data FunDef = FunDef { funname :: Var
                     , funty   :: (ArrowTy Ty2)
                     , funarg  :: Var
                     , funbod  :: L Exp2 }
  deriving (Show, Ord, Eq, Generic, NFData)
--------------------------------------------------------------------------------

-- | Retrieve the type of a function:
getFunTy :: NewFuns -> Var -> ArrowTy Ty2
getFunTy mp f = case M.lookup f mp of
                  Nothing -> error $ "getFunTy: function was not bound: "++show f
                  Just (FunDef{funty}) -> funty




-- TODO: beta-branch: REVAMP BELOW HERE
--------------------------------------------------------------------------------


-- | Retrieve all LocVars mentioned in a type
_getTyLocs :: Ty2 -> Set LocVar
_getTyLocs t =
    case t of
      IntTy  -> S.empty
      SymTy  -> S.empty
      BoolTy -> S.empty
      ProdTy ls     -> S.unions (L.map _getTyLocs ls)
      PackedTy _ lv -> S.singleton lv -- This is a tricky case:
      SymDictTy elt -> _getTyLocs elt
      -- TODO(chai):
      PtrTy (LRM lv _ _) elt -> S.union (S.singleton lv) (_getTyLocs elt)
      CursorTy (LRM lv _ _)  -> S.singleton lv
      ListTy{}               -> error "FINISHLISTS"


-- | Annotate a naked type with fresh location variables.
_tyWithFreshLocs :: Ty1 -> SyM Ty2
_tyWithFreshLocs t =
  case t of
    L1.Packed k -> PackedTy k <$> genLetter
    L1.IntTy    -> return IntTy
    L1.SymTy    -> return SymTy
    L1.BoolTy   -> return BoolTy
    L1.ProdTy l -> ProdTy <$> mapM _tyWithFreshLocs l
    L1.SymDictTy v  -> SymDictTy <$> _tyWithFreshLocs v
    L1.PackedTy _ _ -> error $ "tyWithFreshLocs: unexpected type: " ++ show t
    L1.PtrTy _ _    -> error $ "FINISHME: _tyWithFreshLocs PtrTy"
    L1.CursorTy _   -> error $ "FINISHME: _tyWithFreshLocs CursorTy"
    L1.ListTy _ -> error "tyWithFreshLocs: FIXME implement lists"

-- | Remove the extra location annotations.
_stripTyLocs :: Ty2 -> L1.Ty1
_stripTyLocs = fmap (const ())


-- | Apply a variable substitution to a type.
_substTy :: Map LocVar LocVar -> Ty2 -> Ty2
_substTy mp t = go t
  where
    go t =
     case t of
      IntTy  -> IntTy
      SymTy  -> SymTy
      BoolTy -> BoolTy
      SymDictTy te -> SymDictTy (go te)
      ProdTy    ts -> ProdTy    (L.map go ts)
      PackedTy k l ->
          case M.lookup l mp of
                Just v  -> PackedTy k v
                Nothing -> PackedTy k l
                -- errorWithStackTrace $ "substTy: failed to find "++show l++
                --   "\n  in map: "++show mp++", when processing type "++show t
      -- TODO(chai):
      PtrTy (LRM l reg mod) te ->
        case M.lookup l mp of
          Just v  -> PtrTy (LRM v reg mod) (go te)
          Nothing -> PtrTy (LRM l reg mod) (go te)
      CursorTy (LRM l reg mod) ->
        case M.lookup l mp of
          Just v  -> CursorTy (LRM v reg mod)
          Nothing -> CursorTy (LRM l reg mod)
      ListTy _ -> error "tyWithFreshLocs: FIXME implement lists"

-- | Apply a substitution to an effect set.
_substEffs :: Map LocVar LocVar -> Set Effect -> Set Effect
_substEffs mp ef =
    dbgTrace 5 ("\n  Substituting in effects "++show(mp,ef)) $
    S.map (\(Traverse v) ->
               case M.lookup v mp of
                 Just v2 -> Traverse v2
                 Nothing -> Traverse v) ef

-- | Collect all the locations mentioned in a type.
_allLocVars :: Ty2 -> [LocVar]
-- TODO: could just be a fold
_allLocVars t =
    case t of
      SymTy     -> []
      BoolTy    -> []
      IntTy     -> []
      PackedTy _ v  -> [v]
      ProdTy ls     -> L.concatMap _allLocVars ls
      SymDictTy elt -> _allLocVars elt
      PtrTy (LRM lv _ _) elt -> [lv] ++ _allLocVars elt
      CursorTy (LRM lv _ _)  -> [lv]
      ListTy _ -> error "allLocVars: FIXME lists"


-- | Map exprs with an initial type environment:
mapMExprs :: Monad m => (Env2 (UrTy LocVar) -> L Exp2 -> m (L Exp2)) -> Prog ->
             m Prog
mapMExprs = error $ "FINISHME: L2 mapMExprs"


--------------------------------------------------------------------------------

-- | Because L2 just adds a bit of metadata and enriched types, it is
-- possible to strip it back down to L1.
revertToL1 :: Prog -> L1.Prog
revertToL1 = undefined -- TODO: Fix or remove this function

--------------------------------------------------------------------------------


-- Conventions encoded inside the existing Core IR
-- =============================================================================

-- For use after inlinePacked / before cursorize:
-------------------------------------------------

-- | Used to inline variable bindings while retaining their (former) name and type.
pattern NamedVal :: forall t t1 (t2 :: * -> * -> *).
                    Var -> t -> L (PreExp t2 t1 t) -> PreExp t2 t1 t
pattern NamedVal vr ty e <- LetE (vr,[],ty,e)
                                  (L _ (VarE (Var "NAMED_VAL_PATTERN_SYN")))
  where NamedVal vr ty e = LetE (vr,[],ty,e)
                                    (L NoLoc (VarE "NAMED_VAL_PATTERN_SYN"))


-- For use after cursorize:
--------------------------------------------------------------------------------

pattern NewBuffer :: forall t t1 (t2 :: * -> * -> *). PreExp t2 t1 t
pattern NewBuffer <- AppE (Var "NewBuffer") [] (L _ (MkProdE []))
  where NewBuffer = AppE "NewBuffer" [] (L NoLoc (MkProdE []))

-- | output buffer space that is known not to escape the current function.
pattern ScopedBuffer :: forall t t1 (t2 :: * -> * -> *). PreExp t2 t1 t
pattern ScopedBuffer <- AppE (Var "ScopedBuffer") [] (L _ (MkProdE []))
  where ScopedBuffer = AppE "ScopedBuffer" [] (L NoLoc (MkProdE []))

-- | Tag writing is still modeled by DataConE.
pattern WriteInt :: forall t t1 (t2 :: * -> * -> *).
                    Var -> L (PreExp t2 t1 t) -> PreExp t2 t1 t
pattern WriteInt v e <- AppE (Var "WriteInt") [] (L _ (MkProdE [L _ (VarE v), e]))
  where WriteInt v e = AppE "WriteInt" [] (L NoLoc (MkProdE [L NoLoc (VarE v), e]))

-- | One cursor in, (int,cursor') output.
pattern ReadInt :: forall t t1 (t2 :: * -> * -> *). Var -> PreExp t2 t1 t
pattern ReadInt v <- AppE (Var "ReadInt") [] (L _ (VarE v))
  where ReadInt v = AppE "ReadInt" [] (L NoLoc (VarE v))

-- | Add a constant offset to a cursor variable.
pattern AddCursor :: forall t t1 (t2 :: * -> * -> *).
                     Var -> Int -> PreExp t2 t1 t
pattern AddCursor v i <- AppE (Var "AddCursor") []
                               (L _ (MkProdE [L _ (VarE v), L _ (LitE i)]))
  where AddCursor v i = AppE "AddCursor" []
                                 (L NoLoc (MkProdE [L NoLoc (VarE v),
                                                    L NoLoc (LitE i)]))


-- | A predicate to check if the form is part of the extended "L2.5" language.
isExtendedPattern :: PreExp l e d -> Bool
isExtendedPattern e =
  case e of
    NewBuffer{}    -> True
    ScopedBuffer{} -> True
    ReadInt{}      -> True
    WriteInt{}     -> True
    AddCursor{}    -> True
    _              -> False


-- Initial type environments
--------------------------------------------------------------------------------

dummyCursorTy :: L1.Ty1
dummyCursorTy = CursorTy dummyLRM

-- | Return type for a primitive operation.
primRetTy :: Prim -> L1.Ty1
primRetTy p =
  case p of
    AddP -> IntTy
    SubP -> IntTy
    MulP -> IntTy
    Gensym -> SymTy
    EqSymP  -> BoolTy
    EqIntP  -> BoolTy
    MkTrue  -> BoolTy
    MkFalse -> BoolTy
    MkNullCursor -> dummyCursorTy
    SizeParam -> IntTy
    DictHasKeyP _ -> BoolTy
    DictEmptyP ty -> SymDictTy ty
    DictInsertP ty -> SymDictTy ty
    DictLookupP ty -> ty
    (ErrorP _ ty) -> ty
    ReadPackedFile _ _ ty -> ty



-- | A type environment listing the types of built-in functions.
--
--   Using this table represents a policy decision.  Specifically,
--   that a type checker is relying on the fact that all the L2.5
--   patterns above are valid AppE forms.  The alternative is for that
--   typechecker to match against these patterns before the AppE
--   cases.
--
builtinTEnv :: M.Map Var (ArrowTy L1.Ty1)
builtinTEnv = undefined

includeBuiltins :: Env2 (UrTy ()) -> Env2 (UrTy ())
includeBuiltins (Env2 _ _) = undefined


-- Example
--------------------------------------------------------------------------------

-- | Our canonical simple example, written in this IR.
add1Prog :: Prog
add1Prog = withAdd1Prog Nothing

-- | Supply a main expression to run with add1 defined.
withAdd1Prog :: Maybe (L Exp2,Ty2) -> Prog
withAdd1Prog mainExp =
    let ddfs = ddtree
        funs = (M.fromList [("add1",exadd1)])
    in Prog ddfs funs mainExp
 where
  ddtree :: DDefs Ty2
  ddtree = (fromListDD [DDef "Tree"
                                [ ("Leaf",[(False,IntTy)])
                                , ("Node",[(False,PackedTy "Tree" "l")
                                          ,(False,PackedTy "Tree" "l")])]])

  exadd1 :: FunDef
  exadd1 = FunDef "add1" exadd1ty "tr" exadd1bod

  exadd1ty :: ArrowTy Ty2
  exadd1ty = (ArrowTy
              [LRM "lin" (VarR "r1") Input, LRM "lout" (VarR "r1") Output]
              (PackedTy "tree" "lin")
              (S.fromList [Traverse "lin"])
              (PackedTy "tree" "lout")
              [EndOf $ LRM "lin" (VarR "r1") Input])

  exadd1bod :: L Exp2
  exadd1bod =
      L NoLoc $ CaseE (L NoLoc $ VarE "tr") $
        [ ("Leaf", [("n","l0")], L NoLoc $
                                 LetE ("v",[],IntTy,L NoLoc $ PrimAppE L1.AddP
                                                    [L NoLoc $ VarE "n",
                                                     L NoLoc $ LitE 1])
                                 (L NoLoc $ VarE "v"))
        , ("Node", [("x","l1"),("y","l2")],
           L NoLoc $ Ext $ LetLocE "lout1" (AfterConstantLE 1 "lout") $
           L NoLoc $ LetE ("x1",[],PackedTy "Tree" "lout1",
                           L NoLoc $ AppE "add1" ["l1","lout1"] $
                           L NoLoc $ VarE "x") $
           L NoLoc $ Ext $ LetLocE "lout2" (AfterVariableLE "x1" "lout1") $
           L NoLoc $ LetE ("y1",[],PackedTy "Tree" "lout2",
                           L NoLoc $ AppE "add1" ["l2","lout2"] $
                           L NoLoc $ VarE "y") $
           L NoLoc $ LetE ("z",[],PackedTy "Tree" "lout",
                    L NoLoc $ DataConE "lout" "Node"
                    [ L NoLoc $ VarE "x1" , L NoLoc $ VarE "y1"]) $
           L NoLoc $ VarE "z")
        ]
