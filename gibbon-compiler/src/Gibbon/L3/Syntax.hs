{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | An intermediate language which makes cursors explicit

module Gibbon.L3.Syntax
  (
    -- * Extended language
    E3Ext(..), Prog3, DDef3, DDefs3, FunDef3, FunDefs3 , Exp3, Ty3
  , Scalar(..), mkScalar, scalarToTy

    -- * Functions
  , eraseLocMarkers, mapMExprs, cursorizeTy, toL3Prim, updateAvailVars

  , module Gibbon.Language
  )
where

import Control.DeepSeq
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Text.PrettyPrint.GenericPretty

import           Gibbon.Common
-- import qualified Gibbon.L2.Syntax               as L2
import           Gibbon.Language                hiding (mapMExprs)
import qualified Gibbon.NewL2.Syntax as L2


-------------------------------------------------------------------------------- 

type Prog3 = Prog Var Exp3

type DDef3  = DDef Ty3
type DDefs3 = DDefs Ty3

type FunDefs3 = FunDefs Var Exp3

type FunDef3 = FunDef Var Exp3

-- GHC uses the instance defined for L1.Ty1
-- instance FunctionTy Ty3 where

type Exp3 = PreExp E3Ext () Ty3

type Ty3 = UrTy ()

--------------------------------------------------------------------------------

-- | The extension that turns L1 into L3.
data E3Ext loc dec =
    ReadScalar  Scalar Var                        -- ^ One cursor in, (int, cursor') out
  | WriteScalar Scalar Var (PreExp E3Ext loc dec) -- ^ Write int at cursor, and return a cursor
  | ReadTag Var                            -- ^ One cursor in, (tag,cursor) out
  | WriteTag DataCon Var                   -- ^ Write Tag at Cursor, and return a cursor
  | TagCursor Var Var                      -- ^ Create a tagged cursor
  | WriteTaggedCursor Var (PreExp E3Ext loc dec) -- ^ Write a tagged cursor
  | ReadTaggedCursor Var                   -- ^ Reads and returns a tagged cursor at Var
  | ReadCursor Var                         -- ^ Reads and returns the cursor at Var
  | WriteCursor Var (PreExp E3Ext loc dec) -- ^ Write a cursor, and return a cursor
  | ReadList Var dec                       -- ^ Read a pointer to a linked list
  | WriteList Var (PreExp E3Ext loc dec) dec       -- ^ Write a pointer to a linked list
  | ReadVector Var dec                             -- ^ Read a pointer to a vector
  | WriteVector Var (PreExp E3Ext loc dec) dec     -- ^ Write a pointer to a vector
  | MakeCursorArray Int [Var] -- ^ Make a Cursor Array from a list of Cursors. Returns a new variable for Cursor Array.
  | IndexCursorArray Var Int                       -- ^ Index into a Cursor Array 
  | AddCursor Var (PreExp E3Ext loc dec)           -- ^ Add a constant offset to a cursor variable
  | CastPtr Var dec                                -- ^ Cast a pointer to the specified type
  | SubPtr Var Var                                 -- ^ Pointer subtraction
  | NewBuffer L2.Multiplicity         -- ^ Create a new buffer, and return a cursor
  | ScopedBuffer L2.Multiplicity      -- ^ Create a temporary scoped buffer, and return a cursor
  | NewParBuffer L2.Multiplicity         -- ^ Create a new buffer for parallel allocations, and return a cursor
  | ScopedParBuffer L2.Multiplicity      -- ^ Create a temporary scoped buffer for parallel allocations, and return a cursor
  | EndOfBuffer L2.Multiplicity
  | MMapFileSize Var
  | SizeOfPacked Var Var           -- ^ Takes in start and end cursors, and returns an Int
                                   --   we'll probably represent (sizeof x) as (end_x - start_x) / INT
  | SizeOfScalar Var               -- ^ sizeof(var)
  | BoundsCheck Int Var Var        -- ^ Bytes required, region, write cursor
  | IndirectionBarrier TyCon (Var,Var,Var,Var)
    -- ^ Do one of the following:
    -- (1) If it's a old-to-young indirection, record it in the remembered set.
    -- (2) Otherwise, bump the refcount and update the outset.
  | BumpArenaRefCount Var Var -- ^ Given an arena and end-of-region ptr, add a
                                    --   reference from the arena to the region
  | NullCursor                      -- ^ Constant null cursor value (hack?).
                                    --   Used for dict lookup, which returns a packed value but
                                    --   no end witness.
  | RetE [(PreExp E3Ext loc dec)]   -- ^ Analogous to L2's RetE.
  | GetCilkWorkerNum                -- ^ Translates to  __cilkrts_get_worker_number().
  | LetAvail [Var] (PreExp E3Ext loc dec) -- ^ These variables are available to use before the join point
  | AllocateTagHere Var TyCon  -- ^ Analogous to L2's extension.
  | AllocateScalarsHere Var    -- ^ Analogous to L2's extension.
  | StartTagAllocation Var     -- ^ Marks the beginning of tag allocation.
  | EndTagAllocation Var       -- ^ Marks the end of tag allocation.
  | StartScalarsAllocation Var -- ^ Marks the beginning of scalar allocation.
  | EndScalarsAllocation Var   -- ^ Marks the end of scalar allocation.
  | SSPush SSModality Var Var TyCon
  | SSPop SSModality Var Var
  | Assert (PreExp E3Ext loc dec) -- ^ Translates to assert statements in C.
    -- ^ Analogous to L2's extensions.
  deriving (Show, Ord, Eq, Read, Generic, NFData)

instance FreeVars (E3Ext l d) where
  gFreeVars  e =
    case e of
      ReadScalar _  v     -> S.singleton v
      WriteScalar _ v ex  -> S.insert v (gFreeVars ex)
      ReadTag v      -> S.singleton v
      WriteTag _ v   -> S.singleton v
      TagCursor a b      -> S.fromList [a,b]
      ReadTaggedCursor v -> S.singleton v
      WriteTaggedCursor v ex -> S.insert v (gFreeVars ex)
      ReadCursor v       -> S.singleton v
      WriteCursor c ex   -> S.insert c (gFreeVars ex)
      ReadList v _       -> S.singleton v
      WriteList c ex  _  -> S.insert c (gFreeVars ex)
      AddCursor v ex -> S.insert v (gFreeVars ex)
      SubPtr v w     -> S.fromList [v, w]
      NewBuffer{}    -> S.empty
      NewParBuffer{}     -> S.empty
      ScopedBuffer{}     -> S.empty
      ScopedParBuffer{}  -> S.empty
      EndOfBuffer{}      -> S.empty
      MMapFileSize v     -> S.singleton v
      SizeOfPacked c1 c2 -> S.fromList [c1, c2]
      SizeOfScalar v     -> S.singleton v
      BoundsCheck{}      -> S.empty
      IndirectionBarrier _tycon (l1,r1,l2,r2) -> S.fromList [l1,r1,l2,r2]
      NullCursor         -> S.empty
      BumpArenaRefCount v w -> S.fromList [v, w]
      RetE ls -> S.unions (L.map gFreeVars ls)
      GetCilkWorkerNum   -> S.empty
      LetAvail ls b      -> (S.fromList ls) `S.union` gFreeVars b
      ReadVector{}  -> error "gFreeVars: ReadVector"
      WriteVector{} -> error "gFreeVars: WriteVector"
      AllocateTagHere v _ -> S.singleton v
      AllocateScalarsHere v -> S.singleton v
      StartTagAllocation v -> S.singleton v
      EndTagAllocation v -> S.singleton v
      StartScalarsAllocation v -> S.singleton v
      EndScalarsAllocation v -> S.singleton v
      SSPush _ a b _ -> S.fromList [a,b]
      SSPop _ a b -> S.fromList [a,b]
      Assert a -> gFreeVars a


instance (Out l, Out d, Show l, Show d) => Expression (E3Ext l d) where
  type LocOf (E3Ext l d) = l
  type TyOf  (E3Ext l d) = UrTy l
  isTrivial _ = False

instance (Out l, Show l, Typeable (PreExp E3Ext l (UrTy l))) => Typeable (E3Ext l (UrTy l)) where
    gRecoverType _ddfs _env2 NullCursor = CursorTy
    gRecoverType ddfs env2 (RetE ls)    = ProdTy $ L.map (gRecoverType ddfs env2) ls
    gRecoverType _ _ _ = error "L3.gRecoverType"


    gRecoverTypeLoc _ddfs _env2 NullCursor = CursorTy
    gRecoverTypeLoc ddfs env2 (RetE ls)    = ProdTy $ L.map (gRecoverTypeLoc ddfs env2) ls
    gRecoverTypeLoc _ _ _ = error "L3.gRecoverTypeLoc"

instance (Show l, Out l) => Flattenable (E3Ext l (UrTy l)) where
    gFlattenGatherBinds _ddfs _env ex = return ([], ex)
    gFlattenExp _ddfs _env ex = return ex

instance HasSimplifiableExt E3Ext l d => SimplifiableExt (PreExp E3Ext l d) (E3Ext l d) where
  gInlineTrivExt _ _ = error $ "InlineTriv is not a safe operation to perform on L3." ++
                               " A lot of L3 extensions can only use values" ++
                               " via variable references. So those variables" ++
                               " should *not* be inlined." ++
                               " Running copy-propogation should be OK."


instance HasSubstitutableExt E3Ext l d => SubstitutableExt (PreExp E3Ext l d) (E3Ext l d) where
  gSubstExt old new ext =
    case ext of
      WriteScalar s v bod  -> WriteScalar s v (gSubst old new bod)
      WriteCursor v bod    -> WriteCursor v (gSubst old new bod)
      AddCursor v bod      -> AddCursor v (gSubst old new bod)
      SubPtr v w           -> SubPtr v w
      LetAvail ls bod      -> LetAvail ls (gSubst old new bod)
      _ -> ext

  gSubstEExt old new ext =
    case ext of
      WriteScalar s v bod    -> WriteScalar s v (gSubstE old new bod)
      WriteCursor v bod -> WriteCursor v (gSubstE old new bod)
      AddCursor v bod   -> AddCursor v (gSubstE old new bod)
      SubPtr v w        -> SubPtr v w
      LetAvail ls b     -> LetAvail ls (gSubstE old new b)
      _ -> ext

instance HasRenamable E3Ext l d => Renamable (E3Ext l d) where
  gRename env ext =
    case ext of
      ReadScalar s v     -> ReadScalar s (go v)
      WriteScalar s v bod-> WriteScalar s (go v) (go bod)
      TagCursor a b      -> TagCursor (go a) (go b)
      ReadTaggedCursor v -> ReadTaggedCursor (go v)
      WriteTaggedCursor v bod -> WriteTaggedCursor (go v) (go bod)
      ReadCursor v       -> ReadCursor (go v)
      WriteCursor v bod  -> WriteCursor (go v) (go bod)
      ReadList v el_ty      -> ReadList (go v) el_ty
      WriteList v bod el_ty -> WriteList (go v) (go bod) el_ty
      ReadVector v el_ty      -> ReadVector (go v) el_ty
      WriteVector v bod el_ty -> WriteVector (go v) (go bod) el_ty
      ReadTag v          -> ReadTag (go v)
      WriteTag dcon v    -> WriteTag dcon (go v)
      AddCursor v bod    -> AddCursor (go v) (go bod)
      SubPtr v w         -> SubPtr (go v) (go w)
      NewBuffer{}        -> ext
      ScopedBuffer{}     -> ext
      NewParBuffer{}     -> ext
      ScopedParBuffer{}  -> ext
      EndOfBuffer{}      -> ext
      MMapFileSize v     -> MMapFileSize (go v)
      SizeOfPacked a b   -> SizeOfPacked (go a) (go b)
      SizeOfScalar v     -> SizeOfScalar (go v)
      BoundsCheck i a b  -> BoundsCheck i (go a) (go b)
      IndirectionBarrier tycon (a,b,c,d) ->
        IndirectionBarrier tycon (go a, go b, go c, go d)
      BumpArenaRefCount v w -> BumpArenaRefCount (go v) (go w)
      NullCursor         -> ext
      RetE ls            -> RetE (L.map go ls)
      GetCilkWorkerNum   -> GetCilkWorkerNum
      LetAvail ls b      -> LetAvail (L.map go ls) (go b)
      AllocateTagHere v tycon -> AllocateTagHere (go v) tycon
      AllocateScalarsHere v  -> AllocateScalarsHere (go v)
      StartTagAllocation v -> StartTagAllocation (go v)
      EndTagAllocation v -> EndTagAllocation (go v)
      StartScalarsAllocation v -> StartScalarsAllocation (go v)
      EndScalarsAllocation v -> EndScalarsAllocation (go v)
      SSPush a b c d -> SSPush a (go b) (go c) d
      SSPop a b c -> SSPop a (go b) (go c)
      Assert e -> Assert (go e)
    where
      go :: forall a. Renamable a => a -> a
      go = gRename env

data Scalar = IntS | CharS | FloatS | SymS | BoolS
  deriving (Show, Ord, Eq, Read, Generic, NFData, Out)

mkScalar :: Out a => UrTy a -> Scalar
mkScalar IntTy  = IntS
mkScalar CharTy = CharS
mkScalar FloatTy= FloatS
mkScalar SymTy  = SymS
mkScalar BoolTy = BoolS
mkScalar ty = error $ "mkScalar: Not a scalar type: " ++ sdoc ty

scalarToTy :: Scalar -> UrTy a
scalarToTy IntS  = IntTy
scalarToTy CharS = CharTy
scalarToTy FloatS= FloatTy
scalarToTy SymS  = SymTy
scalarToTy BoolS = BoolTy


-----------------------------------------------------------------------------------------
-- Do this manually to get prettier formatting: (Issue #90)

instance (Out l, Out d) => Out (E3Ext l d)

-----------------------------------------------------------------------------------------

-- | Erase LocVar markers from the data definition
eraseLocMarkers :: DDef L2.Ty2 -> DDef Ty3
eraseLocMarkers (DDef tyargs tyname ls) = DDef tyargs tyname $ L.map go ls
  where go :: (DataCon,[(IsBoxed,L2.Ty2)]) -> (DataCon,[(IsBoxed,Ty3)])
        go (dcon,ls') = (dcon, L.map (\(b,ty) -> (b,L2.stripTyLocs (L2.unTy2 ty))) ls')

cursorizeTy :: UrTy LocVar -> UrTy b
cursorizeTy ty =
  case ty of
    IntTy     -> IntTy
    CharTy    -> CharTy
    FloatTy   -> FloatTy
    SymTy     -> SymTy
    BoolTy    -> BoolTy
    ProdTy ls -> ProdTy $ L.map cursorizeTy ls
    SymDictTy v _ -> SymDictTy v CursorTy
    PDictTy k v   -> PDictTy (cursorizeTy k) (cursorizeTy v)
    PackedTy _ l    -> case l of 
                           Single _ -> ProdTy [CursorTy, CursorTy]
			   SoA _ flds -> ProdTy [CursorArrayTy (1 + length flds), CursorArrayTy (1 + length flds)]
    VectorTy el_ty' -> VectorTy $ cursorizeTy el_ty'
    ListTy el_ty'   -> ListTy $ cursorizeTy el_ty'
    PtrTy    -> PtrTy
    CursorTy -> CursorTy
    ArenaTy  -> ArenaTy
    SymSetTy -> SymSetTy
    SymHashTy-> SymHashTy
    IntHashTy-> IntHashTy

-- | Map exprs with an initial type environment:
-- Exactly the same function that was in L2 before
mapMExprs :: Monad m => (Env2 Var Ty3 -> Exp3 -> m Exp3) -> Prog3 -> m Prog3
mapMExprs fn (Prog ddfs fundefs mainExp) =
  Prog ddfs <$>
    (mapM (\f@FunDef{funArgs,funTy,funBody} ->
              let env = Env2 (M.fromList $ zip funArgs (fst funTy)) funEnv
              in do
                bod' <- fn env funBody
                return $ f { funBody =  bod' })
     fundefs)
    <*>
    (mapM (\ (e,t) -> (,t) <$> fn (Env2 M.empty funEnv) e) mainExp)
  where funEnv = M.map funTy fundefs

toL3Prim :: Prim L2.Ty2 -> Prim Ty3
toL3Prim (DictEmptyP  _ty) = DictEmptyP  CursorTy
toL3Prim (DictInsertP _ty) = DictInsertP CursorTy
toL3Prim (DictLookupP _ty) = DictLookupP CursorTy
toL3Prim (DictHasKeyP _ty) = DictHasKeyP CursorTy
toL3Prim pr = fmap (L2.stripTyLocs . L2.unTy2) pr

-- |
updateAvailVars :: [Var] -> [Var] -> Exp3 -> Exp3
updateAvailVars froms tos ex =
  case ex of
    VarE v          -> VarE v
    LitE _          -> ex
    CharE _         -> ex
    FloatE{}        -> ex
    LitSymE _       -> ex
    AppE v loc ls   -> AppE v loc (map go ls)
    PrimAppE p ls   -> PrimAppE p $ L.map go ls
    LetE (v,loc,t,rhs) bod -> LetE (v,loc,t,go rhs) (go bod)
    ProjE i e         -> ProjE i (go e)
    CaseE e ls        -> CaseE (go e) (L.map (\(c,vs,er) -> (c,vs,go er)) ls)
    MkProdE ls        -> MkProdE $ L.map go ls
    DataConE loc k ls -> DataConE loc k $ L.map go ls
    TimeIt e t b      -> TimeIt (go e) t b
    IfE a b c         -> IfE (go a) (go b) (go c)
    SpawnE v loc ls   -> SpawnE v loc (map go ls)
    SyncE             -> SyncE
    WithArenaE v e    -> WithArenaE v (go e)
    MapE (v,t,rhs) bod -> MapE (v,t, go rhs) (go bod)
    FoldE (v1,t1,r1) (v2,t2,r2) bod ->
      FoldE (v1,t1,go r1) (v2,t2,go r2) (go bod)
    Ext ext ->
      case ext of
        LetAvail vs bod ->
          let n o = if o `elem` froms then tos else [o]
              vs' = foldr (\v acc -> n v ++ acc) [] vs
          in Ext $ LetAvail vs' (go bod)
        _ -> ex
  where
    go = updateAvailVars froms tos
