{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

-- | An intermediate language with an effect system that captures traversals.

module Gibbon.L2.Syntax
    -- * Extended language L2 with location types.
  ( E2Ext(..)
  , Prog2
  , DDefs2
  , DDef2
  , FunDef2
  , FunDefs2
  , Exp2
  , E2
  , Ty2
  , Effect(..)
  , ArrowTy2(..)
  , LocRet(..)
  , LocExp
  , RegExp
  , PreLocExp(..)
  , PreRegExp(..)

-- * Regions and locations
  , LocVar
  , RegVar
  , Region(..)
  --, ExtendedRegion(..)
  , Modality(..)
  , LRM(..)
  , dummyLRM
  , Multiplicity(..)
  , RegionSize(..)
  , RegionType(..)
  , regionToVar
  , getAllRegions

-- * Operations on types
  , allLocVars
  , inLocVars
  , outLocVars
  , outRegVars
  , inRegVars
  , allRegVars
  , substLoc
  , substLocs
  , substEff
  , substEffs
  , extendPatternMatchEnv
  , extendPatternMatchEnvLocVar
  , locsInTy
  , dummyTyLocs
  , allFreeVars
  , allFreeVars'
  , freeLocVars
  , singleLocVar
  , freeVars
  , freeRegVars
  , fromRegVarToLocVar
  , fromSingleRegVarToVar
  , fromLocVarToRegVar

-- * Other helpers
  , revertToL1
  , occurs
  , mapPacked
  , constPacked
  , depList
  , changeAppToSpawn
  , module Gibbon.Language
  ) where

import           Control.DeepSeq
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Safe as Sf

import           GHC.Stack (HasCallStack)
import           Text.PrettyPrint.GenericPretty

import           Gibbon.Common
import           Gibbon.Language
import           Text.PrettyPrint.HughesPJ
import           Gibbon.L1.Syntax hiding (AddFixed, StartOfPkdCursor)
import qualified Gibbon.L1.Syntax as L1

--------------------------------------------------------------------------------

type Prog2    = Prog Var Exp2
type DDef2    = DDef Ty2
type DDefs2   = DDefs Ty2
type FunDef2  = FunDef Var Exp2
type FunDefs2 = FunDefs Var Exp2

-- | Function types know about locations and traversal effects.
instance FunctionTy Ty2 where
  type ArrowTy Ty2 = ArrowTy2 Ty2
  inTys = arrIns
  outTy = arrOut

-- | Extended expressions, L2.
--
--   By adding a `LocVar` decoration, all data constructors,
--   applications, and bindings gain a location annotation.
type Exp2 = E2 LocVar Ty2

-- | L1 Types extended with abstract Locations.
type Ty2 = UrTy LocVar

--------------------------------------------------------------------------------

-- | Shorthand for recursions.
type E2 l d = PreExp E2Ext l d

data RegionSize = BoundedSize Int | Undefined
  deriving (Eq, Read, Show, Generic, NFData, Out)
data RegionType = IndirectionFree | RightwardLocalIndirections | LocalIndirections | NoSharing
  deriving (Eq, Ord, Read, Show, Generic, NFData, Out)


-- | 'Undefined' is at the top of this lattice.
instance Ord RegionSize where
  (<=) (BoundedSize sz1) (BoundedSize sz2) = sz1 <= sz2
  (<=) Undefined         (BoundedSize{})   = False
  (<=) (BoundedSize{})   Undefined         = True
  (<=) Undefined         Undefined         = True

instance Semigroup RegionType where
  -- IndirectionFree < RightwardLocalIndirections < LocalIndirections < NoSharing
  (<>) IndirectionFree            v                          = v
  (<>) v                          IndirectionFree            = v
  (<>) RightwardLocalIndirections v                          = v
  (<>) v                          RightwardLocalIndirections = v
  (<>) LocalIndirections          v                          = v
  (<>) v                          LocalIndirections          = v
  (<>) NoSharing                  v                          = v

instance Semigroup RegionSize where
  (<>) (BoundedSize sz1) (BoundedSize sz2) = BoundedSize (sz1 + sz2)
  (<>) Undefined _         = Undefined
  (<>) _         Undefined = Undefined

instance Monoid RegionSize where
  mempty = BoundedSize 0


-- | The extension that turns L1 into L2.
data E2Ext loc dec
  = LetRegionE    Region RegionSize (Maybe RegionType) (E2 loc dec) -- ^ Allocate a new region.
  | LetParRegionE Region RegionSize (Maybe RegionType) (E2 loc dec) -- ^ Allocate a new region for parallel allocations.
  | LetLocE LocVar (PreLocExp loc) (E2 loc dec) -- ^ Bind a new location.
  | LetRegE RegVar (PreRegExp loc) (E2 loc dec) -- ^ Bind a new region.
  -- Commented this out since it is not very ideal. 
  -- | LetSoALocE LocVar (E2 loc dec) -- ^ Bind a new SoA loc
  | RetE [loc] Var          -- ^ Return a value together with extra loc values.
  | FromEndE loc            -- ^ Bind a location from an EndOf location (for RouteEnds and after).
  | BoundsCheck Int -- Bytes required
                loc -- Region
                loc -- Write cursor
  -- | BoundsCheckVector [Int] [loc] [loc] -- BoundsCheck on vector of regions/loc Probably not needed yet. 
  | AddFixed Var Int
  | IndirectionE TyCon     -- Type of the data pointed to by this indirection.
                 DataCon   -- Constructor for an indirection in this type.
                 (loc,loc) -- Pointer.
                 (loc,loc) -- Pointee (the thing that the pointer points to).
                 (E2 loc dec) -- If this indirection was added to get rid
                              -- of a copy_Foo call, we keep the fn call
                              -- around in case we want to go back to it.
                              -- E.g. when reverting from L2 to L1.
    -- ^ A indirection node.

  | StartOfPkdCursor Var -- Cursor to a packed value, created by AddRAN.

  | TagCursor Var Var    -- Create a tagged cursor.

  | GetCilkWorkerNum
    -- ^ Translates to  __cilkrts_get_worker_number().
  | LetAvail [Var] (E2 loc dec) -- ^ These variables are available to use before the join point.
  | AllocateTagHere LocVar TyCon
  | AllocateScalarsHere LocVar
    -- ^ A marker which tells subsequent a compiler pass where to
    -- move the tag and scalar field allocations so that they happen
    -- before any of the subsequent packed fields.
  | SSPush SSModality LocVar LocVar TyCon
  | SSPop SSModality LocVar LocVar
    -- ^ Spill and restore from the shadow-stack.
  deriving (Show, Ord, Eq, Read, Generic, NFData)

-- | Define a location in terms of a different location.
data PreLocExp loc = StartOfRegionLE Region
                   | AfterConstantLE  Int  -- Number of bytes after. (In case of an SoA loc, this is the offset into the data constructor buffer)
                                      loc  -- Location which this location is offset from.

                   | AfterVariableLE  Var  -- Name of variable v. This loc is size(v) bytes after.
                                      loc  -- Location which this location is offset from.
                                      Bool -- Whether it's running in a stolen continuation i.e
                                           -- whether this should return an index in a fresh region or not.
                                           -- It's True by default and flipped by ParAlloc if required.
                   | InRegionLE Region
                   | FreeLE
                   | FromEndLE  loc
                   -- Helpers for SoA that may be required
                   | GenSoALoc loc [((DataCon, FieldIndex), loc)]
                   | GetDataConLocSoA loc -- Get the data constructor location from an SoA loc
                   | GetFieldLocSoA (DataCon, FieldIndex) loc -- Get the field location from the SoA loc
                   | AssignLE loc
                   -- Although this is available in infer locations constraints, i don't think its required in L2 AST.
                   -- | AfterVectorLE (PreLocExp loc) [PreLocExp loc] loc
                     -- Compute new SoA location from an old SoA location
                     -- Not sure this is fully needed 
								     -- (PreLocExp loc) -> expression for arithmetic on data constructor buffer 
								     -- [PreLocExp loc] -> expressions for arithmetic on each field location 
								     -- loc, store the old loc, why? -- capture more metadata, also style
  deriving (Read, Show, Eq, Ord, Functor, Generic, NFData)


data PreRegExp loc = GetDataConRegSoA loc
                  |  GetFieldRegSoA (DataCon, FieldIndex) loc
  deriving (Read, Show, Eq, Ord, Functor, Generic, NFData) 

type LocExp = PreLocExp LocVar

type RegExp = PreRegExp LocVar

-- | Locations (end-witnesses) returned from functions after RouteEnds.
data LocRet = EndOf LRM
              deriving (Read, Show, Eq, Ord, Generic, NFData)

instance FreeVars (E2Ext l d) where
  gFreeVars e =
    case e of
     LetRegionE _ _ _ bod   -> gFreeVars bod
     LetParRegionE _ _ _ bod   -> gFreeVars bod
     LetLocE _ rhs bod  -> (case rhs of
                              AfterVariableLE v _loc _ -> S.singleton v
                              _ -> S.empty)
                           `S.union`
                           gFreeVars bod
     StartOfPkdCursor cur -> S.singleton cur
     TagCursor a b      -> S.fromList [a,b]
     RetE _ vr          -> S.singleton vr
     FromEndE _         -> S.empty
     AddFixed vr _      -> S.singleton vr
     BoundsCheck{}      -> S.empty
     IndirectionE _ _ _ _ e -> gFreeVars e
     GetCilkWorkerNum   -> S.empty
     LetAvail vs bod    -> S.fromList vs `S.union` gFreeVars bod
     AllocateTagHere{}  -> S.empty
     AllocateScalarsHere{}  -> S.empty
     SSPush{} -> S.empty
     SSPop{} -> S.empty

instance FreeVars LocExp where
  gFreeVars e =
    case e of
      --AfterConstantLE _ loc   -> S.singleton $ unwrapLocVar loc
      --AfterVariableLE v loc _ -> S.fromList [v, unwrapLocVar loc]
      -- All the locations inside an SoA loc are also free. 
      --GetDataConLocSoA loc -> S.fromList $ varsInLocVar loc
      --GetFieldLocSoA _ loc -> S.fromList $ varsInLocVar loc
      --GenSoALoc loc flocs -> S.fromList (varsInLocVar loc) `S.union` (S.fromList $ L.concatMap (\(_, loc) -> varsInLocVar loc) flocs)
      AfterVariableLE v loc _ -> S.fromList [v]
      _ -> S.empty

instance (Out l, Out d, Show l, Show d) => Expression (E2Ext l d) where
  type LocOf (E2Ext l d) = l
  type TyOf (E2Ext l d)  = d
  isTrivial e =
    case e of
      LetRegionE{} -> False
      LetParRegionE{} -> False
      LetLocE{}    -> False
      StartOfPkdCursor{} -> False
      TagCursor{} -> False
      RetE{}       -> False -- Umm... this one could be potentially.
      FromEndE{}   -> True
      AddFixed{}     -> True
      BoundsCheck{}  -> False
      IndirectionE{} -> False
      GetCilkWorkerNum-> False
      LetAvail{}      -> False
      AllocateTagHere{} -> False
      AllocateScalarsHere{} -> False
      SSPush{} -> False
      SSPop{} -> False

instance (Out l, Show l, Typeable (E2 l (UrTy l))) => Typeable (E2Ext l (UrTy l)) where
  gRecoverType ddfs env2 ex =
    case ex of
      LetRegionE _r _ _ bod    -> gRecoverType ddfs env2 bod
      LetParRegionE _r _ _ bod -> gRecoverType ddfs env2 bod
      LetLocE _l _rhs bod -> gRecoverType ddfs env2 bod
      StartOfPkdCursor{}  -> CursorTy
      TagCursor{}         -> CursorTy
      RetE _loc var       -> case M.lookup var (vEnv env2) of
                               Just ty -> ty
                               Nothing -> error $ "gRecoverType: unbound variable " ++ sdoc var
      FromEndE _loc       -> error "Shouldn't enconter FromEndE in tail position"
      BoundsCheck{}       -> error "Shouldn't enconter BoundsCheck in tail position"
      IndirectionE tycon _ _ (to,_) _ -> PackedTy tycon to
      AddFixed{}          -> error "Shouldn't enconter AddFixed in tail position"
      GetCilkWorkerNum    -> IntTy
      LetAvail _ bod -> gRecoverType ddfs env2 bod
      AllocateTagHere{} -> ProdTy []
      AllocateScalarsHere{} -> ProdTy []
      SSPush{} -> ProdTy []
      SSPop{} -> ProdTy []

  gRecoverTypeLoc ddfs env2 ex =
    case ex of
      LetRegionE _r _ _ bod    -> gRecoverTypeLoc ddfs env2 bod
      LetParRegionE _r _ _ bod -> gRecoverTypeLoc ddfs env2 bod
      LetLocE _l _rhs bod -> gRecoverTypeLoc ddfs env2 bod
      StartOfPkdCursor{}  -> CursorTy
      TagCursor{}         -> CursorTy
      RetE _loc var       -> case M.lookup (fromVarToFreeVarsTy var) (vEnv env2) of
                               Just ty -> ty
                               Nothing -> error $ "gRecoverTypeLoc: unbound variable " ++ sdoc var
      FromEndE _loc       -> error "Shouldn't enconter FromEndE in tail position"
      BoundsCheck{}       -> error "Shouldn't enconter BoundsCheck in tail position"
      IndirectionE tycon _ _ (to,_) _ -> PackedTy tycon to
      AddFixed{}          -> error "Shouldn't enconter AddFixed in tail position"
      GetCilkWorkerNum    -> IntTy
      LetAvail _ bod -> gRecoverTypeLoc ddfs env2 bod
      AllocateTagHere{} -> ProdTy []
      AllocateScalarsHere{} -> ProdTy []
      SSPush{} -> ProdTy []
      SSPop{} -> ProdTy []

instance (Typeable (E2Ext l d),
          Expression (E2Ext l d),
          Flattenable (E2 l d))
      => Flattenable (E2Ext l d) where

  gFlattenGatherBinds ddfs env ex =
      case ex of
          LetRegionE r sz ty bod -> do
                                (bnds,bod') <- go bod
                                return ([], LetRegionE r sz ty (flatLets bnds bod'))

          LetParRegionE r sz ty bod -> do
                                (bnds,bod') <- go bod
                                return ([], LetParRegionE r sz ty (flatLets bnds bod'))

          LetLocE l rhs bod -> do (bnds,bod') <- go bod
                                  return ([], LetLocE l rhs $ flatLets bnds bod')

          TagCursor{}-> return ([],ex)
          StartOfPkdCursor{} -> return ([],ex)
          RetE{}        -> return ([],ex)
          FromEndE{}    -> return ([],ex)
          AddFixed{}    -> return ([],ex)
          BoundsCheck{} -> return ([],ex)
          IndirectionE{}-> return ([],ex)
          GetCilkWorkerNum-> return ([],ex)
          LetAvail vs bod -> do (bnds,bod') <- go bod
                                return ([], LetAvail vs $ flatLets bnds bod')
          AllocateTagHere{} -> return ([],ex)
          AllocateScalarsHere{} -> return ([],ex)
          SSPush{} -> return ([],ex)
          SSPop{} -> return ([],ex)

    where go = gFlattenGatherBinds ddfs env

  gFlattenExp ddfs env ex = do (_b,e') <- gFlattenGatherBinds ddfs env ex
                               return e'
  

instance HasSimplifiableExt E2Ext l d => SimplifiableExt (PreExp E2Ext l d) (E2Ext l d) where
  gInlineTrivExt env ext =
    case ext of
      LetRegionE r sz ty bod   -> LetRegionE r sz ty (gInlineTrivExp env bod)
      LetParRegionE r sz ty bod -> LetParRegionE r sz ty (gInlineTrivExp env bod)
      LetLocE loc le bod -> LetLocE loc le (gInlineTrivExp env bod)
      TagCursor{} -> ext
      StartOfPkdCursor{} -> ext
      RetE{}         -> ext
      FromEndE{}     -> ext
      BoundsCheck{}  -> ext
      IndirectionE{} -> ext
      AddFixed{}     -> ext
      GetCilkWorkerNum-> ext
      LetAvail vs bod -> LetAvail vs (gInlineTrivExp env bod)
      AllocateTagHere{} -> ext
      AllocateScalarsHere{} -> ext
      SSPush{} -> ext
      SSPop{} -> ext


instance HasSubstitutableExt E2Ext l d => SubstitutableExt (PreExp E2Ext l d) (E2Ext l d) where
  gSubstExt old new ext =
    case ext of
      LetRegionE r sz ty bod -> LetRegionE r sz ty (gSubst old new bod)
      LetParRegionE r sz ty bod -> LetParRegionE r sz ty (gSubst old new bod)
      LetLocE l le bod -> LetLocE l le (gSubst old new bod)
      TagCursor{}   -> ext
      StartOfPkdCursor{} -> ext
      RetE{}           -> ext
      FromEndE{}       -> ext
      BoundsCheck{}    -> ext
      IndirectionE{}   -> ext
      AddFixed{}       -> ext
      GetCilkWorkerNum -> ext
      LetAvail vs bod  -> LetAvail vs (gSubst old new bod)
      AllocateTagHere{} -> ext
      AllocateScalarsHere{} -> ext
      SSPush{} -> ext
      SSPop{} -> ext

  gSubstEExt old new ext =
    case ext of
      LetRegionE r sz ty bod -> LetRegionE r sz ty (gSubstE old new bod)
      LetParRegionE r sz ty bod -> LetParRegionE r sz ty (gSubstE old new bod)
      LetLocE l le bod -> LetLocE l le (gSubstE old new bod)
      TagCursor{}   -> ext
      StartOfPkdCursor{} -> ext
      RetE{}           -> ext
      FromEndE{}       -> ext
      BoundsCheck{}    -> ext
      IndirectionE{}   -> ext
      AddFixed{}       -> ext
      GetCilkWorkerNum -> ext
      LetAvail vs bod  -> LetAvail vs (gSubstE old new bod)
      AllocateTagHere{} -> ext
      AllocateScalarsHere{} -> ext
      SSPush{} -> ext
      SSPop{} -> ext

instance HasRenamable E2Ext l d => Renamable (E2Ext l d) where
  gRename env ext =
    case ext of
      LetRegionE r sz ty bod -> LetRegionE r sz ty (gRename env bod)
      LetParRegionE r sz ty bod -> LetParRegionE r sz ty (gRename env bod)
      LetLocE l le bod -> LetLocE l le (gRename env bod)
      TagCursor a b -> TagCursor (gRename env a) (gRename env b)
      StartOfPkdCursor cur -> StartOfPkdCursor (gRename env cur)
      RetE{}           -> ext
      FromEndE{}       -> ext
      BoundsCheck{}    -> ext
      IndirectionE{}   -> ext
      AddFixed{}       -> ext
      GetCilkWorkerNum -> ext
      LetAvail vs bod  -> LetAvail vs (gRename env bod)
      AllocateTagHere{} -> ext
      AllocateScalarsHere{} -> ext
      SSPush{} -> ext
      SSPop{} -> ext

-- | Our type for functions grows to include effects, and explicit universal
-- quantification over location/region variables.
data ArrowTy2 ty2 = ArrowTy2
    { locVars :: [LRM]          -- ^ Universally-quantified location params.
                                -- Only these should be referenced in arrIn/arrOut.
    , arrIns  :: [ty2]          -- ^ Input type for the function.
    , arrEffs :: (S.Set Effect) -- ^ These are present-but-empty initially,
                                -- and the populated by InferEffects.
    , arrOut  :: ty2            -- ^ Output type for the function.
    , locRets :: [LocRet]       -- ^ L2B feature: multi-valued returns.
    , hasParallelism :: Bool        -- ^ Does this function have parallelism
    }
  deriving (Read, Show, Eq, Ord, Functor, Generic, NFData)

-- | The side-effect of evaluating a function.
data Effect = Traverse LocVar
              -- ^ The function, during its execution, traverses all
              -- of the value living at this location.
  deriving (Read,Show,Eq,Ord, Generic, NFData)

--------------------------------------------------------------------------------
--
-- See https://github.com/iu-parfunc/gibbon/issues/79 for more details
-- | Region variants (multiplicities)
data Multiplicity
    = Bounded Int -- ^ Contain a finite number of values and can be
                  --   stack-allocated.

    | Infinite    -- ^ Consist of a linked list of buffers, spread
                  --   throughout memory (though possible constrained
                  --   to 4GB regions). Writing into these regions requires
                  --   bounds-checking. The buffers can start very small
                  --   at the head of the list, but probably grow
                  --   geometrically in size, making the cost of traversing
                  --   all of them logarithmic.

    | BigInfinite -- ^ These regions are infinite, but also have the
                  --   expectation of containing many values. Thus we give
                  --   them large initial page sizes. This is also could be
                  --   the appropriate place to use mmap to grow the region
                  --   and to establish guard places.
  deriving (Read,Show,Eq,Ord,Generic)

instance Out Multiplicity where
  doc = text . show

instance NFData Multiplicity where
  rnf _ = ()

-- | An abstract region identifier.  This is used inside type signatures and elsewhere.
data Region = GlobR Var Multiplicity -- ^ A global region with lifetime equal to the
                                     --   whole program.
            | DynR Var Multiplicity  -- ^ A dynamic region that may be created or
                                     --   destroyed, tagged by an identifier.
            | VarR Var               -- ^ A region metavariable that can range over
                                     --   either global or dynamic regions.
            | MMapR Var              -- ^ A region that doesn't result in an (explicit)
                                     --   memory allocation. It merely ensures that there
                                     --   are no free locations in the program.

            | SoAR Region [((DataCon, FieldIndex), Region)]


  deriving (Read,Show,Eq,Ord, Generic)

{- 
   Why a new datatype? -- Well, For a location, we changed the exisiting datatype, i.e. LocVar.
   That's acceptable, because locations are just cursors into regions, they are an indices into 
   regions which don't necessarily signify an entity/allocated object. 

   A region encapsulates an allocated space and i'd like a region to represent a unit of space. 
   We can create more "complex" regions using that unit, for instance a SoA region. 
   In additon, it preserves statements like "is region A within region B?"
   It would be tough to formalize what is means by saying an SoA region exists within another region 
   (VarR x). Since an SoA region contains multiple regions.
-}
-- data ExtendedRegion = AoSR Region                                   -- ^ A simple "flat" region where the datatype 
--                                                                     --   will reside in an array of structure representation.
--                     | SoAR Region [((DataCon, FieldIndex), Region)] -- ^ A complex region representation for a datatype 
--                                                                     --   One "flat" buffer makes space for all the data constructors. 
--                                                                     --   In addition to a list containing a "flat" buffer for each 
--                                                                     --   field. The region can also be mapped to which data constructore 
--                                                                     --   and field tuple it belongs to. A structure of arrays representation. 
--       deriving (Read,Show,Eq,Ord, Generic)



instance Out Region
-- instance Out ExtendedRegion

instance NFData Region where
  rnf (GlobR v _) = rnf v
  rnf (DynR v _)  = rnf v
  rnf (VarR v)    = rnf v
  rnf (MMapR v)   = rnf v
  rnf (SoAR reg fieldRegs) = let 
                                regions = L.map (\(_, fregs) -> fregs) fieldRegs
                                regions' = L.map rnf regions  
                               in case regions' of 
                                        [] -> rnf reg 
                                        _ -> L.foldr (\r accum -> r `seq` accum) (rnf reg) regions' 


-- instance NFData ExtendedRegion where
--   rnf (AoSR reg) = rnf reg
--   rnf (SoAR reg fieldRegs) = let 
--                                regions = L.map (\(_, fregs) -> fregs) fieldRegs
--                                regions' = L.map rnf regions  
--                               in case regions' of 
--                                       [] -> rnf reg 
--                                       _ -> L.foldr (\r accum -> r `seq` accum) (rnf reg) regions' 
                                      


-- | The modality of locations and cursors: input/output, for reading
-- and writing, respectively.
data Modality = Input | Output
  deriving (Read,Show,Eq,Ord, Generic)
instance Out Modality
instance NFData Modality where
  rnf Input  = ()
  rnf Output = ()

-- | A location and region, together with modality.
data LRM = LRM { lrmLoc :: LocVar
               , lrmReg :: Region
               , lrmMode :: Modality }
  deriving (Read,Show,Eq,Ord, Generic)

instance Out LRM

instance NFData LRM where
  rnf (LRM a b c)  = rnf a `seq` rnf b `seq` rnf c

-- | A designated doesn't-really-exist-anywhere location.
dummyLRM :: LRM
dummyLRM = LRM (singleLocVar "l_dummy") (VarR "r_dummy") Input

regionToVar :: Region -> RegVar
regionToVar r = case r of
                  GlobR v _ -> SingleR v
                  DynR  v _ -> SingleR v
                  VarR  v   -> SingleR v
                  MMapR v   -> SingleR v
                  SoAR reg fieldRegs -> SoARv (regionToVar reg) (L.map (\(k, freg) -> (k, regionToVar freg)) fieldRegs)
                  _ -> error "L2/Syntax.hs: regionToVar: unexpected case."

getAllRegions :: Region -> [Region]
getAllRegions r = case r of 
                    SoAR reg fieldRegs -> [reg] ++ L.map (\(_, freg) -> freg) fieldRegs
                    _ -> [r]

-- | The 'gRecoverType' instance defined in Language.Syntax is incorrect for L2.
-- For the AppE case, it'll just return the type with with the function was
-- defined. However, we want the recovered type to have the locations actually
-- used at the callsites! For example,
--
--     add1 :: Tree @ a -> Tree @ b
--     add1 = _
--
--     ... (add1 [loc1, loc2] tr1) ..
--
-- in this case, we want the type of (add1 tr1) to be (Tree @ loc2)
-- and NOT (Tree @ b). We have to do something similar for variables bound by
-- a pattern match.
instance Typeable (PreExp E2Ext LocVar (UrTy LocVar)) where
  gRecoverType ddfs env2 ex =
    case ex of
      VarE v       -> M.findWithDefault (error $ "Cannot find type of variable " ++ show v ++ " in " ++ show (vEnv env2)) v (vEnv env2)
      LitE _       -> IntTy
      CharE{}      -> CharTy
      FloatE{}     -> FloatTy
      LitSymE _    -> SymTy
      AppE v locs _ -> let fnty  = fEnv env2 # v
                           outty = arrOut fnty
                           mp = M.fromList $ zip (allLocVars fnty) locs
                       in substLoc mp outty

      PrimAppE (DictInsertP ty) ((VarE v):_) -> SymDictTy (Just v) $ stripTyLocs ty
      PrimAppE (DictEmptyP  ty) ((VarE v):_) -> SymDictTy (Just v) $ stripTyLocs ty
      PrimAppE p _ -> primRetTy p

      LetE (v,_,t,_) e -> gRecoverType ddfs (extendVEnv v t env2) e
      IfE _ e _        -> gRecoverType ddfs env2 e
      MkProdE es       -> ProdTy $ L.map (gRecoverType ddfs env2) es
      DataConE loc c _ -> PackedTy (getTyOfDataCon ddfs c) loc
      TimeIt e _ _     -> gRecoverType ddfs env2 e
      MapE _ e         -> gRecoverType ddfs env2 e
      FoldE _ _ e      -> gRecoverType ddfs env2 e
      Ext ext          -> gRecoverType ddfs env2 ext
      ProjE i e ->
        case gRecoverType ddfs env2 e of
          (ProdTy tys) -> tys !! i
          oth -> error$ "typeExp: Cannot project fields from this type: "++show oth
                        ++"\nExpression:\n  "++ sdoc ex
                        ++"\nEnvironment:\n  "++sdoc (vEnv env2)
      SpawnE v locs _ -> let fnty  = fEnv env2 # v
                             outty = arrOut fnty
                             mp = M.fromList $ zip (allLocVars fnty) locs
                         in substLoc mp outty
      SyncE -> voidTy
      WithArenaE _v e -> gRecoverType ddfs env2 e
      CaseE _ mp ->
        let (c,vlocs,e) = Sf.headErr mp
            (vars,locs) = unzip vlocs
            env2' = extendPatternMatchEnv c ddfs vars locs env2
        in gRecoverType ddfs env2' e


  gRecoverTypeLoc ddfs env2 ex =
    case ex of
      VarE v       -> M.findWithDefault (error $ "Cannot find type of variable " ++ show v ++ " in " ++ show (vEnv env2)) (fromVarToFreeVarsTy v) (vEnv env2)
      LitE _       -> IntTy
      CharE{}      -> CharTy
      FloatE{}     -> FloatTy
      LitSymE _    -> SymTy
      AppE v locs _ -> let fnty  = fEnv env2 # (fromVarToFreeVarsTy v)
                           outty = arrOut fnty
                           mp = M.fromList $ zip (allLocVars fnty) locs
                       in substLoc mp outty

      PrimAppE (DictInsertP ty) ((VarE v):_) -> SymDictTy (Just v) $ stripTyLocs ty
      PrimAppE (DictEmptyP  ty) ((VarE v):_) -> SymDictTy (Just v) $ stripTyLocs ty
      PrimAppE p _ -> primRetTy p

      LetE (v,_,t,_) e -> gRecoverTypeLoc ddfs (extendVEnvLocVar (fromVarToFreeVarsTy v) t env2) e
      IfE _ e _        -> gRecoverTypeLoc ddfs env2 e
      MkProdE es       -> ProdTy $ L.map (gRecoverTypeLoc ddfs env2) es
      DataConE loc c _ -> PackedTy (getTyOfDataCon ddfs c) loc
      TimeIt e _ _     -> gRecoverTypeLoc ddfs env2 e
      MapE _ e         -> gRecoverTypeLoc ddfs env2 e
      FoldE _ _ e      -> gRecoverTypeLoc ddfs env2 e
      Ext ext          -> gRecoverTypeLoc ddfs env2 ext
      ProjE i e ->
        case gRecoverTypeLoc ddfs env2 e of
          (ProdTy tys) -> tys !! i
          oth -> error$ "typeExp: Cannot project fields from this type: "++show oth
                        ++"\nExpression:\n  "++ sdoc ex
                        ++"\nEnvironment:\n  "++sdoc (vEnv env2)
      SpawnE v locs _ -> let fnty  = fEnv env2 # (fromVarToFreeVarsTy v)
                             outty = arrOut fnty
                             mp = M.fromList $ zip (allLocVars fnty) locs
                         in substLoc mp outty
      SyncE -> voidTy
      WithArenaE _v e -> gRecoverTypeLoc ddfs env2 e
      CaseE _ mp ->
        let (c,vlocs,e) = Sf.headErr mp
            (vars,locs) = unzip vlocs
            env2' = extendPatternMatchEnvLocVar c ddfs vars locs env2
        in gRecoverTypeLoc ddfs env2' e


--------------------------------------------------------------------------------
-- Do this manually to get prettier formatting: (Issue #90)

instance Out (ArrowTy2 Ty2)

--instance Out (ArrowTy2 Ty2SoA)

instance Out Effect
instance Out a => Out (S.Set a) where
  docPrec n x = docPrec n (S.toList x)
  doc x = doc (S.toList x)
instance (Out l, Out d) => Out (E2Ext l d)
instance Out l => Out (PreLocExp l)
instance Out l => Out (PreRegExp l)
instance Out LocRet

-------------------------------------------------------------------------------

-- | Retrieve all LocVars from a fn type (Arrow)
allLocVars :: ArrowTy2 ty2 -> [LocVar]
allLocVars ty = L.map (\(LRM l _ _) -> l) (locVars ty)

inLocVars :: ArrowTy2 ty2 -> [LocVar]
inLocVars ty = L.map (\(LRM l _ _) -> l) $
               L.filter (\(LRM _ _ m) -> m == Input) (locVars ty)

outLocVars :: ArrowTy2 ty2 -> [LocVar]
outLocVars ty = L.map (\(LRM l _ _) -> l) $
                L.filter (\(LRM _ _ m) -> m == Output) (locVars ty)

outRegVars :: ArrowTy2 ty2 -> [RegVar]
outRegVars ty = L.concatMap (\(LRM _ r _) -> case r of
                                          SoAR rr fieldRegions -> [regionToVar r]
                                          _ -> [regionToVar r] 
                      ) $ L.filter (\(LRM _ _ m) -> m == Output) (locVars ty)

inRegVars :: ArrowTy2 ty2 -> [RegVar]
inRegVars ty = L.nub $ L.concatMap (\(LRM _ r _) -> case r of 
                                                SoAR rr fieldRegions -> [regionToVar r]
                                                _ -> [regionToVar r]
                      ) $ L.filter (\(LRM _ _ m) -> m == Input) (locVars ty)

allRegVars :: ArrowTy2 ty2 -> [RegVar]
allRegVars ty = L.nub $ L.concatMap (\ (LRM _ r _) -> [regionToVar r]
                                    ) (locVars ty)

-- | Apply a location substitution to a type.
substLoc :: M.Map LocVar LocVar -> Ty2 -> Ty2
substLoc mp ty =
  case ty of
   SymDictTy v te -> SymDictTy v te -- (go te)
   ProdTy    ts -> ProdTy (L.map go ts)
   PackedTy k l ->
       case M.lookup l mp of
             Just v  -> PackedTy k v
             Nothing -> PackedTy k l
   _ -> ty
  where go = substLoc mp

-- | List version of 'substLoc'.
substLocs :: M.Map LocVar LocVar -> [Ty2] -> [Ty2]
substLocs mp tys = L.map (substLoc mp) tys

-- | Extend an environment for a pattern match. E.g.
--
--     data Foo = MkFoo Int Foo | ...
--
--     case foo1 of
--        MkFoo (i:loc1) (f:loc2) ->
--          new_env2 = extendPatternMatchEnv [loc1,loc2] old_env2
extendPatternMatchEnv :: HasCallStack => DataCon -> DDefs Ty2 -> [Var] -> [LocVar]
                      -> Env2 Var Ty2 -> Env2 Var Ty2
extendPatternMatchEnv dcon ddefs vars locs env2 =
  let tys  = lookupDataCon ddefs dcon
      tys' = foldr
               (\(loc,ty) acc ->
                  case locsInTy ty of
                    []     -> ty:acc
                    [loc2] -> (substLoc (M.singleton loc2 loc) ty) : acc
                    _  -> error $ "extendPatternMatchEnv': Found more than 1 location in type: " ++ sdoc ty)
               []
               (fragileZip locs tys)
  in extendsVEnv (M.fromList $ fragileZip vars tys') env2

-- | Extend an environment for a pattern match. (LocVar) E.g.
--
--     data Foo = MkFoo Int Foo | ...
--
--     case foo1 of
--        MkFoo (i:loc1) (f:loc2) ->
--          new_env2 = extendPatternMatchEnv [loc1,loc2] old_env2
extendPatternMatchEnvLocVar :: HasCallStack => DataCon -> DDefs Ty2 -> [Var] -> [LocVar]
                                -> Env2 FreeVarsTy Ty2 -> Env2 FreeVarsTy Ty2
extendPatternMatchEnvLocVar dcon ddefs vars locs env2 =
  let tys  = lookupDataCon ddefs dcon
      tys' = foldr
               (\(loc,ty) acc ->
                  case locsInTy ty of
                    []     -> ty:acc
                    [loc2] -> (substLoc (M.singleton loc2 loc) ty) : acc
                    _  -> error $ "extendPatternMatchEnvLocVar': Found more than 1 location in type: " ++ sdoc ty)
               []
               (fragileZip locs tys)
      vars' = L.map fromVarToFreeVarsTy vars
  in extendsVEnvLocVar (M.fromList $ fragileZip vars' tys') env2

-- | Apply a substitution to an effect.
substEff :: M.Map LocVar LocVar -> Effect -> Effect
substEff mp (Traverse v) =
    case M.lookup v mp of
      Just v2 -> Traverse v2
      Nothing -> Traverse v

-- | Apply a substitution to an effect set.
substEffs :: M.Map LocVar LocVar -> S.Set Effect -> S.Set Effect
substEffs mp effs =
    S.map (\ef -> substEff mp ef) effs

dummyTyLocs :: Applicative f => UrTy () -> f (UrTy LocVar)
dummyTyLocs ty = traverse (const (pure (singleLocVar (toVar "dummy")))) ty

-- | Collect all the locations mentioned in a type.
locsInTy :: Ty2 -> [LocVar]
locsInTy ty =
    case ty of
      PackedTy _ lv -> [lv]
      ProdTy tys -> concatMap locsInTy tys
      _ -> []

-- Because L2 just adds a bit of metadata and enriched types, it is
-- possible to strip it back down to L1.
revertToL1 :: Prog2  -> Prog1
revertToL1 Prog{ddefs,fundefs,mainExp} =
  Prog ddefs' funefs' mainExp'
  where
    ddefs'   = M.map revertDDef ddefs
    funefs'  = M.map revertFunDef fundefs
    mainExp' = case mainExp of
                Nothing -> Nothing
                Just (e,ty) -> Just (revertExp e, stripTyLocs ty)

revertDDef :: DDef Ty2 -> DDef Ty1
revertDDef (DDef tyargs a b) =
  DDef tyargs a
    (L.filter (\(dcon,_) -> not $ isIndirectionTag dcon) $
         L.map (\(dcon,tys) -> (dcon, L.map (\(x,y) -> (x, stripTyLocs y)) tys)) b)

revertFunDef :: FunDef2 -> FunDef1
revertFunDef FunDef{funName,funArgs,funTy,funBody,funMeta} =
  FunDef { funName = funName
         , funArgs = funArgs --(L.map unwrapLocVar funArgs)
         , funTy   = (L.map stripTyLocs (arrIns funTy), stripTyLocs (arrOut funTy))
         , funBody = revertExp funBody
         , funMeta = funMeta
         }

revertExp :: Exp2 -> Exp1
revertExp ex =
  case ex of
    VarE v    -> VarE v
    LitE n    -> LitE n
    CharE c   -> CharE c
    FloatE n  -> FloatE n
    LitSymE v -> LitSymE v
    AppE v _ args   -> AppE v [] (L.map revertExp args)
    PrimAppE p args -> PrimAppE (revertPrim p) $ L.map revertExp args
    LetE (v,_,ty, (Ext (IndirectionE _ _ _ _ arg))) bod ->
      let PackedTy tycon _ =  ty in
          LetE (v,[],(stripTyLocs ty), AppE (mkCopyFunName tycon) [] [revertExp arg]) (revertExp bod)
    LetE (v,_,ty,rhs) bod ->
      LetE (v,[], stripTyLocs ty, revertExp rhs) (revertExp bod)
    IfE a b c  -> IfE (revertExp a) (revertExp b) (revertExp c)
    MkProdE ls -> MkProdE $ L.map revertExp ls
    ProjE i e  -> ProjE i (revertExp e)
    CaseE scrt brs     -> CaseE (revertExp scrt) (L.map docase brs)
    DataConE _ dcon ls -> DataConE () dcon $ L.map revertExp ls
    TimeIt e ty b -> TimeIt (revertExp e) (stripTyLocs ty) b
    SpawnE v _ args -> SpawnE v [] (L.map revertExp args)
    SyncE -> SyncE
    WithArenaE v e -> WithArenaE v (revertExp e)
    Ext ext ->
      case ext of
        LetRegionE _ _ _ bod -> revertExp bod
        LetParRegionE _ _ _ bod -> revertExp bod
        LetLocE _ _ bod  -> revertExp bod
        StartOfPkdCursor cur -> Ext (L1.StartOfPkdCursor cur)
        TagCursor a _b -> Ext (L1.StartOfPkdCursor a)
        RetE _ v -> VarE v
        AddFixed{} -> error "revertExp: TODO AddFixed."
        FromEndE{} -> error "revertExp: TODO FromEndLE"
        BoundsCheck{}   -> error "revertExp: TODO BoundsCheck"
        IndirectionE{}  -> error "revertExp: TODO IndirectionE"
        GetCilkWorkerNum-> LitE 0
        LetAvail _ bod  -> revertExp bod
        AllocateTagHere{} -> error "revertExp: TODO AddFixed."
        AllocateScalarsHere{} -> error "revertExp: TODO AddFixed."
        SSPush{} -> error "revertExp: TODO SSPush."
        SSPop{} -> error "revertExp: TODO SSPop."
    MapE{}  -> error $ "revertExp: TODO MapE"
    FoldE{} -> error $ "revertExp: TODO FoldE"
  where
    -- Ugh .. this is bad. Can we remove the identity cases here ?
    -- TODO: Get rid of this (and L3.toL3Prim) soon.
    revertPrim :: Prim Ty2 -> Prim Ty1
    revertPrim pr = fmap stripTyLocs pr

    docase :: (DataCon, [(Var,LocVar)], Exp2) -> (DataCon, [(Var,())], Exp1)
    docase (dcon,vlocs,rhs) =
      let (vars,_) = unzip vlocs
      in (dcon, zip vars (repeat ()), revertExp rhs)

docase :: (DataCon, [(Var, LocVar)], Exp2) -> (DataCon, [(Var, ())], Exp1)
docase (dcon, vlocs, rhs) =
  let (vars, _) = unzip vlocs
   in (dcon, zip vars (repeat ()), revertExp rhs)

-- | Does a variable occur in an expression ?
--
-- N.B. it only looks for actual variables, not LocVar's or RegionVar's.
occurs :: S.Set Var -> Exp2 -> Bool
occurs w ex =
  case ex of
    VarE v -> v `S.member` w
    LitE{}    -> False
    CharE{}   -> False
    FloatE{}  -> False
    LitSymE{} -> False
    AppE _ _ ls   -> any go ls
    PrimAppE _ ls -> any go ls
    LetE (_,_,_,rhs) bod -> go rhs || go bod
    IfE a b c   -> go a || go b || go c
    MkProdE ls  -> any go ls
    ProjE _ e   -> go e
    CaseE e brs -> go e || any (\(_,_,bod) -> go bod) brs
    DataConE _ _ ls  -> any go ls
    TimeIt e _ _     -> go e
    SpawnE _ _ ls    -> any go ls
    SyncE            -> False
    WithArenaE v rhs -> v `S.member` w || go rhs
    Ext ext ->
      case ext of
        LetRegionE _ _ _ bod  -> go bod
        LetParRegionE _ _ _ bod  -> go bod
        LetLocE _ le bod  ->
          let oc_bod = go bod in
          case le of
            AfterVariableLE v _  _ -> v `S.member` w || oc_bod
            StartOfRegionLE{}         -> oc_bod
            AfterConstantLE{}   -> oc_bod
            InRegionLE{}        -> oc_bod
            FreeLE{}            -> oc_bod
            FromEndLE{}         -> oc_bod
            _ -> oc_bod
        StartOfPkdCursor v -> v `S.member` w
        TagCursor a b -> a `S.member` w || b `S.member` w
        RetE _ v      -> v `S.member` w
        FromEndE{}    -> False
        BoundsCheck{} -> False
        AddFixed v _  -> v `S.member` w
        IndirectionE _ _ (_,v1) (_,v2) ib ->
          (unwrapLocVar v1) `S.member` w  || (unwrapLocVar v2) `S.member` w || go ib
        GetCilkWorkerNum -> False
        LetAvail _ bod -> go bod
        AllocateTagHere{} -> False
        AllocateScalarsHere{} -> False
        SSPush{} -> False
        SSPop{} -> False
    MapE{}  -> error "occurs: TODO MapE"
    FoldE{} -> error "occurs: TODO FoldE"
  where
    go = occurs w

mapPacked :: (Var -> l -> UrTy l) -> UrTy l -> UrTy l
mapPacked fn t =
  case t of
    IntTy  -> IntTy
    CharTy -> CharTy
    FloatTy-> FloatTy
    BoolTy -> BoolTy
    SymTy  -> SymTy
    (ProdTy x)    -> ProdTy $ L.map (mapPacked fn) x
    (SymDictTy v x) -> SymDictTy v x
    PDictTy k v -> PDictTy k v
    PackedTy k l  -> fn (toVar k) l
    PtrTy    -> PtrTy
    CursorTy -> CursorTy
    CursorArrayTy size -> CursorArrayTy size
    ArenaTy  -> ArenaTy
    VectorTy elty -> VectorTy elty
    ListTy elty   -> ListTy elty
    SymSetTy -> SymSetTy
    SymHashTy-> SymHashTy
    IntHashTy-> IntHashTy

constPacked :: UrTy a1 -> UrTy a2 -> UrTy a1
constPacked c t =
  case t of
    IntTy  -> IntTy
    CharTy -> CharTy
    FloatTy-> FloatTy
    BoolTy -> BoolTy
    SymTy  -> SymTy
    (ProdTy x)    -> ProdTy $ L.map (constPacked c) x
    (SymDictTy v _x) -> SymDictTy v $ stripTyLocs c
    PDictTy k v -> PDictTy (constPacked c k) (constPacked c v)
    PackedTy _k _l  -> c
    PtrTy    -> PtrTy
    CursorTy -> CursorTy
    CursorArrayTy size -> CursorArrayTy size
    ArenaTy  -> ArenaTy
    VectorTy el_ty -> VectorTy (constPacked c el_ty)
    ListTy el_ty -> ListTy (constPacked c el_ty)
    SymSetTy -> SymSetTy
    SymHashTy-> SymHashTy
    IntHashTy-> IntHashTy


-- data RegOrLoc = DReg Region | DLoc LocVar
--   deriving (Read,Show,Eq,Ord, Generic, NFData, Out)

-- varToRegOrLoc :: Var -> RegOrLoc
-- varToRegOrLoc v = DLoc $ singleLocVar v

-- regionToRegOrLoc :: Region -> RegOrLoc
-- regionToRegOrLoc r = DReg r

-- locToRegOrLoc :: LocVar -> RegOrLoc
-- locToRegOrLoc l = DLoc l

-- | Build a dependency list which can be later converted to a graph
depList :: Exp2 -> [(FreeVarsTy, FreeVarsTy, [FreeVarsTy])]
-- The helper function, go, works with a map rather than list so that all
-- dependencies are properly grouped, without any duplicate keys. But we
-- convert it back to a list so that we can hand it off to 'graphFromEdges'.
-- Reversing the list makes it easy to peek at the return value of this AST later.
depList = L.map (\(a,b) -> (a,a,b)) . M.toList . go M.empty
    where
      go :: M.Map FreeVarsTy [FreeVarsTy] -> Exp2 -> M.Map FreeVarsTy [FreeVarsTy]
      go acc ex =
        case ex of
          VarE v    -> M.insertWith (++) (fromVarToFreeVarsTy v) [(fromVarToFreeVarsTy v)] acc
          LitE{}    -> acc
          CharE{}   -> acc
          FloatE{}  -> acc
          LitSymE{} -> acc
          AppE _ _ args   -> foldl go acc args
          PrimAppE _ args -> foldl go acc args
          LetE (v,_,_,rhs) bod ->
            let acc_rhs = go acc rhs
            in go (M.insertWith (++) (fromVarToFreeVarsTy v) (S.toList $ allFreeVars rhs) acc_rhs) bod
          IfE _ b c  -> go (go acc b) c
          MkProdE ls -> foldl go acc ls
          ProjE _ e  -> go acc e
          CaseE (VarE v) mp ->
            L.foldr (\(_,vlocs,e) acc' ->
                       let (vars,locs) = unzip vlocs
                           vars' = L.map fromVarToFreeVarsTy vars
                           acc'' = L.foldr (\w acc''' -> M.insertWith (++) (fromVarToFreeVarsTy v) [w] acc''')
                                           acc'
                                           (vars' ++ (L.map fromLocVarToFreeVarsTy locs))
                       in go acc'' e)
                    acc
                    mp
          CaseE _scrt mp -> L.foldr (\(_,_,e) acc' -> go acc' e) acc mp
          DataConE _ _ args -> foldl go acc args
          TimeIt e _ _ -> go acc e
          WithArenaE _ e -> go acc e
          SpawnE _ _ ls  -> foldl go acc ls
          SyncE          -> acc
          MapE{}  -> acc
          FoldE{} -> acc
          Ext ext ->
            case ext of
              LetRegionE r _ _ rhs ->
                go (M.insertWith (++) (fromRegVarToFreeVarsTy (regionToVar r)) (S.toList $ allFreeVars rhs) acc) rhs
              LetParRegionE r _ _ rhs ->
                go (M.insertWith (++) (fromRegVarToFreeVarsTy (regionToVar r)) (S.toList $ allFreeVars rhs) acc) rhs
              LetLocE loc phs rhs  ->
                -- Assumption that the loc for the data constructor buffer is passed in case 
                -- of SoA. If in SoA, ignoring the locs of the fields atm. 
                go (M.insertWith (++) (fromLocVarToFreeVarsTy loc) (dep phs ++ (S.toList $ allFreeVars rhs)) acc) rhs
              RetE{}         -> acc
              FromEndE{}     -> acc
              BoundsCheck{}  -> acc
              IndirectionE{} -> acc
              AddFixed v _   -> M.insertWith (++) (fromVarToFreeVarsTy v) [fromVarToFreeVarsTy v] acc
              GetCilkWorkerNum -> acc
              LetAvail _ bod -> go acc bod
              AllocateTagHere{} -> acc
              AllocateScalarsHere{} -> acc
              SSPush{} -> acc
              SSPop{} -> acc
              StartOfPkdCursor w -> go acc (VarE w)
              TagCursor a b -> go (go acc (VarE a)) (VarE b)

      dep :: PreLocExp LocVar -> [FreeVarsTy]
      dep ex =
        case ex of
          StartOfRegionLE r -> [fromRegVarToFreeVarsTy (regionToVar r)]
          AfterConstantLE _ loc -> [fromLocVarToFreeVarsTy loc]
          AfterVariableLE v loc _ -> [fromVarToFreeVarsTy v, fromLocVarToFreeVarsTy loc]
          InRegionLE r  -> [fromRegVarToFreeVarsTy (regionToVar r)]
          FromEndLE loc -> [fromLocVarToFreeVarsTy loc]
          FreeLE -> []

-- TODO: VS: I don't think region vars are handled properly here. 
allFreeVars :: Exp2 -> S.Set FreeVarsTy
allFreeVars ex =
  case ex of
    AppE _ locs args -> S.fromList (map fromLocVarToFreeVarsTy locs) `S.union` (S.unions (map allFreeVars args))
    PrimAppE _ args -> (S.unions (map allFreeVars args))
    LetE (v,locs,_,rhs) bod -> (S.fromList (map fromLocVarToFreeVarsTy locs) `S.union` (allFreeVars rhs) `S.union` (allFreeVars bod))
                               `S.difference` S.singleton (V v)
    IfE a b c -> allFreeVars a `S.union` allFreeVars b `S.union` allFreeVars c
    MkProdE args -> (S.unions (map allFreeVars args))
    ProjE _ bod -> allFreeVars bod
    CaseE scrt brs -> (allFreeVars scrt) `S.union` (S.unions (map (\(_,vlocs,c) -> allFreeVars c `S.difference`
                                                                                   S.fromList (map (V . fst) vlocs) `S.difference`
                                                                                   (S.fromList (map (fromLocVarToFreeVarsTy . snd) vlocs)))
                                                                  brs))
    DataConE locvar _ args -> S.singleton (fromLocVarToFreeVarsTy locvar) `S.union` (S.unions (map allFreeVars args))
    TimeIt e _ _ -> allFreeVars e
    WithArenaE _ e -> allFreeVars e
    SpawnE _ locs args -> S.fromList (map fromLocVarToFreeVarsTy locs) `S.union` (S.unions (map allFreeVars args))
    Ext ext ->
      case ext of
        LetRegionE r _ _ bod -> let regVar = regionToVar r
                                  in S.delete (R regVar) (allFreeVars bod)
        LetParRegionE r _ _ bod -> S.delete (R $ regionToVar r) (allFreeVars bod)
        LetLocE loc locexp bod -> let locs_locexp = case locexp of 
                                                      AfterConstantLE _ loc   -> S.singleton $ fromLocVarToFreeVarsTy loc
                                                      AfterVariableLE v loc _ -> S.fromList [fromLocVarToFreeVarsTy loc]
                                                      -- All the locations inside an SoA loc are also free. 
                                                      -- TODO this works since locvar is not recursive but this needs to change
                                                      GetDataConLocSoA loc -> S.fromList $ L.map (fromLocVarToFreeVarsTy . singleLocVar) (varsInLocVar loc)
                                                      GetFieldLocSoA _ loc -> S.fromList $ L.map (fromLocVarToFreeVarsTy . singleLocVar) (varsInLocVar loc)
                                                      GenSoALoc dloc flocs -> let 
                                                                               field_locs_only = map (\(_, floc_inner) -> floc_inner) flocs
                                                                               field_locs_only' = field_locs_only ++ [dloc]
                                                                              in S.fromList (L.map fromLocVarToFreeVarsTy field_locs_only') 
                                                        
                                                        --S.fromList (L.map (fromLocVarToFreeVarsTy . singleLocVar) (varsInLocVar loc)) `S.union` (S.fromList $ L.map (fromLocVarToFreeVarsTy . singleLocVar) (L.concatMap (\(_, loc) -> varsInLocVar loc) flocs))
                                                      _ -> S.empty
                                      vars_locexp = S.map fromVarToFreeVarsTy (gFreeVars locexp)
                                    in S.delete (fromLocVarToFreeVarsTy loc) (allFreeVars bod `S.union` locs_locexp `S.union` vars_locexp)
        StartOfPkdCursor cur -> S.singleton (V cur)
        TagCursor a b -> S.fromList [V a, V b]
        RetE locs v     -> S.insert (V v) (S.fromList (map fromLocVarToFreeVarsTy locs))
        FromEndE loc    -> S.singleton (fromLocVarToFreeVarsTy loc)
        BoundsCheck _ reg cur -> S.fromList [(fromLocVarToFreeVarsTy reg),(fromLocVarToFreeVarsTy cur)]
        IndirectionE _ _ (a, b) (c, d) _ -> S.fromList $ [(fromLocVarToFreeVarsTy a), (fromRegVarToFreeVarsTy (fromLocVarToRegVar b)), (fromLocVarToFreeVarsTy c), (fromRegVarToFreeVarsTy (fromLocVarToRegVar d))]
        AddFixed v _    -> S.singleton (V v)
        GetCilkWorkerNum -> S.empty
        LetAvail vs bod -> S.fromList (L.map V vs) `S.union` (S.map V (gFreeVars bod))
        AllocateTagHere loc _ -> S.singleton (fromLocVarToFreeVarsTy loc)
        AllocateScalarsHere loc -> S.singleton (fromLocVarToFreeVarsTy loc)
        SSPush _ a b _ -> S.fromList [(fromLocVarToFreeVarsTy a), (fromLocVarToFreeVarsTy b)]
        SSPop _ a b -> S.fromList [(fromLocVarToFreeVarsTy a), (fromLocVarToFreeVarsTy b)]
    _ -> S.map V (gFreeVars ex)

allFreeVars' :: Exp2 -> S.Set Var 
allFreeVars' ex = let freeVars = allFreeVars ex 
                   in S.fromList $ L.concatMap (\fv -> case fv of 
                                      V v -> [v] 
                                      FL l -> varsInLocVar l
                                      R r -> varsInRegVar r
                            ) (S.toList freeVars)

freeLocVars :: Exp2 -> [LocVar]
freeLocVars ex = L.map getLocVarFromFreeVarsTy $ S.toList (allFreeVars ex)

freeVars :: Exp2 -> [Var]
freeVars ex = L.map getVarFromFreeVarsTy $ S.toList (allFreeVars ex)

freeRegVars :: Exp2 -> [RegVar]
freeRegVars ex = L.map getRegVarFromFreeVarsTy $ S.toList (allFreeVars ex)

changeAppToSpawn :: (Eq loc, Eq dec) => Var -> [PreExp E2Ext loc dec] -> PreExp E2Ext loc dec -> PreExp E2Ext loc dec
changeAppToSpawn v args2 ex1 =
  case ex1 of
    VarE{}    -> ex1
    LitE{}    -> ex1
    CharE{}   -> ex1
    FloatE{}  -> ex1
    LitSymE{} -> ex1
    AppE f locs args | v == f && args == args2 -> SpawnE f locs $ map go args
    AppE f locs args -> AppE f locs $ map go args
    PrimAppE f args  -> PrimAppE f $ map go args
    LetE (v,loc,ty,rhs) bod -> LetE (v,loc,ty, go rhs) (go bod)
    IfE a b c  -> IfE (go a) (go b) (go c)
    MkProdE xs -> MkProdE $ map go xs
    ProjE i e  -> ProjE i $ go e
    DataConE loc dcon args -> DataConE loc dcon $ map go args
    CaseE scrt mp ->
      CaseE (go scrt) $ map (\(a,b,c) -> (a,b, go c)) mp
    TimeIt e ty b  -> TimeIt (go e) ty b
    WithArenaE v e -> WithArenaE v (go e)
    SpawnE{} -> ex1
    SyncE{}  -> ex1
    Ext ext ->
      case ext of
        LetRegionE r sz ty rhs  -> Ext $ LetRegionE r sz ty (go rhs)
        LetParRegionE r sz ty rhs  -> Ext $ LetParRegionE r sz ty (go rhs)
        LetLocE l lhs rhs -> Ext $ LetLocE l lhs (go rhs)
        StartOfPkdCursor{} -> ex1
        TagCursor{}    -> ex1
        RetE{}            -> ex1
        FromEndE{}        -> ex1
        BoundsCheck{}     -> ex1
        IndirectionE{}    -> ex1
        AddFixed{}        -> ex1
        GetCilkWorkerNum  -> ex1
        LetAvail vs bod   -> Ext $ LetAvail vs (go bod)
        AllocateTagHere{} -> ex1
        AllocateScalarsHere{} -> ex1
        SSPush{} -> ex1
        SSPop{} -> ex1
    MapE{}  -> error "addRANExp: TODO MapE"
    FoldE{}  -> error "addRANExp: TODO FoldE"

  where go = changeAppToSpawn v args2


fromSingleRegVarToVar :: RegVar -> Var
fromSingleRegVarToVar (SingleR v) = v
fromSingleRegVarToVar _ = error "fromSingleRegVarToVar: unexpected case."

fromRegVarToLocVar :: RegVar -> LocVar
fromRegVarToLocVar reg = case reg of 
  SingleR v -> Single v
  SoARv regvar fieldRegs -> SoA (fromSingleRegVarToVar regvar) (L.map (\(k, freg) -> (k, fromRegVarToLocVar freg)) fieldRegs)

fromLocVarToRegVar :: LocVar -> RegVar
fromLocVarToRegVar loc = case loc of 
  Single v -> SingleR v
  SoA dcon fieldLocs -> SoARv (SingleR dcon) (L.map (\(k, floc) -> (k, fromLocVarToRegVar floc)) fieldLocs)  