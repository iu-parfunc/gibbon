{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

-- | An intermediate language with an effect system that captures traversals.

module Gibbon.NewL2.Syntax
    (
    -- * Extended language L2 with location types.
      Old.E2Ext(..)
    , Prog2, DDefs2, DDef2, FunDef2, FunDefs2, Exp2, Ty2(..)
    , Old.Effect(..), Old.ArrowTy2(..) , Old.LocRet(..), LocArg(..), LocExp, RegExp, Old.PreLocExp(..), Old.PreRegExp(..)

    -- * Regions and locations
    , LocVar, Old.Region(..), Old.Modality(..),  Old.LRM(..), LREM(..)
    , Old.Multiplicity(..), Old.RegionSize(..), Old.RegionType(..), Old.regionToVar

    -- * Operations on types
    , Old.allLocVars, Old.inLocVars, Old.outLocVars, Old.outRegVars, Old.inRegVars, Old.allRegVars
    , substLoc, substLocs, Old.substEff, Old.substEffs, extendPatternMatchEnv, extendPatternMatchEnvLocVar
    , locsInTy, Old.dummyTyLocs, allFreeVars, freeLocVars
    , toLocVar, fromLRM, fromVarToSingleRegVar, fromLocArgToFreeVarsTy, Old.fromLocVarToRegVar

    -- * Other helpers
    , revertToL1, Old.occurs, Old.mapPacked, Old.constPacked, depList, Old.changeAppToSpawn
    , toEndFromTaggedV, toTagV, toEndFromTaggedRegVar, genSymRegVar

    , module Gibbon.Language
    )
    where

import           Control.DeepSeq
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Safe as Sf

import           GHC.Stack (HasCallStack)
import           Text.PrettyPrint.GenericPretty

import           Gibbon.Common
import           Gibbon.Language
-- import           Text.PrettyPrint.HughesPJ
import           Gibbon.L1.Syntax hiding (AddFixed, StartOfPkdCursor)
import qualified Gibbon.L1.Syntax as L1

import qualified Gibbon.L2.Syntax as Old

--------------------------------------------------------------------------------

type Prog2    = Prog Var Exp2
type DDef2    = DDef Ty2
type DDefs2   = DDefs Ty2
type FunDef2  = FunDef Var Exp2
type FunDefs2 = FunDefs Var Exp2

-- | Function types know about locations and traversal effects.
instance FunctionTy Ty2 where
  type ArrowTy Ty2 = Old.ArrowTy2 Ty2
  inTys = Old.arrIns
  outTy = Old.arrOut

-- | Extended expressions, L2.
--
--   By adding a `LocVar` decoration, all data constructors,
--   applications, and bindings gain a location annotation.
type Exp2   = PreExp Old.E2Ext LocArg Ty2
type LocExp = Old.PreLocExp LocArg
type RegExp = Old.PreRegExp LocArg

-- We need a newtype here to avoid overlapping type family instance for FunctionTy
-- | L1 Types extended with abstract Locations.
newtype Ty2 = MkTy2 { unTy2 :: (UrTy LocVar) }
  deriving (Read, Show, Eq, Ord, Generic)

instance Out Ty2
instance NFData Ty2

--------------------------------------------------------------------------------

data LREM = LREM { lremLoc    :: LocVar
                 , lremReg    :: RegVar
                 , lremEndReg :: RegVar
                 , lremMode   :: Old.Modality
                 }
  deriving (Read,Show,Eq,Ord,Generic)

instance Out LREM

instance NFData LREM where
  rnf (LREM a b c d)  = rnf a `seq` rnf b `seq` rnf c `seq` rnf d

fromLRM :: Old.LRM -> LREM
fromLRM (Old.LRM loc reg mode) = case reg of 
  _ -> LREM loc (Old.regionToVar reg) (toEndVRegVar (Old.regionToVar reg)) mode
  Old.SoAR _ _ -> error "TODO: NewL2/Syntax.hs, fromLRM, implement SoA region."

data LocArg = Loc LREM
            | EndWitness LREM LocVar
            | Reg RegVar Old.Modality
            | EndOfReg RegVar Old.Modality RegVar
            | EndOfReg_Tagged RegVar
  deriving (Read, Show, Eq, Ord, Generic)

instance Out LocArg
instance NFData LocArg

toRegVar :: LocArg -> RegVar
toRegVar arg =
  case arg of
    Loc lrm        -> lremReg lrm
    Reg v _        -> v
    EndOfReg _ _ v -> v
    EndOfReg_Tagged v -> v

fromVarToSingleRegVar :: Var -> RegVar
fromVarToSingleRegVar v = SingleR v

{- VS: TODO: this should return either LocVar or RegVar -}
toLocVar :: LocArg -> LocVar
toLocVar arg =
  case arg of
    Loc lrm        -> lremLoc lrm
    EndWitness _ v -> v
    Loc lrm        -> Old.fromRegVarToLocVar $ lremReg lrm
    Reg v _        -> Old.fromRegVarToLocVar v
    EndOfReg _ _ v -> Old.fromRegVarToLocVar v
    EndOfReg_Tagged v -> Old.fromRegVarToLocVar v
    _ -> error "NewL2/Syntax.hs, toLocVar: unexpected case."


fromLocArgToFreeVarsTy :: LocArg -> FreeVarsTy
fromLocArgToFreeVarsTy arg =
  case arg of
    Loc lrm        -> fromLocVarToFreeVarsTy $ lremLoc lrm
    EndWitness _ v -> fromLocVarToFreeVarsTy v
    Loc lrm        -> fromRegVarToFreeVarsTy $ lremReg lrm
    Reg v _        -> fromRegVarToFreeVarsTy v
    EndOfReg _ _ v -> fromRegVarToFreeVarsTy v
    EndOfReg_Tagged v -> fromRegVarToFreeVarsTy v
    _ -> error "NewL2/Syntax.hs, toLocVar: unexpected case."


-- Returns the data constructor 
-- fromLocArgToVar :: LocArg -> Var 
-- fromLocArgToVar arg =
--   case arg of
--     Loc lrm        -> unwrapLocVar $ lremLoc lrm
--     EndWitness _ v -> v
--     Reg v _        -> v
--     EndOfReg _ _ v -> v
--     EndOfReg_Tagged v -> toEndFromTaggedV v

instance Out (Old.ArrowTy2 Ty2)

toTagV :: Var -> Var
toTagV v = (toVar "tag_") `varAppend` v

toEndFromTaggedV :: Var -> Var
toEndFromTaggedV v = (toVar "end_from_tagged_") `varAppend` v

genSymRegVar :: RegVar -> PassM RegVar
genSymRegVar reg = do
  case reg of 
    SingleR v -> do 
                  v' <- gensym v
                  return $ SingleR v'
    SoARv reg fieldRegs -> do 
                  reg' <- genSymRegVar reg
                  fieldRegs' <- mapM (\(k, freg) -> do 
                                        freg' <- genSymRegVar freg
                                        return (k, freg')) fieldRegs
                  return $ SoARv reg' fieldRegs'

toEndFromTaggedRegVar :: RegVar -> RegVar
toEndFromTaggedRegVar r = case r of 
                            SingleR v -> SingleR (toEndFromTaggedV v)
                            SoARv regvar fieldRegs -> SoARv (toEndFromTaggedRegVar regvar) (L.map (\(k, freg) -> (k, toEndFromTaggedRegVar freg)) fieldRegs) 

--------------------------------------------------------------------------------

instance FreeVars LocExp where
  gFreeVars e =
    case e of
      -- Old.AfterConstantLE _ loc  -> S.singleton $ unwrapLocVar (toLocVar loc)
      -- Old.AfterVariableLE v loc _ -> S.fromList [v, unwrapLocVar (toLocVar loc)]
      Old.AfterConstantLE _ loc  -> S.empty
      Old.AfterVariableLE v loc _ -> S.fromList [v] 
      _ -> S.empty


instance Typeable (Old.E2Ext LocArg Ty2) where
  gRecoverType ddfs env2 ex =
    case ex of
      Old.LetRegionE _r _ _ bod    -> gRecoverType ddfs env2 bod
      Old.LetParRegionE _r _ _ bod -> gRecoverType ddfs env2 bod
      Old.StartOfPkdCursor{}       -> MkTy2 $ CursorTy
      Old.TagCursor{}      -> MkTy2 $ CursorTy
      Old.LetLocE _l _rhs bod -> gRecoverType ddfs env2 bod
      Old.RetE _loc var       -> case M.lookup var (vEnv env2) of
                                   Just ty -> ty
                                   Nothing -> error $ "gRecoverType: unbound variable " ++ sdoc var
      Old.FromEndE _loc       -> error "Shouldn't enconter FromEndE in tail position"
      Old.BoundsCheck{}       -> error "Shouldn't enconter BoundsCheck in tail position"
      Old.IndirectionE tycon _ _ (to,_) _ -> MkTy2 $ PackedTy tycon (toLocVar to)
      Old.AddFixed{}          -> error "Shouldn't enconter AddFixed in tail position"
      Old.GetCilkWorkerNum    -> MkTy2 $ IntTy
      Old.LetAvail _ bod      -> gRecoverType ddfs env2 bod
      Old.AllocateTagHere{}   -> MkTy2 $ ProdTy []
      Old.AllocateScalarsHere{} -> MkTy2 $ ProdTy []
      Old.SSPush{}              -> MkTy2 $ ProdTy []
      Old.SSPop{}               -> MkTy2 $ ProdTy []

  gRecoverTypeLoc ddfs env2 ex =
    case ex of
      Old.LetRegionE _r _ _ bod    -> gRecoverTypeLoc ddfs env2 bod
      Old.LetParRegionE _r _ _ bod -> gRecoverTypeLoc ddfs env2 bod
      Old.StartOfPkdCursor{}       -> MkTy2 $ CursorTy
      Old.TagCursor{}      -> MkTy2 $ CursorTy
      Old.LetLocE _l _rhs bod -> gRecoverTypeLoc ddfs env2 bod
      Old.RetE _loc var       -> case M.lookup (fromVarToFreeVarsTy var) (vEnv env2) of
                                   Just ty -> ty
                                   Nothing -> error $ "gRecoverType: unbound variable " ++ sdoc var
      Old.FromEndE _loc       -> error "Shouldn't enconter FromEndE in tail position"
      Old.BoundsCheck{}       -> error "Shouldn't enconter BoundsCheck in tail position"
      Old.IndirectionE tycon _ _ (to,_) _ -> MkTy2 $ PackedTy tycon (toLocVar to)
      Old.AddFixed{}          -> error "Shouldn't enconter AddFixed in tail position"
      Old.GetCilkWorkerNum    -> MkTy2 $ IntTy
      Old.LetAvail _ bod      -> gRecoverTypeLoc ddfs env2 bod
      Old.AllocateTagHere{}   -> MkTy2 $ ProdTy []
      Old.AllocateScalarsHere{} -> MkTy2 $ ProdTy []
      Old.SSPush{}              -> MkTy2 $ ProdTy []
      Old.SSPop{}               -> MkTy2 $ ProdTy []



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
instance Out (Old.E2Ext LocArg Ty2) => Typeable (PreExp Old.E2Ext LocArg Ty2) where
  gRecoverType ddfs env2 ex =
    case ex of
      VarE v       -> M.findWithDefault (error $ "Cannot find type of variable " ++ show v ++ " in " ++ show (vEnv env2)) v (vEnv env2)
      LitE _       -> MkTy2 $ IntTy
      CharE _      -> MkTy2 $ CharTy
      FloatE{}     -> MkTy2 $ FloatTy
      LitSymE _    -> MkTy2 $ SymTy
      AppE v locargs _ ->
                       let fnty  = fEnv env2 # v
                           outty = Old.arrOut fnty
                           mp = M.fromList $ zip (Old.allLocVars fnty) (map toLocVar locargs)
                       in substLoc mp outty

      PrimAppE (DictInsertP ty) ((VarE v):_) -> MkTy2 $ SymDictTy (Just v) $ stripTyLocs (unTy2 ty)
      PrimAppE (DictEmptyP  ty) ((VarE v):_) -> MkTy2 $ SymDictTy (Just v) $ stripTyLocs (unTy2 ty)
      PrimAppE p _ -> MkTy2 $ primRetTy (fmap unTy2 p)

      LetE (v,_,t,_) e -> gRecoverType ddfs (extendVEnv v t env2) e
      IfE _ e _        -> gRecoverType ddfs env2 e
      MkProdE es       -> MkTy2 $ ProdTy $ L.map (unTy2 . gRecoverType ddfs env2) es
      DataConE loc c _ -> MkTy2 $ PackedTy (getTyOfDataCon ddfs c) (toLocVar loc)
      TimeIt e _ _     -> gRecoverType ddfs env2 e
      MapE _ e         -> gRecoverType ddfs env2 e
      FoldE _ _ e      -> gRecoverType ddfs env2 e
      Ext ext          -> gRecoverType ddfs env2 ext
      ProjE i e ->
        case unTy2 $ gRecoverType ddfs env2 e of
          (ProdTy tys) -> MkTy2 $ (tys !! i)
          oth -> error$ "typeExp: Cannot project fields from this type: "++show oth
                        ++"\nExpression:\n  "++ sdoc ex
                        ++"\nEnvironment:\n  "++sdoc (vEnv env2)
      SpawnE v locargs _ ->
                         let fnty  = fEnv env2 # v
                             outty = Old.arrOut fnty
                             mp = M.fromList $ zip (Old.allLocVars fnty) (map toLocVar locargs)
                         in substLoc mp outty
      SyncE -> MkTy2 $ voidTy
      WithArenaE _v e -> gRecoverType ddfs env2 e
      CaseE _ mp ->
        let (c,vlocargs,e) = Sf.headErr mp
            (vars,locargs) = unzip vlocargs
            locs = map toLocVar locargs

            env2' = extendPatternMatchEnv c ddfs vars locs env2
        in gRecoverType ddfs env2' e


  gRecoverTypeLoc ddfs env2 ex =
    case ex of
      VarE v       -> M.findWithDefault (error $ "Cannot find type of variable " ++ show v ++ " in " ++ show (vEnv env2)) (fromVarToFreeVarsTy v) (vEnv env2)
      LitE _       -> MkTy2 $ IntTy
      CharE _      -> MkTy2 $ CharTy
      FloatE{}     -> MkTy2 $ FloatTy
      LitSymE _    -> MkTy2 $ SymTy
      AppE v locargs _ ->
                       let fnty  = fEnv env2 # (fromVarToFreeVarsTy v)
                           outty = Old.arrOut fnty
                           mp = M.fromList $ zip (Old.allLocVars fnty) (map toLocVar locargs)
                       in substLoc mp outty

      PrimAppE (DictInsertP ty) ((VarE v):_) -> MkTy2 $ SymDictTy (Just v) $ stripTyLocs (unTy2 ty)
      PrimAppE (DictEmptyP  ty) ((VarE v):_) -> MkTy2 $ SymDictTy (Just v) $ stripTyLocs (unTy2 ty)
      PrimAppE p _ -> MkTy2 $ primRetTy (fmap unTy2 p)

      LetE (v,_,t,_) e -> gRecoverTypeLoc ddfs (extendVEnvLocVar (fromVarToFreeVarsTy v) t env2) e
      IfE _ e _        -> gRecoverTypeLoc ddfs env2 e
      MkProdE es       -> MkTy2 $ ProdTy $ L.map (unTy2 . gRecoverTypeLoc ddfs env2) es
      DataConE loc c _ -> MkTy2 $ PackedTy (getTyOfDataCon ddfs c) (toLocVar loc)
      TimeIt e _ _     -> gRecoverTypeLoc ddfs env2 e
      MapE _ e         -> gRecoverTypeLoc ddfs env2 e
      FoldE _ _ e      -> gRecoverTypeLoc ddfs env2 e
      Ext ext          -> gRecoverTypeLoc ddfs env2 ext
      ProjE i e ->
        case unTy2 $ gRecoverTypeLoc ddfs env2 e of
          (ProdTy tys) -> MkTy2 $ (tys !! i)
          oth -> error$ "typeExp: Cannot project fields from this type: "++show oth
                        ++"\nExpression:\n  "++ sdoc ex
                        ++"\nEnvironment:\n  "++sdoc (vEnv env2)
      SpawnE v locargs _ ->
                         let fnty  = fEnv env2 # (fromVarToFreeVarsTy v)
                             outty = Old.arrOut fnty
                             mp = M.fromList $ zip (Old.allLocVars fnty) (map toLocVar locargs)
                         in substLoc mp outty
      SyncE -> MkTy2 $ voidTy
      WithArenaE _v e -> gRecoverTypeLoc ddfs env2 e
      CaseE _ mp ->
        let (c,vlocargs,e) = Sf.headErr mp
            (vars,locargs) = unzip vlocargs
            locs = map toLocVar locargs

            env2' = extendPatternMatchEnvLocVar c ddfs vars locs env2
        in gRecoverTypeLoc ddfs env2' e

-------------------------------------------------------------------------------
-- Need to redefine the following because of the Ty2 newtype:

-- | Apply a location substitution to a type.
substLoc :: M.Map LocVar LocVar -> Ty2 -> Ty2
substLoc mp ty = MkTy2 $
  case unTy2 ty of
   SymDictTy v te -> SymDictTy v te -- (go te)
   ProdTy    ts -> ProdTy (L.map (unTy2 . go . MkTy2) ts)
   PackedTy k l ->
       case M.lookup l mp of
             Just v  -> PackedTy k v
             Nothing -> PackedTy k l
   _ -> unTy2 ty
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

extendPatternMatchEnvLocVar :: HasCallStack => DataCon -> DDefs Ty2 -> [Var] -> [LocVar]
                      -> Env2 FreeVarsTy Ty2 -> Env2 FreeVarsTy Ty2
extendPatternMatchEnvLocVar dcon ddefs vars locs env2 =
  let tys  = lookupDataCon ddefs dcon
      tys' = foldr
               (\(loc,ty) acc ->
                  case locsInTy ty of
                    []     -> ty:acc
                    [loc2] -> (substLoc (M.singleton loc2 loc) ty) : acc
                    _  -> error $ "extendPatternMatchEnv': Found more than 1 location in type: " ++ sdoc ty)
               []
               (fragileZip locs tys)
      vars' = L.map fromVarToFreeVarsTy vars
  in extendsVEnvLocVar (M.fromList $ fragileZip vars' tys') env2

-- | Collect all the locations mentioned in a type.
locsInTy :: Ty2 -> [LocVar]
locsInTy ty =
    case unTy2 ty of
      PackedTy _ lv -> [lv]
      ProdTy tys -> concatMap (locsInTy . MkTy2) tys
      _ -> []

-- Because L2 just adds a bit of metadata and enriched types, it is
-- possible to strip it back down to L1.
revertToL1 :: Prog2 -> Prog1
revertToL1 Prog{ddefs,fundefs,mainExp} =
  Prog ddefs' funefs' mainExp'
  where
    ddefs'   = M.map revertDDef ddefs
    funefs'  = M.map revertFunDef fundefs
    mainExp' = case mainExp of
                Nothing -> Nothing
                Just (e,ty) -> Just (revertExp e, stripTyLocs (unTy2 ty))

revertDDef :: DDef Ty2 -> DDef Ty1
revertDDef (DDef tyargs a b) =
  DDef tyargs a
    (L.filter (\(dcon,_) -> not $ isIndirectionTag dcon) $
         L.map (\(dcon,tys) -> (dcon, L.map (\(x,y) -> (x, stripTyLocs (unTy2 y))) tys)) b)

revertFunDef :: FunDef2 -> FunDef1
revertFunDef FunDef{funName,funArgs,funTy,funBody,funMeta} =
  FunDef { funName = funName
         , funArgs = funArgs
         , funTy   = (L.map (stripTyLocs . unTy2) (Old.arrIns funTy), stripTyLocs (unTy2 (Old.arrOut funTy)))
         , funBody = revertExp funBody
         , funMeta = funMeta
         }

revertExp :: Exp2 -> Exp1
revertExp ex =
  case ex of
    VarE v    -> VarE v
    LitE n    -> LitE n
    CharE n  -> CharE n
    FloatE n  -> FloatE n
    LitSymE v -> LitSymE v
    AppE v _ args   -> AppE v [] (L.map revertExp args)
    PrimAppE p args -> PrimAppE (revertPrim p) $ L.map revertExp args
    LetE (v,_, ty, (Ext (Old.IndirectionE _ _ _ _ arg))) bod ->
      let PackedTy tycon _ =  unTy2 ty in
          LetE (v,[],(stripTyLocs (unTy2 ty)), AppE (mkCopyFunName tycon) [] [revertExp arg]) (revertExp bod)
    LetE (v,_,ty,rhs) bod ->
      LetE (v,[], stripTyLocs (unTy2 ty), revertExp rhs) (revertExp bod)
    IfE a b c  -> IfE (revertExp a) (revertExp b) (revertExp c)
    MkProdE ls -> MkProdE $ L.map revertExp ls
    ProjE i e  -> ProjE i (revertExp e)
    CaseE scrt brs     -> CaseE (revertExp scrt) (L.map docase brs)
    DataConE _ dcon ls -> DataConE () dcon $ L.map revertExp ls
    TimeIt e ty b -> TimeIt (revertExp e) (stripTyLocs (unTy2 ty)) b
    SpawnE v _ args -> SpawnE v [] (L.map revertExp args)
    SyncE -> SyncE
    WithArenaE v e -> WithArenaE v (revertExp e)
    Ext ext ->
      case ext of
        Old.LetRegionE _ _ _ bod -> revertExp bod
        Old.LetParRegionE _ _ _ bod -> revertExp bod
        Old.LetLocE _ _ bod  -> revertExp bod
        Old.TagCursor a _b -> Ext (L1.StartOfPkdCursor a)
        Old.StartOfPkdCursor v -> Ext (L1.StartOfPkdCursor v)
        Old.RetE _ v -> VarE v
        Old.AddFixed{} -> error "revertExp: TODO AddFixed."
        Old.FromEndE{} -> error "revertExp: TODO FromEndLE"
        Old.BoundsCheck{}   -> error "revertExp: TODO BoundsCheck"
        Old.IndirectionE{}  -> error "revertExp: TODO IndirectionE"
        Old.GetCilkWorkerNum-> LitE 0
        Old.LetAvail _ bod  -> revertExp bod
        Old.AllocateTagHere{} -> error "revertExp: TODO AddFixed."
        Old.AllocateScalarsHere{} -> error "revertExp: TODO AddFixed."
        Old.SSPush{} -> error "revertExp: TODO SSPush."
        Old.SSPop{} -> error "revertExp: TODO SSPop."
    MapE{}  -> error $ "revertExp: TODO MapE"
    FoldE{} -> error $ "revertExp: TODO FoldE"
  where
    -- Ugh .. this is bad. Can we remove the identity cases here ?
    -- TODO: Get rid of this (and L3.toL3Prim) soon.
    revertPrim :: Prim Ty2 -> Prim Ty1
    revertPrim pr = fmap (stripTyLocs . unTy2) pr

    docase :: (DataCon, [(Var,LocArg)], Exp2) -> (DataCon, [(Var,())], Exp1)
    docase (dcon,vlocargs,rhs) =
      let (vars,_) = unzip vlocargs
      in (dcon, zip vars (repeat ()), revertExp rhs)


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
          CharE{}  -> acc
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
                           acc'' = L.foldr (\w acc''' -> M.insertWith (++) (fromVarToFreeVarsTy v) [w] acc''')
                                           acc'
                                           ((map fromVarToFreeVarsTy vars) ++ (map (fromLocVarToFreeVarsTy . toLocVar) locs))
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
              Old.LetRegionE r _ _ rhs ->
                go (M.insertWith (++) (fromRegVarToFreeVarsTy $ Old.regionToVar r) (S.toList $ allFreeVars rhs) acc) rhs
              Old.LetParRegionE r _ _ rhs ->
                go (M.insertWith (++) (fromRegVarToFreeVarsTy $ Old.regionToVar r) (S.toList $ allFreeVars rhs) acc) rhs
              Old.LetLocE loc phs rhs  ->
                go (M.insertWith (++) (fromLocVarToFreeVarsTy loc) (dep phs ++ (S.toList $ allFreeVars rhs)) acc) rhs
              Old.RetE{}         -> acc
              Old.FromEndE{}     -> acc
              Old.BoundsCheck{}  -> acc
              Old.IndirectionE{} -> acc
              Old.AddFixed v _   -> M.insertWith (++) (fromVarToFreeVarsTy v) [(fromVarToFreeVarsTy v)] acc
              Old.GetCilkWorkerNum -> acc
              Old.LetAvail _ bod -> go acc bod
              Old.AllocateTagHere{} -> acc
              Old.AllocateScalarsHere{} -> acc
              Old.SSPush{} -> acc
              Old.SSPop{} -> acc
              Old.StartOfPkdCursor cur -> M.insertWith (++) (fromVarToFreeVarsTy cur) [(fromVarToFreeVarsTy cur)] acc
              Old.TagCursor a b -> M.insertWith (++) (fromVarToFreeVarsTy b) [(fromVarToFreeVarsTy b)] (M.insertWith (++) (fromVarToFreeVarsTy a) [(fromVarToFreeVarsTy a)] acc)

      dep :: Old.PreLocExp LocArg -> [FreeVarsTy]
      dep ex =
        case ex of
          Old.StartOfRegionLE r -> [fromRegVarToFreeVarsTy $ Old.regionToVar r]
          Old.AfterConstantLE _ loc   -> [fromLocVarToFreeVarsTy $ toLocVar loc]
          Old.AfterVariableLE v loc _ -> [fromVarToFreeVarsTy v, fromLocVarToFreeVarsTy $ toLocVar loc]
          Old.InRegionLE r  -> [fromRegVarToFreeVarsTy $ Old.regionToVar r]
          Old.FromEndLE loc -> [fromLocVarToFreeVarsTy $ toLocVar loc]
          Old.FreeLE -> []

-- gFreeVars ++ locations ++ region variables
allFreeVars :: Exp2 -> S.Set FreeVarsTy
allFreeVars ex =
  case ex of
    AppE _ locs args -> S.fromList (map (fromLocVarToFreeVarsTy . toLocVar) locs) `S.union` (S.unions (map allFreeVars args))
    PrimAppE _ args -> (S.unions (map allFreeVars args))
    LetE (v,locs,_,rhs) bod -> (S.fromList (map (fromLocVarToFreeVarsTy . toLocVar) locs) `S.union` (allFreeVars rhs) `S.union` (allFreeVars bod))
                               `S.difference` S.singleton (fromVarToFreeVarsTy v)
    IfE a b c -> allFreeVars a `S.union` allFreeVars b `S.union` allFreeVars c
    MkProdE args -> (S.unions (map allFreeVars args))
    ProjE _ bod -> allFreeVars bod
    CaseE scrt brs -> (allFreeVars scrt) `S.union` (S.unions (map (\(_,vlocs,c) -> allFreeVars c `S.difference`
                                                                                   S.fromList (map (fromVarToFreeVarsTy . fst) vlocs) `S.difference`
                                                                                   S.fromList (map (fromLocVarToFreeVarsTy . toLocVar . snd) vlocs))
                                                                  brs))
    DataConE loc _ args -> S.singleton ((fromLocVarToFreeVarsTy . toLocVar) loc) `S.union` (S.unions (map allFreeVars args))
    TimeIt e _ _ -> allFreeVars e
    WithArenaE _ e -> allFreeVars e
    SpawnE _ locs args -> S.fromList (map (fromLocVarToFreeVarsTy . toLocVar) locs) `S.union` (S.unions (map allFreeVars args))
    Ext ext ->
      case ext of
        Old.LetRegionE r _ _ bod -> S.delete ((fromRegVarToFreeVarsTy . Old.regionToVar) r) (allFreeVars bod)
        Old.LetParRegionE r _ _ bod -> S.delete ((fromRegVarToFreeVarsTy . Old.regionToVar) r) (allFreeVars bod)
        Old.LetLocE loc locexp bod -> S.difference ((S.singleton . fromLocVarToFreeVarsTy) loc) (allFreeVars bod `S.union` (S.map fromVarToFreeVarsTy $ gFreeVars locexp))
        Old.StartOfPkdCursor v -> S.singleton (fromVarToFreeVarsTy v)
        Old.TagCursor a b-> S.fromList [(fromVarToFreeVarsTy a),(fromVarToFreeVarsTy b)]
        Old.RetE locs v     -> S.insert (fromVarToFreeVarsTy v) (S.fromList (map (fromLocVarToFreeVarsTy . toLocVar) locs))
        Old.FromEndE loc    -> S.singleton ((fromLocVarToFreeVarsTy . toLocVar) loc)
        Old.BoundsCheck _ reg cur -> S.fromList (map (fromLocVarToFreeVarsTy . toLocVar) [reg, cur])
        Old.IndirectionE _ _ (a,b) (c,d) _ -> S.fromList (map (fromLocVarToFreeVarsTy . toLocVar) [a, b, c, d])
        Old.AddFixed v _    -> S.singleton (fromVarToFreeVarsTy v)
        Old.GetCilkWorkerNum-> S.empty
        Old.LetAvail vs bod -> S.fromList (L.map fromVarToFreeVarsTy vs) `S.union` (S.map fromVarToFreeVarsTy $ gFreeVars bod)
        Old.AllocateTagHere loc _ -> S.singleton $ fromLocVarToFreeVarsTy loc
        Old.AllocateScalarsHere loc -> S.singleton $ fromLocVarToFreeVarsTy loc
        Old.SSPush _ a b _ -> S.fromList (map fromLocVarToFreeVarsTy [a,b])
        Old.SSPop _ a b -> S.fromList (map fromLocVarToFreeVarsTy [a,b])
    _ -> (S.map fromVarToFreeVarsTy $ gFreeVars ex)

freeLocVars :: Exp2 -> [LocVar]
freeLocVars ex = L.map getLocVarFromFreeVarsTy (S.toList $ (allFreeVars ex))
