{-# LANGUAGE OverloadedStrings #-}

module Packed.FirstOrder.Passes.InfRegions where

import Data.Loc
import Data.List as L
import qualified Data.Map as M

import Packed.FirstOrder.Common hiding (FunDef(..))
import Packed.FirstOrder.L1.Syntax hiding (Prog(..), FunDef(..))
import Packed.FirstOrder.L2.Syntax as L2

{- Note [Infinite regions]
~~~~~~~~~~~~~~~~~~~~~~~~~~

Instead of allocating a single global region (4GB right now), the compiler would
use a linked list of small regions which are allocated as required. Such regions
are referred to as "Infinite" in Gibbon. And the old regions are now "BigInfinite".
The size of the first region in the chain would typically be small. And subsequent
allocations double the size everytime (upto a certain limit).
For more details, refer https://github.com/iu-parfunc/gibbon/issues/79.

Since every region now has a boundary (an end), we have to ensure that we don't
cross that. This "bounds-check" has to be done before every write op (almost).


Bounds-check: Where?
~~~~~~~~~~~~~~~~~~~~

We only have to do a bounds-check when we're constructing a datatype with atleast 1
scalar value. Eg. `(Leaf 10)` or `(A 10 20 (B ...))`. But not for datatypes like
`(Node (Leaf ..) (Node ..))`. The reason is that constructing a `Node` just involves
writing a tag *before* it's children. So it's OK to assume that this write will
be safe. (TODO: Is it actually safe ?)


Bounds-check: How?
~~~~~~~~~~~~~~~~~~

To implement this, we have to tweak the cursor passing style a bit.


    -- char*
    type Cursor = Ptr Char
    type Region = Cursor

    add1 :: Region -> Region -> Cursor -> Cursor
         -> (Region, Region, Cursor, (Cursor, Cursor))
    add1 reg end_reg lout lin =
      let tag = readTag lin
      in case tag of
           Leaf -> let n  = readInt tag
                       (reg1, end_reg1, lout1) = bounds_check reg end_reg lout 9
                       wt = writeTag lout1 Leaf
                       wi = writeInt wt   (n+1)
                   in (reg1, end_reg1, lin + 8, (lout1, wi))
           Node -> ...

    bounds_check :: Region -> Region -> Cursor -> Int -> (Region, Region, Cursor)
    bounds_check reg end_reg cur size =
      if (end_reg - cur) > size
      then (reg, end_reg, cur)
      else
        do new_reg <- allocate new_reg_size
           redirect cur new_reg
           return (new_reg, new_reg + new_reg_size, new_reg)


Every function takes in an additional (reg_start, reg_end) for every packed type
in the return value (i.e 1 pair per output cursor). The reg_end is used to
check if the current write cursor is within the region boundary. If it is,
we just continue using the current region. Otherwise, we allocate a new region,
write a "redirection" at the current cursor location and use new fresh region
for subsequent writes. The reg_start is used to calculate the size of the current
region, where `size_region = end_reg - start_reg`. Most of the work will be done
during codegen. This pass just inserts the `BoundsCheck` with correct args.


To thread through the region arguments:

(1) Region vars are prepended to the locations that AppE forms accept,
    and the corresponding let bindings are updated to accept region return values.

        LetE (x,[endof_lin], Packed, AppE "add1" [lin,lout] arg)

    becomes

        LetE (x,[reg2, end_reg2, endof_lin], Packed, AppE "add1" [reg1, reg2, lin, lout] arg)

(2) RetE forms returning packed values are modified to return region arguments,
    in addition to the "endof" locations.

        RetE [endof1] arg

    becomes

        RetE [reg2, end_reg2, endof1] arg

-}

-- Maps a location to a region
type LocEnv = M.Map LocVar Var

type TypeEnv = M.Map Var Ty2

infRegions :: L2.Prog -> SyM L2.Prog
infRegions Prog{ddefs,fundefs,mainExp} = do
  fds' <- mapM (infRegionsFn ddefs fundefs) $ M.elems fundefs
  let fundefs' = M.fromList $ map (\f -> (funname f,f)) fds'
  mainExp' <- case mainExp of
                Nothing -> return Nothing
                Just (mn, ty) -> Just . (,ty) <$>
                  infRegionsExp ddefs fundefs True M.empty M.empty mn
  return $ Prog ddefs fundefs' mainExp'

infRegionsFn :: DDefs Ty2 -> NewFuns -> L2.FunDef -> SyM L2.FunDef
infRegionsFn ddefs fundefs f@FunDef{funarg,funty,funbod} = do
  let initLocEnv = M.fromList $ map (\(LRM lc r _) -> (lc, regionVar r)) (locVars funty)
      initTyEnv  = M.singleton funarg (arrIn funty)
  bod' <- infRegionsExp ddefs fundefs False initLocEnv initTyEnv funbod
  return $ f {funbod = bod'}

infRegionsExp :: DDefs Ty2 -> NewFuns -> Bool -> LocEnv -> TypeEnv -> L L2.Exp2
              -> SyM (L L2.Exp2)
infRegionsExp ddefs fundefs isMain lenv tenv (L p ex) = L p <$>
  case ex of
    LetE (v,locs,ty, rhs@(L _ (AppE f applocs arg))) bod
      | hasPacked ty -> do
          let tylocs = getTyLocs ty
              regs   = map (lenv M.!) tylocs
          regs' <- mapM gensym regs
          -- Update all locations to point to the fresh region
          let lenv' = foldr (\(lc,r,r') acc ->
                               M.insert lc r' $
                               M.map (\tyl -> if tyl == r then r' else tyl) acc)
                      lenv
                      (L.zip3 tylocs regs regs')
              newlocs    = (concat $ map (\r -> [r, toEndV r]) regs') ++ locs
              newapplocs = (concat $ map (\r -> [r, toEndV r]) regs)  ++ applocs
          LetE (v, newlocs, ty, l$ AppE f newapplocs arg) <$>
            infRegionsExp ddefs fundefs isMain lenv' (M.insert v ty tenv) bod

      | otherwise ->
          LetE (v,locs,ty, rhs) <$>
            infRegionsExp ddefs fundefs isMain lenv (M.insert v ty tenv) bod

    LetE (v,locs,ty, rhs) bod ->
      LetE (v,locs,ty, rhs) <$>
        infRegionsExp ddefs fundefs isMain lenv (M.insert v ty tenv) bod

    Ext ext ->
      case ext of
        -- Update lenv with a binding for loc
        LetLocE loc rhs bod -> do
          let reg = case rhs of
                      StartOfLE r  -> regionVar r
                      InRegionLE r -> regionVar r
                      AfterConstantLE _ lc -> lenv M.! lc
                      AfterVariableLE _ lc -> lenv M.! lc
                      FromEndLE lc         -> lenv M.! lc
          Ext <$> LetLocE loc rhs <$>
            infRegionsExp ddefs fundefs isMain (M.insert loc reg lenv) tenv bod

        -- Implements (2)
        RetE locs v -> do
          let ty = tenv M.! v
          if not isMain && hasPacked ty
          then do
            let tylocs  = getTyLocs ty
                regs    = map (lenv M.!) tylocs
                newlocs = concat $ map (\r -> [r, toEndV r]) regs
            return $ Ext $ RetE (newlocs ++ locs) v
          else return $ Ext ext

        LetRegionE r bod -> Ext <$> LetRegionE r <$> go bod
        FromEndE{} -> return $ Ext ext

    -- Straightforward recursion

    VarE{}     -> return ex
    LitE{}     -> return ex
    LitSymE{}  -> return ex
    AppE{}     -> return ex
    PrimAppE{} -> return ex
    DataConE{} -> return ex
    ProjE i e  -> ProjE i <$> go e
    IfE a b c  -> IfE <$> go a <*> go b <*> go c
    MkProdE ls -> MkProdE <$> mapM go ls
    CaseE scrt mp -> do
      let L _ (VarE v) = scrt
          PackedTy _ tyloc = tenv M.! v
          reg = lenv M.! tyloc
      CaseE scrt <$> mapM (docase reg lenv tenv) mp
    TimeIt e ty b -> do
      e' <- go e
      return $ TimeIt e' ty b
    MapE{}  -> error $ "go: TODO MapE"
    FoldE{} -> error $ "go: TODO FoldE"

  where
    go = infRegionsExp ddefs fundefs isMain lenv tenv
    toEndV = varAppend "end_"

    docase reg lenv1 tenv1 (dcon,vlocs,bod) = do
      -- Update the envs with bindings for pattern matched variables and locations.
      -- The locations point to the same region as the scrutinee.
      let (vars,locs) = unzip vlocs
          lenv1' = foldr (\lc acc -> M.insert lc reg acc) lenv1 locs
          tys = lookupDataCon ddefs dcon
          tenv1' = foldr (\(x,ty) acc -> M.insert x ty acc) tenv1 (zip vars tys)
      (dcon,vlocs,) <$> (infRegionsExp ddefs fundefs isMain lenv1' tenv1' bod)
