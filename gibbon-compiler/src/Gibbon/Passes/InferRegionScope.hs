-- | Infer region multiplicities
--
--   During inference, regions are merely annotated with a region metavariable.
--   InferRegionScope takes the next step and decides the region scope (global/dynamic)
--   and also assigns it a multiplicity.

module Gibbon.Passes.InferRegionScope
  (inferRegScope, inferRegScopeExp) where

import Data.Graph
import qualified Data.Map as M

import Gibbon.DynFlags
import Gibbon.Common
import Gibbon.L2.Syntax

-- All regions are "infinite" right now

-- | Infer region scope and multiplicity for a program annotated with regions
-- and locations.
inferRegScope :: Prog2 -> PassM Prog2
inferRegScope Prog{ddefs,fundefs,mainExp} = do
  fds' <- mapM inferRegScopeFun $ M.elems fundefs
  let fundefs' = M.fromList $ map (\f -> (funName f,f)) fds'
  mainExp' <- case mainExp of
                Nothing -> return Nothing
                Just (mn, ty) -> Just <$> (,ty) <$> inferRegScopeExp M.empty mn
  return $ Prog ddefs fundefs' mainExp'

inferRegScopeFun :: FunDef2 -> PassM FunDef2
inferRegScopeFun f@FunDef{funBody} = do
  funBody' <- inferRegScopeExp M.empty funBody
  return $ f {funBody = funBody'}

{- Region scoping rules:
~~~~~~~~~~~~~~~~~~~~~~~~

A region in Gibbon can either be global or dynamic (local). Global regions have a
lifetime equal to that of the whole program. Whereas, dynamic regions can be created &
destroyed on the stack. The scope of the values in the region dictate it's scope.
If the value in the region escapes the context it was created in, it has be global.
Otherwise, a dynamic region would be sufficient. Consider two functions of the form:

    fnA = letregion ra in
          letloc la in
          let x = (Leaf 1) at la
          in x

    fnB = letregion rb in
          letloc lb in
          let y = (Leaf 1) at lb
          in 1

The return value of fnA resides in the region `ra`. Therefore `ra` has to be global.
Whereas, it's safe to mark the region `rb` as dynamic. The fn `inferRegScopeExp` traverses
the AST and transforms all region metavariables to fully scoped region variables.

Whenever it encounters a `letregion`, it builds a graph of dependencies of the
relevant part of the AST. And if there's a path between the region and
the end of the AST, the return value so to speak, the region is marked global.
For example, in fnA, it discovers the path `ra -> la -> x` and hence `ra` is global.
In fnB, there's no path from `rb` to 1.

-}


mkScopedRegion :: Region -> Multiplicity -> Region 
mkScopedRegion r m = 
  case r of 
    SoAR dcr fieldRegs -> let scopedDcr = case dcr of 
                                                VarR dcrVar -> GlobR dcrVar m
                                                _ -> error "Did not handle SoAR in inferRegScopeExp."
                              scopedFieldRegs = map (\(t, r') -> case r of
                                                                    _ -> (t, mkScopedRegion r' m)
                                                                
                                                    ) fieldRegs
                           in SoAR scopedDcr scopedFieldRegs
    VarR reg -> GlobR reg m
    _ -> error "Did not handle region in inferRegScopeExp (mkScopedRegion)."


inferRegScopeExpHelper :: Exp2 -> Exp2 -> Region -> M.Map Region Region -> PassM Exp2 
inferRegScopeExpHelper ex rhs r env = 
  let deps = depList ex
    in case deps of
      ((retVar,_,_):_) ->
        let (g,_,vtxF) = graphFromEdges deps
          in case r of 
            -- VS: TODO: Handle case then field Regions also contain more factored out SoA regions.
            SoAR dcr fieldRegs -> let regVLoc = R $ regionToVar r 
                                      regVertex = case vtxF regVLoc of
                                        Just x  -> x
                                        Nothing -> error $ "No vertex for:" ++ sdoc r
                                      retVertex = case vtxF retVar of
                                        Just x  -> x
                                        Nothing -> error $ "No vertex for:" ++ sdoc retVar
                                      -- The value in the region  escapes the current scope if there's
                                      -- a path between the region variable and the thing returned.
                                    in do dflags <- getDynFlags 
                                          let defaultMul = if (gopt Opt_BigInfiniteRegions dflags) ||
                                                       (gopt Opt_Gibbon1 dflags)
                                                    then BigInfinite
                                                    else Infinite
                                          let isPath = path g retVertex regVertex
                                          -- Not handling recursive SoA Regions
                                          -- let scopedDcr = case dcr of 
                                          --                     VarR dcrVar -> GlobR dcrVar defaultMul
                                          --                     _ -> error "Did not handle SoAR in inferRegScopeExp."
                                          -- let scopedFieldRegs = map (\(t, r) -> case r of 
                                          --                                           VarR fvar -> (t, GlobR fvar defaultMul)
                                          --                                           _ -> error "Did not handle SoAR in inferRegScopeExp."
                                                                
                                          --                             ) fieldRegs
                                          -- let scoped_reg = SoAR scopedDcr scopedFieldRegs
                                          let scoped_reg = mkScopedRegion r defaultMul
                                          Ext <$> LetRegionE scoped_reg Undefined Nothing <$> inferRegScopeExp (M.insert r scoped_reg env) rhs 
            _ -> let regV = regionToVar r
                     regVToVar = case regV of
                        SingleR v -> v
                        SoARv _ _ -> error "inferRegScopeExp: did not expect an SoA region!"
                     regVLoc = R $ regionToVar r
                     -- Vertex of the region variable
                     regVertex = case vtxF regVLoc of
                        Just x  -> x
                        Nothing -> error $ "No vertex for:" ++ sdoc r
                     -- Vertex of the return value
                     retVertex = case vtxF retVar of
                        Just x  -> x
                        Nothing -> error $ "No vertex for:" ++ sdoc retVar
                     -- The value in the region  escapes the current scope if there's
                     -- a path between the region variable and the thing returned.
                     -- TODO: Warn the user when this happens in a fn ?
                  in do dflags <- getDynFlags
                        let defaultMul = if (gopt Opt_BigInfiniteRegions dflags) ||
                                     (gopt Opt_Gibbon1 dflags)
                                  then BigInfinite
                                  else Infinite
                        let scoped_reg = if path g retVertex regVertex
                                  then (GlobR regVToVar defaultMul)
                                  -- [2018.03.30] - TEMP: Turning off scoped buffers.
                                  -- else Ext$ LetRegionE (DynR regV mul) (inferRegScopeExp rhs)
                                  -- else (DynR regV mul)
                                  else (GlobR regVToVar defaultMul)
                        Ext <$>
                             LetRegionE scoped_reg Undefined Nothing <$>
                             inferRegScopeExp (M.insert r scoped_reg env) rhs
      [] -> return ex

-- | Decide if a region should be global or local (dynamic).
--
--  Dynamic regions are stack allocated and automatically freed
inferRegScopeExp :: M.Map Region Region -> Exp2 -> PassM Exp2
inferRegScopeExp env ex =
  case ex of
    Ext ext ->
      case ext of
        AddFixed{} -> return ex
        LetRegionE r sz ty rhs ->
          case r of
            -- SoAR dcr fieldRegs -> reti
            MMapR{} -> Ext . LetRegionE r sz ty <$> go rhs
            _ -> inferRegScopeExpHelper ex rhs r env

        LetParRegionE r sz ty rhs ->
          case r of
            SoAR dcr fieldRegs -> error "Did not handle SoAR in inferRegScopeExp."
            MMapR{} -> Ext . LetParRegionE r sz ty <$> go rhs
            _ ->
              let deps = depList ex
              in case deps of
                   ((retVar,_,_):_) ->
                     let (g,_,vtxF) = graphFromEdges deps
                         regV = regionToVar r
                         regVtoVar = case regV of
                           SingleR v -> v
                           SoARv _ _ -> error "inferRegScopeExp: did not expect an SoA region!"
                         regVLoc = R $ regionToVar r
                         -- Vertex of the region variable
                         regVertex =
                           case vtxF regVLoc of
                             Just x  -> x
                             Nothing -> error $ "No vertex for:" ++ sdoc r
                         -- Vertex of the return value
                         retVertex =
                           case vtxF retVar of
                             Just x  -> x
                             Nothing -> error $ "No vertex for:" ++ sdoc retVar
                         -- The value in the region  escapes the current scope if there's
                         -- a path between the region variable and the thing returned.
                         -- TODO: Warn the user when this happens in a fn ?
                     in do dflags <- getDynFlags
                           let defaultMul = if (gopt Opt_BigInfiniteRegions dflags) ||
                                               (gopt Opt_Gibbon1 dflags)
                                            then BigInfinite
                                            else Infinite
                           let scoped_reg = if path g retVertex regVertex
                                            then (GlobR regVtoVar defaultMul)
                                            -- [2018.03.30] - TEMP: Turning off scoped buffers.
                                            -- else Ext$ LetRegionE (DynR regV mul) (inferRegScopeExp rhs)
                                            -- else (DynR regV mul)
                                            else (GlobR regVtoVar defaultMul)
                           Ext <$> LetParRegionE (GlobR regVtoVar defaultMul) Undefined Nothing <$>
                                   (inferRegScopeExp (M.insert r scoped_reg env) rhs)
                   [] -> return ex

        -- Straightforward recursion
        LetLocE loc le bod -> do
          let le' = case le of
                      StartOfRegionLE r -> StartOfRegionLE (env # r)
                      InRegionLE r -> InRegionLE (env # r)
                      _ -> le
          Ext <$> LetLocE loc le' <$> (go bod)
        StartOfPkdCursor{}-> return ex
        TagCursor{}-> return ex
        RetE{}     -> return ex
        FromEndE{} -> return ex
        BoundsCheck{} -> return ex
        IndirectionE{}-> return ex
        GetCilkWorkerNum -> return ex
        LetAvail vs e    -> Ext <$> LetAvail vs <$> go e
        AllocateTagHere{} -> return ex
        AllocateScalarsHere{} -> return ex
        SSPush{} -> return ex
        SSPop{} -> return ex

    -- Straightforward recursion ...
    VarE{}     -> return ex
    LitE{}     -> return ex
    CharE{}    -> return ex
    FloatE{}   -> return ex
    LitSymE{}  -> return ex
    AppE{}     -> return ex
    PrimAppE{} -> return ex
    DataConE{} -> return ex
    ProjE i e  -> ProjE i <$> go e
    IfE a b c  -> (IfE a) <$> go b <*> go c
    MkProdE ls -> MkProdE <$> mapM go ls
    LetE (v,locs,ty,rhs) bod -> LetE <$> (v,locs,ty,) <$> go rhs <*> go bod
    CaseE scrt mp -> (CaseE scrt) <$> mapM (\(a,b,c) -> (a,b,) <$> go c) mp
    TimeIt e ty b -> do
      e' <- go e
      return $ TimeIt e' ty b
    SpawnE{} -> pure ex
    SyncE{}  -> pure ex
    WithArenaE v e -> WithArenaE v <$> go e
    MapE{}  -> error $ "inferRegScopeExp: TODO MapE"
    FoldE{} -> error $ "inferRegScopeExp: TODO FoldE"
  where
    go = inferRegScopeExp env
