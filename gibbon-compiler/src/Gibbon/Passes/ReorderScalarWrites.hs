{-# LANGUAGE RecordWildCards #-}

module Gibbon.Passes.ReorderScalarWrites
  ( reorderScalarWrites, writeOrderMarkers )
  where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Graph as G
import           Data.Maybe ( fromJust )
import           Text.PrettyPrint.GenericPretty
import           Text.PrettyPrint ( text )

import           Gibbon.Common hiding ( Mode )
import           Gibbon.Language
import qualified Gibbon.L2.Syntax as L2
import qualified Gibbon.L3.Syntax as L3
import qualified Safe as Sf
import Data.Foldable (foldrM)

--------------------------------------------------------------------------------

-- | Inserts markers which tells subsequent a compiler pass where to
-- move the tag and scalar field allocations so that they happen
-- before any of the subsequent packed fields.
writeOrderMarkers :: L2.Prog2 -> PassM L2.Prog2
writeOrderMarkers (Prog ddefs fundefs mainExp) = do
    fds' <- mapM gofun (M.elems fundefs)
    let fundefs' = M.fromList $ map (\f -> (funName f,f)) fds'
    mainExp' <- case mainExp of
                    Just (e,ty) -> do let env2 = Env2 M.empty (initFunEnv fundefs)
                                      e' <- go M.empty M.empty M.empty env2 e
                                      pure $ Just (e', ty)
                    Nothing     -> pure Nothing
    pure $ Prog ddefs fundefs' mainExp'

  where
    gofun f@FunDef{funArgs,funBody,funTy} = do
        let (reg_env, alloc_env) =
              foldr (\(L2.LRM loc reg mode) (renv,aenv) ->
                            let renv' = M.insert loc reg renv
                             in case reg of
                              L2.SoAR dcr fieldRegs -> 
                                                       let renv'' = case loc of 
                                                                  Single _ -> error $ "gofun: did not expect a SoA location for an SoA region."
                                                                  SoA dcloc fieldLocs ->
                                                                                         let renvl = M.insert (Single dcloc) dcr renv'
                                                                                             renvl' = foldr (\(k, floc) acc -> do
                                                                                                                               let freg = case lookup k fieldRegs of 
                                                                                                                                             Just freg -> freg
                                                                                                                                             Nothing -> error "gofun: expected a field region for corresponding key in SoAR."
                                                                                                                               let acc' = M.insert floc freg acc
                                                                                                                                in acc'   
                                                                                                      
                                                                                                            ) renvl fieldLocs
                                                                                           in renvl'
                                                           aenv' = case mode of
                                                                        L2.Output ->
                                                                            let aenvl = M.insert reg (RegionLocs [loc] S.empty) aenv
                                                                              in case loc of 
                                                                                    Single _ -> aenvl
                                                                                    SoA dcloc fieldLocs -> 
                                                                                         let aenvl' = foldr (\(k, floc) acc -> do
                                                                                                                               let freg = case lookup k fieldRegs of 
                                                                                                                                             Just freg -> freg
                                                                                                                                             Nothing -> error "gofun: expected a field region for corresponding key in SoAR."
                                                                                                                               let acc' = M.insert freg (RegionLocs [floc] S.empty) acc
                                                                                                                                in acc'   
                                                                                                      
                                                                                                            ) aenvl fieldLocs
                                                                                           in aenvl'
                                                                            
                                                                        L2.Input -> aenv
                                                         in (renv'', aenv')
                                                       
                              _ -> let renv' = M.insert loc reg renv
                                       aenv' = case mode of
                                                    L2.Output ->
                                                      let reg_locs = RegionLocs [loc] S.empty
                                                        in M.insert reg reg_locs aenv
                                                    L2.Input -> aenv
                                     in (renv',aenv')
                    )
                    (M.empty,M.empty)
                    (L2.locVars funTy)
            init_ty_env  = M.fromList $ zip funArgs (L2.arrIns funTy)
            env2 = Env2 init_ty_env (initFunEnv fundefs)
        funBody' <- go reg_env alloc_env M.empty env2 funBody
        pure $ f { funBody = funBody' }

    go :: RegEnv -> AllocEnv -> StoreEnv -> Env2 Var L2.Ty2 -> L2.Exp2 -> PassM L2.Exp2
    go reg_env alloc_env store_env env2 ex =
      case ex of
        LetE (v,locs,ty,rhs) bod -> do
          let env2' = extendVEnv v ty env2
          case (L2.locsInTy ty) of
            [] -> (LetE (v,locs,ty,rhs)) <$> (go reg_env alloc_env store_env env2' bod) -- simple recursion on let body
            [one] -> let (is_ok, locs_before, reg, (RegionLocs rlocs allocated)) = isAllocationOk one rhs bod
                         reg_env' = foldr (\loc acc -> M.insert loc reg acc) reg_env locs
                         alloc_env' =
                           M.insert reg
                             (RegionLocs rlocs (S.insert one (S.fromList locs_before `S.union` allocated)))
                             alloc_env
                     in if is_ok
                        then (LetE (v,locs,ty,rhs)) <$> (go reg_env' alloc_env' store_env env2' bod)
                        else do
                          let tag_loc = Sf.headErr locs_before
                          let tag_tycon = findTyCon tag_loc bod
                          let in_scope = M.keysSet (vEnv env2) `S.union` M.keysSet (fEnv env2)
                              (move_set,move_scalars) = checkScalarDeps ddefs in_scope tag_loc ex
                              move_scalars_easy = move_scalars && S.null move_set
                              store_env' = foldr (\x acc -> M.insert x tag_loc acc) store_env (S.toList move_set)
                          alloc_tag_here <- gensym "alloc_tag_here"
                          alloc_scalars_here <- gensym "alloc_scalars_here"
                          LetE (alloc_tag_here,[],ProdTy [],Ext $ L2.AllocateTagHere tag_loc tag_tycon) <$>
                            (if move_scalars_easy
                             then
                               LetE (alloc_scalars_here,[],ProdTy [],Ext $ L2.AllocateScalarsHere tag_loc) <$>
                               LetE (v,locs,ty,rhs) <$>
                                 go reg_env' alloc_env' store_env' env2' bod
                             else
                               LetE (v,locs,ty,rhs) <$>
                                 go reg_env' alloc_env' store_env' env2' bod)

            ls -> error $ "writeOrderMarkers: encountered allocation to more" ++
                          " than one output location; " ++ sdoc ls ++ " in " ++ sdoc ex

        Ext ext ->
          case ext of
            L2.LetRegionE reg sz ty bod -> do
              let alloc_env' = M.insert reg (RegionLocs [] S.empty) alloc_env
              case reg of 
                  L2.SoAR dcr fieldRegs -> do
                                           let alloc_env'' = M.insert dcr (RegionLocs [] S.empty) alloc_env'
                                           alloc_env''' <- foldrM (\(_, freg) acc -> do
                                                                 let acc' = M.insert freg (RegionLocs [] S.empty) acc
                                                                 return acc'
                                                                 ) alloc_env'' fieldRegs
                                           Ext <$> (L2.LetRegionE reg sz ty) <$> go reg_env alloc_env''' store_env env2 bod
                                           -- dbgTraceIt "Print allocEnv " dbgTraceIt (sdoc alloc_env''') dbgTraceIt "End allocEnv\n"
                                           
                  _ -> Ext <$> (L2.LetRegionE reg sz ty) <$> go reg_env alloc_env' store_env env2 bod
              --alloc_env'' <- foldrM () alloc_env' reg
              --Ext <$> (L2.LetRegionE reg sz ty) <$> go reg_env alloc_env' store_env env2 bod
            L2.LetParRegionE reg sz ty bod -> do
              let alloc_env' = M.insert reg (RegionLocs [] S.empty) alloc_env
              Ext <$> (L2.LetParRegionE reg sz ty) <$> go reg_env alloc_env' store_env env2 bod
            L2.LetLocE loc L2.FreeLE bod -> do
              Ext <$> (L2.LetLocE loc L2.FreeLE) <$> (go reg_env alloc_env store_env env2 bod)
            L2.StartOfPkdCursor cur -> pure $ Ext $ L2.StartOfPkdCursor cur
            L2.TagCursor a b -> pure $ Ext $ L2.TagCursor a b
            L2.LetLocE loc rhs bod -> do
              let (reg, reg_env') = case rhs of
                      L2.StartOfRegionLE r  -> (r, M.insert loc r reg_env)
                      L2.InRegionLE r -> (r, M.insert loc r reg_env)
                      L2.AfterConstantLE _ lc   -> let r = reg_env # lc
                                                    in (r, M.insert loc r reg_env)
                      L2.AssignLE lc -> let r = reg_env # lc
                                         in (r, M.insert loc r reg_env)
                      L2.AfterVariableLE _ lc _ -> let r = reg_env # lc
                                                    in (r, M.insert loc r reg_env)
                      L2.FromEndLE lc           -> let r = reg_env # lc
                                                     in (r, M.insert loc r reg_env)
                      -- TODO: VS we should not use unwrap LocVar, LocVar should be recursive
                      L2.GenSoALoc dlc flocs -> let dlcr = reg_env # dlc
                                                    fieldRegs = map (\(d, flc) -> let flcr = reg_env # flc
                                                                                   in (d, flcr)
                                                                    ) flocs
                                                    soa_loc = SoA (unwrapLocVar dlc) (map (\(d, flc) -> (d, flc)) flocs)
                                                    soa_reg = L2.SoAR dlcr fieldRegs
                                                  in (soa_reg, M.insert soa_loc soa_reg reg_env)
                      L2.GetDataConLocSoA lc -> let soa_reg = reg_env # lc 
                                                    dcon_reg = case soa_reg of 
                                                                    L2.SoAR dreg _ -> dreg
                                                  in (dcon_reg, M.insert (getDconLoc lc) dcon_reg reg_env)
                      L2.GetFieldLocSoA (dcon, idx) lc -> let soa_reg = reg_env # lc 
                                                              field_reg = case soa_reg of 
                                                                                L2.SoAR _ fregs -> case lookup (dcon, idx) fregs of 
                                                                                                    Just freg -> freg
                                                                                                    Nothing -> error "writeOrderMarkers: GetFieldLocSoA: data constructor not found!"
                                                            in (field_reg, M.insert (getFieldLoc (dcon, idx) lc) field_reg reg_env)
                  --reg_env' = M.insert loc reg reg_env
              case M.lookup reg alloc_env of
                Nothing -> do 
                           -- dbgTraceIt "Print reg_env before case: " dbgTraceIt (sdoc (reg_env, reg_env')) dbgTraceIt "End1 reg_env\n"
                           bod' <- go reg_env' alloc_env store_env env2 bod
                           return $ Ext $ (L2.LetLocE loc rhs) bod'
                Just (RegionLocs locs allocated) -> do
                  -- dbgTraceIt "Print reg_env before case: " dbgTraceIt (sdoc (reg_env, reg_env')) dbgTraceIt "End2 reg_env\n"
                  let reg_locs = RegionLocs (locs ++ [loc]) allocated
                      alloc_env' = M.insert reg reg_locs alloc_env
                  Ext <$> (L2.LetLocE loc rhs) <$> (go reg_env' alloc_env' store_env env2 bod)

            L2.RetE{} -> pure ex
            L2.FromEndE{} -> pure ex
            L2.BoundsCheck{} -> pure ex
            L2.AddFixed{} -> pure ex
            L2.IndirectionE{} -> pure ex
            L2.GetCilkWorkerNum{} -> pure ex
            L2.LetAvail vars bod -> Ext <$> (L2.LetAvail vars) <$> recur bod
            L2.AllocateTagHere{} -> pure ex
            L2.AllocateScalarsHere{} -> pure ex
            L2.SSPush{} -> pure ex
            L2.SSPop{} -> pure ex

        -- straightforward recursion (assumption: a-normal form)
        VarE{}     -> pure ex
        LitE{}     -> pure ex
        CharE{}    -> pure ex
        FloatE{}   -> pure ex
        LitSymE{}  -> pure ex
        AppE{}     -> pure ex
        PrimAppE{} -> pure ex
        IfE a b c  -> IfE <$> recur a <*> recur b <*> recur c
        MkProdE{}  -> pure ex
        ProjE{}    -> pure ex
        CaseE scrt brs -> do
          let (VarE v) = scrt
              PackedTy _ tyloc = lookupVEnv v env2
              reg = reg_env M.! tyloc
          brs' <- mapM (\(dcon,vlocs,rhs) -> do
                           -- Update the envs with bindings for pattern matched variables and locations.
                           -- The locations point to the same region as the scrutinee.
                           let (vars,locs) = unzip vlocs
                               reg_env' = foldr (\lc acc -> M.insert lc reg acc) reg_env locs
                               env2' = (L2.extendPatternMatchEnv dcon ddefs vars locs env2)
                           (dcon,vlocs,) <$> go reg_env' alloc_env store_env env2' rhs)
                       brs
          pure $ CaseE scrt brs'
        DataConE{} -> pure ex
        TimeIt{}   -> pure ex
        WithArenaE v e -> (WithArenaE v) <$> recur e
        SpawnE{}   -> pure ex
        SyncE      -> pure ex
        MapE{}     -> pure ex
        FoldE{}    -> pure ex
      where
        recur = go reg_env alloc_env store_env env2
        isAllocationOk loc rhs bod =
          case M.lookup loc reg_env of
            Nothing  -> error $ "writeOrderMarkers: free location " ++ sdoc loc
            Just reg -> case M.lookup reg alloc_env of
                          Nothing -> error $ "writeOrderMarkers: free region " ++ sdoc (loc, reg, rhs)
                          Just rloc@(RegionLocs locs allocated_to) ->
                            let locs_before = takeWhile (/= loc) locs in
                              case locs_before of
                                [] -> let ret = (True, locs_before, reg, rloc) 
                                        in ret
                                _  ->
                                  let freev = L2.allFreeVars rhs `S.union` L2.allFreeVars bod
                                      locs_before' = filter (\x -> S.member (fromLocVarToFreeVarsTy x) freev) locs_before
                                  in (S.isProperSubsetOf (S.fromList locs_before') allocated_to, locs_before', reg, rloc)
                                  -- dbgTraceIt "Print in isAllocationOk: " dbgTraceIt (sdoc (loc, rhs, locs_before, locs_before', allocated_to)) dbgTraceIt "End isAllocationOk.\n"

        findTyCon :: LocVar -> L2.Exp2 -> TyCon
        findTyCon want e =
          case e of
            DataConE loc dcon _ | want == loc -> getTyOfDataCon ddefs dcon
                                | otherwise -> error $ "findTyCon: " ++ show want ++ " not found. "  ++ sdoc (want,e)
            LetE (_v,_locs,PackedTy tycon loc,_rhs) bod | want == loc -> tycon
                                                        | otherwise -> findTyCon want bod
            LetE (_v,_locs,_ty,_rhs) bod -> findTyCon want bod
            IfE _ b c  -> let tycon_b = (findTyCon want b)
                              tycon_c = (findTyCon want c)
                          in if tycon_b == tycon_c
                             then tycon_b
                             else error $ "findTyCon want: types don't match"
            CaseE _scrt brs -> let tycons = foldr (\(_a,_b,c) acc -> findTyCon want c : acc) [] brs
                               in if all (== (Sf.headErr tycons)) tycons
                                  then Sf.headErr tycons
                                  else error $ "findTyCon want: types don't match"
            WithArenaE _ar bod -> (findTyCon want bod)
            TimeIt e0 _ty _b -> (findTyCon want e0)
            Ext ext ->
              case ext of
                L2.LetRegionE _ _ _ bod -> (findTyCon want bod)
                L2.LetParRegionE _ _ _ bod -> (findTyCon want bod)
                L2.LetLocE _ _ bod -> (findTyCon want bod)
                L2.LetAvail _ bod -> (findTyCon want bod)
                _ -> error $ "findTyCon: " ++ show want ++ " not found. " ++ sdoc (want,e)
            _ -> error $ "findTyCon: " ++ show want ++ " not found. " ++ sdoc (want,e)


-- | Do the values of scalar fields depend on the packed fields?
--   If they do the scalar allocations cannot be moved up.
checkScalarDeps :: L2.DDefs2 -> S.Set Var -> LocVar -> L2.Exp2 -> (S.Set Var, Bool)
checkScalarDeps ddefs in_scope tag_loc ex0 =
    let (_a,b,c) = go (foldr (\v move -> M.insert v [v] move) M.empty (S.toList in_scope)) S.empty True ex0
    in (b `S.difference` in_scope,c)
  where
    go :: M.Map Var [Var] -> S.Set Var -> Bool -> L2.Exp2 -> (M.Map Var [Var], S.Set Var, Bool)
    go dep_env move_set move ex =
      case ex of
        VarE{}     -> (dep_env,move_set,move)
        LitE{}     -> (dep_env,move_set,move)
        CharE{}    -> (dep_env,move_set,move)
        FloatE{}   -> (dep_env,move_set,move)
        LitSymE{}  -> (dep_env,move_set,move)
        AppE{}     -> (dep_env,move_set,move)
        PrimAppE{} -> (dep_env,move_set,move)
        --  -> do_dcon dep_env move loc dcon args
        LetE (v,_,_,rhs@(DataConE loc dcon args)) bod
          | loc == tag_loc -> do_dcon dep_env move_set move loc dcon args
          | otherwise ->
            let free_vars = S.toList $ gFreeVars rhs `S.difference` in_scope
                (dep_env',move_set',move') = go (M.insertWith (++) v free_vars dep_env) move_set move bod
            in (dep_env `M.union` dep_env', move_set `S.union` move_set', move && move')
        LetE (v,_locs,_ty,rhs) bod ->
          let free_vars = S.toList $ gFreeVars rhs `S.difference` in_scope
              (dep_env',move_set',move') = go (M.insertWith (++) v free_vars dep_env) move_set move bod
          in (dep_env `M.union` dep_env', move_set `S.union` move_set', move && move')
        IfE a b c  ->
          let (dep_env1,move_set1,move1) = go dep_env move_set move a
              (dep_env2,move_set2,move2) = go dep_env move_set move b
              (dep_env3,move_set3,move3) = go dep_env move_set move c
          in (dep_env `M.union` dep_env1 `M.union` dep_env2 `M.union` dep_env3,
              move_set `S.union` move_set1 `S.union` move_set2 `S.union` move_set3,
              move && move1 && move2 && move3)
        MkProdE{}  -> (dep_env,move_set,move)
        ProjE{}    -> (dep_env,move_set,move)
        CaseE scrt brs ->
          let (VarE v) = scrt in
            foldr (\(_,vlocs,rhs) (dep_env',move_set',move') ->
                     let (vars,_locs) = unzip vlocs
                         dep_env'' = M.insertWith (++) v vars dep_env'
                         (dep_env''',move_set'',move'') = go dep_env' move_set move' rhs
                     in (dep_env'' `M.union` dep_env''',
                         move_set' `S.union` move_set'',
                         move' && move''))
                  (dep_env,move_set,move)
                  brs
        DataConE loc dcon args -> do_dcon dep_env move_set move loc dcon args
        TimeIt{}   -> (dep_env,move_set,move)
        WithArenaE _ bod ->
          let (dep_env',move_set',move') = go dep_env move_set move bod
          in (dep_env' `M.union` dep_env, move_set `S.union` move_set', move && move')
        SpawnE{} -> (dep_env,move_set,move)
        SyncE    -> (dep_env,move_set,move)
        MapE{}   -> (dep_env,move_set,move)
        FoldE{}  -> (dep_env,move_set,move)
        Ext ext  ->
          case ext of
            L2.LetRegionE _ _ _ bod ->
              let (dep_env',move_set',move') = go dep_env move_set move bod
              in (dep_env' `M.union` dep_env, move_set `S.union` move_set', move && move')
            L2.LetParRegionE _ _ _ bod ->
              let (dep_env',move_set',move') = go dep_env move_set move bod
              in (dep_env' `M.union` dep_env, move_set `S.union` move_set', move && move')
            L2.LetLocE _ _ bod ->
              let (dep_env',move_set',move') = go dep_env move_set move bod
              in (dep_env' `M.union` dep_env, move_set `S.union` move_set', move && move')
            L2.LetAvail _ bod ->
              let (dep_env',move_set',move') = go dep_env move_set move bod
              in (dep_env' `M.union` dep_env, move_set `S.union` move_set', move && move')
            _ -> (dep_env, move_set, move)

    to_vertex fn (VarE v) =
      case fn v of
        Just x  -> x
        Nothing -> error $ "checkScalarDeps: No vertex for:" ++ sdoc v
    to_vertex _ e = error $ "checkScalarDeps: not in ANF " ++ sdoc e

    do_dcon dep_env move_set move loc dcon args
      | loc == tag_loc =
            -- graphFromEdges :: Ord key => [(node, key, [key])]
            --                -> (Graph, Vertex -> (node, key, [key]), key -> Maybe Vertex)
            let edges = L.map (\(a,b) -> (a,a,b)) $ M.toList dep_env
                (graph,keyFn,vtxFn) = G.graphFromEdges edges
                arg_tys = lookupDataCon ddefs dcon
                first_packed = fromJust $ L.findIndex isPackedTy arg_tys
                (scalars,packed) = splitAt first_packed args
                packed_reachable = S.fromList $
                                   concatMap (G.reachable graph) $
                                   map (to_vertex vtxFn) packed
            in case scalars of
                 [] -> (dep_env,move_set,False)
                 _ ->
                   let (move_set0, move0) =
                         foldr (\sc (move_set', move') ->
                             case sc of
                               VarE{} ->
                                 let sc_vertex = to_vertex vtxFn sc
                                     sc_reachable = S.fromList (G.reachable graph sc_vertex)
                                     move'' = move' && packed_reachable `S.disjoint` sc_reachable
                                 in if move''
                                    then ((S.map (fst3 . keyFn) sc_reachable) `S.union` move_set',
                                          move'')
                                    else (move_set', move'')
                               _ -> (move_set', move'))
                          (move_set, move)
                          scalars
                   in (dep_env, move_set0, move0)
      | otherwise = (dep_env,move_set,move)

type StoreEnv = M.Map Var LocVar
type RegEnv = M.Map LocVar L2.Region
type AllocEnv = M.Map L2.Region RegionLocs
data RegionLocs = RegionLocs { locs :: [LocVar], allocated_to :: S.Set LocVar }
  deriving (Show, Generic)

instance Out RegionLocs

--------------------------------------------------------------------------------

reorderScalarWrites :: L3.Prog3 -> PassM L3.Prog3
reorderScalarWrites (Prog ddefs fundefs mainExp) = do
    let fds' = map gofun (M.elems fundefs)
        fundefs' = M.fromList $ map (\f -> (funName f,f)) fds'
        mainExp' = case mainExp of
                    Just (e,ty) -> Just (go e, ty)
                    Nothing     -> Nothing
    pure $ Prog ddefs fundefs' mainExp'

  where
    gofun f@FunDef{funBody} =
      let funBody' = go funBody
      in f { funBody = funBody' }

    go :: L3.Exp3 -> L3.Exp3
    go ex =
      case ex of
        LetE (v,locs,ty,rhs) bod ->
          case rhs of
            Ext (L3.AllocateTagHere loc _) ->
              let (binds,bod') = collectBinds Tag loc bod
              in (mkLets binds (go bod'))
            Ext (L3.AllocateScalarsHere loc) ->
              let (binds,bod') = collectBinds Scalars loc bod
              in (mkLets binds (go bod'))
            Ext (L3.StartTagAllocation{})     -> go bod
            Ext (L3.EndTagAllocation{})       -> go bod
            Ext (L3.StartScalarsAllocation{}) -> go bod
            Ext (L3.EndScalarsAllocation{})   -> go bod
            _ -> LetE (v,locs,ty, go rhs) (go bod)
        IfE a b c  -> IfE (go a) (go b) (go c)
        CaseE scrt brs -> CaseE (go scrt) (map (\(a,b,c) -> (a,b,go c)) brs)
        WithArenaE ar bod -> WithArenaE ar (go bod)
        TimeIt e ty b -> TimeIt (go e) ty b
        _ -> ex

data Collect = Tag | Scalars
  deriving Eq

data Mode = Search L3.Exp3 | SearchAndStore L3.Exp3
  deriving (Eq, Show, Generic)

instance Out Mode where
  doc = text . show

collectBinds :: Collect -> Var -> L3.Exp3 -> ([(Var,[()],L3.Ty3,L3.Exp3)], L3.Exp3)
collectBinds collect loc ex0 =
  case collect of
    Tag -> go (Search (Ext (L3.StartTagAllocation loc))) [] ex0
    Scalars -> go (Search (Ext (L3.StartScalarsAllocation loc))) [] ex0
  where
    invert (Ext (L3.StartTagAllocation loc2)) = (Ext (L3.EndTagAllocation loc2))
    invert (Ext (L3.StartScalarsAllocation loc2)) = (Ext (L3.EndScalarsAllocation loc2))
    invert oth = error $ "collectBinds: " ++ sdoc oth

    go :: Mode -> [(Var,[()],L3.Ty3,L3.Exp3)] -> L3.Exp3
       -> ([(Var,[()],L3.Ty3,L3.Exp3)], L3.Exp3)
    go mode acc ex =
      case ex of
        LetE (v,locs,ty,rhs) bod ->
          case mode of
            Search s ->
              if s == rhs
              then go (SearchAndStore (invert s)) acc bod
              else
                let (acc1,rhs') = go mode acc rhs
                    (acc2,bod') = go mode acc1 bod
                in (acc2, LetE (v,locs,ty,rhs') bod')
            SearchAndStore s ->
              if s == rhs
              then (acc,ex)
              else
                let (acc1,bod') = go mode acc bod
                in ((v,locs,ty,rhs) : acc1, bod')
        IfE a b c  ->
          let (acc1,a') = go mode acc a
              (acc2,b') = go mode acc1 b
              (acc3,c') = go mode acc1 c
          in (acc2++acc3, IfE a' b' c')
        CaseE scrt brs ->
          let (acc0,brs') =
                foldr (\(a,b,c) (acc',es) ->
                         let (acc'',c') = go mode acc' c
                         in (acc'', (a,b,c'):es))
                      (acc,[])
                      brs
          in (acc0, CaseE scrt brs')
        WithArenaE ar bod ->
          let (acc',bod') = go mode acc bod
          in (acc', WithArenaE ar bod')
        TimeIt e ty b ->
          let (acc', e') = go mode acc e
          in (acc', TimeIt e' ty b)
        Ext (L3.LetAvail vs bod) ->
          let (acc',bod') = go mode acc bod
          in (acc', Ext (L3.LetAvail vs bod'))
        _ -> (acc,ex)


--------------------------------------------------------------------------------
