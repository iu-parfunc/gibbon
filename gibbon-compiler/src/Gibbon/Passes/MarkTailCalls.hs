module Gibbon.Passes.MarkTailCalls (markTailCalls) where 

import qualified Data.List as L
import Data.Maybe ( fromJust )
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Foldable ( foldrM )
import Prelude as P

import Gibbon.Common
import Gibbon.DynFlags
import Gibbon.NewL2.Syntax as NewL2
import Gibbon.L2.Syntax as Old

-- ^ A map that tracks location variables that need to be mutable. 
type TrackLocVariables = M.Map LocVar (S.Set LocVar, Bool)

markTailCalls :: NewL2.Prog2 -> PassM NewL2.Prog2
markTailCalls Prog{ddefs,fundefs,mainExp} = do
   fds' <- mapM (markTailCallsFn ddefs) $ M.elems fundefs
   let newFundefs = M.fromList $ map (\f -> (funName f,f)) fds'
   let newProg = Prog{ddefs=ddefs, fundefs=newFundefs, mainExp=mainExp}
   pure $ newProg {- dbgTraceIt (sdoc newProg) dbgTraceIt (sdoc $ M.elems fundefs')-}


markTailCallsFn :: NewL2.DDefs2 -> NewL2.FunDef2 -> PassM NewL2.FunDef2
markTailCallsFn ddefs f@FunDef{funName, funArgs, funTy, funMeta, funBody} = do 
  let (funBody', env) = markTailCallsFnBody funName M.empty funBody
      (ArrowTy2 locVars arrIns _arrEffs arrOut _locRets _isPar) = dbgTraceIt "Print env at the end."  dbgTraceIt (sdoc (env, M.elems env)) dbgTraceIt "End\n" funTy
      locVars' = P.map (\(LRM l r m) -> case (backTrackLocs env l False M.empty) of 
                                                 (False, _)-> LRM l r m  
                                                 (True, _) -> LRM l r OutputMutable
                       ) locVars
      funTy' = (ArrowTy2 locVars' arrIns _arrEffs arrOut _locRets _isPar)
    in return $ FunDef funName funArgs funTy' funBody' funMeta

  --  if tailCallTy == TMC
  --  then
  --     let (ArrowTy2 locVars arrIns _arrEffs arrOut _locRets _isPar) = funTy 
  --         locVars' = P.map (\(LRM l r m) -> if m == Output 
  --                                           then LRM l r OutputMutable
  --                                           else LRM l r m  
  --                          ) locVars
  --         funTy' = (ArrowTy2 locVars' arrIns _arrEffs arrOut _locRets _isPar)
  --       in return $ FunDef funName funArgs funTy' funBody' funMeta  {-dbgTraceIt (sdoc (tailCallTy, funName, funTy')) dbgTraceIt "a" dbgTraceIt (sdoc (tailCallTy, funName, funTy')) dbgTraceIt "a"  -}
  --  else if tailCallTy == TC
  --  then 
  --     let (ArrowTy2 locVars arrIns _arrEffs arrOut _locRets _isPar) = funTy
  --         funTy' = (ArrowTy2 locVars arrIns _arrEffs arrOut _locRets _isPar)
  --       in return $ FunDef funName funArgs funTy' funBody' funMeta {-dbgTraceIt (sdoc (tailCallTy, funName, funTy')) dbgTraceIt "b" dbgTraceIt (sdoc (tailCallTy, funName, funTy')) dbgTraceIt "b"  -} 
  --  else pure f {-dbgTraceIt (sdoc (tailCallTy, funName, funTy)) dbgTraceIt "c" dbgTraceIt (sdoc (tailCallTy, funName, funTy)) dbgTraceIt "c"-}
  --  --dbgTraceIt (sdoc tailCallTy) pure f
  -- pure f 

backTrackLocs :: TrackLocVariables -> LocVar -> Bool -> M.Map LocVar Bool -> (Bool, M.Map LocVar Bool) 
backTrackLocs env v accum visited = case M.lookup v env of 
                                      Nothing -> let visited' = M.insert v True visited 
                                                    in (accum, visited') 
                                      Just (s, mut) -> let  locsToLook = S.toList s
                                                            results = P.map (\l -> let (a', v') = backTrackLocs env l accum visited
                                                                                    in (a', v')
                                                                            ) locsToLook
                                                            accum' = P.foldr (\b a -> b || a) accum $ P.map fst results
                                                            visited' = M.unions $ P.map snd results
                                                            visited'' = M.insert v True visited
                                                            accum'' = accum' || mut
                                                          in (accum'', visited'') 

markTailCallsFnBody :: Var -> TrackLocVariables -> NewL2.Exp2 -> (NewL2.Exp2, TrackLocVariables)
markTailCallsFnBody funName env exp2  = case exp2 of 
                                VarE v -> (VarE v, env)
                                LitE l -> (LitE l, env)
                                CharE c -> (CharE c, env)
                                FloatE f -> (FloatE f, env)
                                LitSymE v -> (LitSymE v, env)
                                AppE (v, t) locs args -> let results = P.map (markTailCallsFnBody funName env) args
                                                             args' = P.map fst results
                                                             env' = M.unionsWith unionMapLambda $ P.map snd results
                                                           in (AppE (v, t) locs args', env')

                                PrimAppE p args -> let results = P.map (markTailCallsFnBody funName env) args
                                                       args' = P.map fst results
                                                       env' = M.unionsWith unionMapLambda $ P.map snd results
                                                     in (PrimAppE p args', env')

                                LetE (v, loc, ty,rhs) bod -> case rhs of 
                                                            AppE (v', _) locs' args' -> if v' == funName 
                                                                                   then let tailCallType = markTailCallsFnBodyHelper 0 bod {-dbgTraceIt ("Here markTailCallsFnBody then\n")-} 
                                                                                            (bod', env') = markTailCallsFnBody funName env bod
                                                                                            rhs' = AppE (v', tailCallType) locs' args' 
                                                                                            env'' = case tailCallType of 
                                                                                                           NoTail -> env' 
                                                                                                           TC -> env' 
                                                                                                           TMC -> P.foldr (\loc e -> case M.lookup (toLocVar loc) e of 
                                                                                                                                              Nothing -> M.insert (toLocVar loc) (S.empty, True) e
                                                                                                                                              Just (s, m) -> M.insert (toLocVar loc) (s, True) e
                                                                                                                          ) env' locs' 
                                                                                          in dbgTraceIt ("Print map: ") dbgTraceIt (sdoc (env'', M.elems env'')) dbgTraceIt ("End\n") (LetE (v, loc, ty, rhs') bod', env'') 
                                                                                   else let (bod', env') = markTailCallsFnBody funName env bod {-dbgTraceIt ("Here markTailCallsFnBody else\n")-}
                                                                                          in (LetE (v, loc, ty, rhs) bod', M.unionWith unionMapLambda env env')
                                                            _ -> let (bod', env') = markTailCallsFnBody funName env bod {-dbgTraceIt ("Here markTailCallsFnBody RST\n")-}
                                                                   in (LetE (v, loc, ty, rhs) bod', env')
                                IfE a b c -> let (a', e1) = markTailCallsFnBody funName env a
                                                 (b', e2) = markTailCallsFnBody funName e1 b
                                                 (c', e3) = markTailCallsFnBody funName e2 c
                                              in (IfE a' b' c', e3)

                                MkProdE ls -> let results = P.map (markTailCallsFnBody funName env) ls
                                                  ls' = P.map fst results
                                                  env' = M.unionsWith unionMapLambda $ P.map snd results
                                                 in (MkProdE ls', env')

                                ProjE i e -> let (e', env') = markTailCallsFnBody funName env e
                                               in (ProjE i e', env')

                                -- [(DataCon, [(Var,loc)], EXP)]
                                CaseE scrt brs -> let results = P.map (\(a, b, c) -> let (c', env') = markTailCallsFnBody funName env c
                                                                                       in ((a, b, c'), env')
                                                                      ) brs
                                                      brs' = P.map fst results 
                                                      env'' = M.unionsWith unionMapLambda $ P.map snd results  
                                                    in (CaseE scrt brs', env'') 
                                
                                -- TODO: Check map for any mutable output locations, if they are in the data con then mark them outputMutable
                                DataConE loc c args -> let locInDataCon = toLocVar loc
                                                         in case M.lookup locInDataCon env of 
                                                                          Nothing -> let results = P.map (markTailCallsFnBody funName env) args
                                                                                         args' = P.map fst results 
                                                                                         env' = M.unionsWith unionMapLambda $ P.map snd results
                                                                                       in (DataConE loc c args', env')
                                                                          Just (s, m) -> if m 
                                                                                         then let loc' = case loc of 
                                                                                                                NewL2.Loc lrem -> NewL2.Loc lrem{lremMode = OutputMutable}
                                                                                                                _ -> loc
                                                                                                  results = P.map (markTailCallsFnBody funName env) args
                                                                                                  args' = P.map fst results 
                                                                                                  env' = M.unionsWith unionMapLambda $ P.map snd results
                                                                                               in (DataConE loc' c args', env')
                                                                                          else let results = P.map (markTailCallsFnBody funName env) args
                                                                                                   args' = P.map fst results 
                                                                                                   env' = M.unionsWith unionMapLambda $ P.map snd results
                                                                                                in (DataConE loc c args', env')
                                TimeIt e d b -> let (e', env') = markTailCallsFnBody funName env e 
                                                  in (TimeIt e' d b, env')
                                MapE d e -> let (e', env') = markTailCallsFnBody funName env e
                                              in (MapE d e', env')
                                FoldE i it e -> let (e', env') = markTailCallsFnBody funName env e
                                                 in (FoldE i it e', env')

                                -- TODO: Check map for any mutable output locations, if they are in the data con then mark them outputMutable                 
                                SpawnE v locs exps -> let results = P.map (markTailCallsFnBody funName env) exps
                                                          exps' = P.map fst results 
                                                          env' = M.unionsWith unionMapLambda $ P.map snd results
                                                        in (SpawnE v locs exps', env')

                                SyncE -> (exp2, env)

                                WithArenaE _v e -> let (e', env') = markTailCallsFnBody funName env e
                                                     in (WithArenaE _v e', env')
                                Ext ext -> 
                                    case ext of 
                                       Old.LetRegionE r a b bod -> let (bod', env') = markTailCallsFnBody funName env bod
                                                                     in (Ext $ Old.LetRegionE r a b bod', env')
                                       Old.LetParRegionE r a b bod -> let (bod', env') = markTailCallsFnBody funName env bod
                                                                        in (Ext $ Old.LetParRegionE r a b bod', env')
                                       Old.LetLocE loc locexp bod -> let locInExp = freeLoc locexp 
                                                                         env' = case locInExp of 
                                                                                      Nothing -> env   
                                                                                      Just l -> M.insert l (S.singleton loc, False) env    
                                                                         (bod', env'') = markTailCallsFnBody funName env' bod
                                                                       in (Ext $ Old.LetLocE loc locexp bod', env'')
                                       _ -> (Ext ext, env)
                                       -- Old.StartOfPkdCursor v -> [NoTail]
                                       -- Old.TagCursor a b -> [NoTail]
                                       -- Old.RetE locs v -> [NoTail]
                                       -- Old.FromEndE loc -> [NoTail]
                                       -- Old.BoundsCheck _ reg cur -> [NoTail]
                                       -- Old.IndirectionE _ _ (a,b) (c,d) _ -> [NoTail]
                                       -- Old.AddFixed v _    -> [NoTail]
                                       -- Old.GetCilkWorkerNum -> [NoTail]
                                       -- Old.LetAvail vs bod -> [NoTail]
                                       -- Old.AllocateTagHere loc _ -> [NoTail]
                                       -- Old.AllocateScalarsHere loc -> [NoTail]
                                       -- Old.SSPush _ a b _ -> [NoTail]
                                       -- Old.SSPop _ a b -> [NoTail]
                                       -- Old.LetRegionE r _ _ bod -> S.delete (Old.regionToVar r) (allFreeVars bod)
                                       
    where
      unionMapLambda = (\(locSet, m) (locSet', m') -> (S.union locSet locSet', m))
      freeLoc :: PreLocExp LocArg -> Maybe LocVar
      freeLoc exp = case exp of 
                                AfterConstantLE c loc   -> Just (toLocVar loc)
                                AfterVariableLE v loc b -> Just (toLocVar loc)  
                                FromEndLE loc -> Just (toLocVar loc) 
                                _ -> Nothing

-- Old.LetRegionE r _ _ bod -> S.delete (Old.regionToVar r) (allFreeVars bod)
-- Old.LetParRegionE r _ _ bod -> S.delete (Old.regionToVar r) (allFreeVars bod)
-- Old.LetLocE loc locexp bod -> S.delete loc (allFreeVars bod `S.union` gFreeVars locexp)
-- Old.StartOfPkdCursor v -> S.singleton v
-- Old.TagCursor a b-> S.fromList [a,b]
-- Old.RetE locs v     -> S.insert v (S.fromList (map toLocVar locs))
-- Old.FromEndE loc    -> S.singleton (toLocVar loc)
-- Old.BoundsCheck _ reg cur -> S.fromList (map toLocVar [reg, cur])
-- Old.IndirectionE _ _ (a,b) (c,d) _ -> S.fromList $ [toLocVar a, toLocVar b, toLocVar c, toLocVar d]
-- Old.AddFixed v _    -> S.singleton v
-- Old.GetCilkWorkerNum-> S.empty
-- Old.LetAvail vs bod -> S.fromList vs `S.union` gFreeVars bod
-- Old.AllocateTagHere loc _ -> S.singleton loc
-- Old.AllocateScalarsHere loc -> S.singleton loc
-- Old.SSPush _ a b _ -> S.fromList [a,b]
-- Old.SSPop _ a b -> S.fromList [a,b]                                    
                                       
                                  
                                    
markTailCallsFnBodyHelper :: Int -> NewL2.Exp2 -> TailRecType 
markTailCallsFnBodyHelper depth exp2 = case exp2 of
                              --   VarE v -> False 
                              --   LitE _ -> False 
                              --   CharE{} -> False 
                              --   FloatE{} -> False 
                              --   LitSymE _ -> False 
                              --   AppE v locs args -> False
                              --   PrimAppE p args -> False 
                                LetE (v,_,_,rhs) bod -> if True --depth == 0
                                                        then 
                                                          case rhs of 
                                                               DataConE loc d args -> markTailCallsFnBodyHelper (depth+1) bod {-dbgTraceIt ("Here2!") dbgTraceIt (sdoc rhs)-}
                                                                                      -- TODO: figure out a way to get the return type of the function
                                                                                      --let tyConOfDataConE = getTyOfDataCon ddefs d 
                                                                                      --    returnTy = outTy ty2
                                                                                      -- in if tyConOfDataConE == returnTy
                                                                                      --    then markTailCallsFnBodyHelper ddefs bod ty2 (depth+1)
                                                                                      --    else NoTail    
                                                               _ -> NoTail {- dbgTraceIt (sdoc rhs) dbgTraceIt ("Here!")-}
                                                        else NoTail {-dbgTraceIt ("Here3!")-}                                                                                
                              --   IfE a b c -> 
                              --   MkProdE ls -> 
                              --   ProjE i e -> 
                              --   CaseE scrt brs -> 
                              --   DataConE loc c args -> 
                              --   TimeIt e _ _ -> 
                              --   MapE _ e -> 
                              --   FoldE _ _ e -> 
                              --   SpawnE v locs _ ->
                              --   SyncE -> 
                              --   WithArenaE _v e ->
                                Ext ext -> case ext of 
                                    -- Old.LetRegionE r _ _ bod -> 
                                    -- Old.LetParRegionE r _ _ bod -> 
                                    -- Old.LetLocE loc locexp bod -> 
                                    -- Old.StartOfPkdCursor v -> 
                                    -- Old.TagCursor a b -> 
                                    Old.RetE locs v -> if depth == 0 then TC 
                                                       else if depth == 1 then TMC 
                                                       else NoTail 
                                    -- Old.FromEndE loc    -> 
                                    -- Old.BoundsCheck _ reg cur -> 
                                    -- Old.IndirectionE _ _ (a,b) (c,d) _ -> 
                                    -- Old.AddFixed v _    -> 
                                    -- Old.GetCilkWorkerNum-> 
                                    -- Old.LetAvail vs bod -> 
                                    -- Old.AllocateTagHere loc _ -> 
                                    -- Old.AllocateScalarsHere loc -> 
                                    -- Old.SSPush _ a b _ -> 
                                    -- Old.SSPop _ a b -> 
                                _ -> NoTail