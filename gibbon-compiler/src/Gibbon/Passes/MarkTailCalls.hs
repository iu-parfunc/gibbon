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

markTailCalls :: NewL2.Prog2 -> PassM NewL2.Prog2
markTailCalls Prog{ddefs,fundefs,mainExp} = do
   fds' <- mapM (markTailCallsFn ddefs) $ M.elems fundefs
   let newFundefs = M.fromList $ map (\f -> (funName f,f)) fds'
   let newProg = Prog{ddefs=ddefs, fundefs=newFundefs, mainExp=mainExp}
   pure $ newProg {- dbgTraceIt (sdoc newProg) dbgTraceIt (sdoc $ M.elems fundefs')-}


markTailCallsFn :: NewL2.DDefs2 -> NewL2.FunDef2 -> PassM NewL2.FunDef2
markTailCallsFn ddefs f@FunDef{funName, funArgs, funTy, funMeta, funBody} = do 
   let (funBody', tailCallTy) = markTailCallsFnBody funName ddefs funTy funBody 
   if tailCallTy == TMC
   then
      let (ArrowTy2 locVars arrIns _arrEffs arrOut _locRets _isPar) = funTy 
          locVars' = P.map (\(LRM l r m mu) -> if m == Output 
                                                then LRM l r m True
                                                else LRM l r m mu  
                           ) locVars
          funTy' = (ArrowTy2 locVars' arrIns _arrEffs arrOut _locRets _isPar)
        in return $ FunDef funName funArgs funTy' funBody' funMeta  {-dbgTraceIt (sdoc (tailCallTy, funName, funTy')) dbgTraceIt "a" dbgTraceIt (sdoc (tailCallTy, funName, funTy')) dbgTraceIt "a"  -}
   else if tailCallTy == TC
   then 
      let (ArrowTy2 locVars arrIns _arrEffs arrOut _locRets _isPar) = funTy
          funTy' = (ArrowTy2 locVars arrIns _arrEffs arrOut _locRets _isPar)
        in return $ FunDef funName funArgs funTy' funBody' funMeta {-dbgTraceIt (sdoc (tailCallTy, funName, funTy')) dbgTraceIt "b" dbgTraceIt (sdoc (tailCallTy, funName, funTy')) dbgTraceIt "b"  -} 
   else pure f {-dbgTraceIt (sdoc (tailCallTy, funName, funTy)) dbgTraceIt "c" dbgTraceIt (sdoc (tailCallTy, funName, funTy)) dbgTraceIt "c"-}
   --dbgTraceIt (sdoc tailCallTy) pure f


markTailCallsFnBody :: Var -> NewL2.DDefs2 -> ArrowTy2 NewL2.Ty2 -> NewL2.Exp2 -> (NewL2.Exp2, TailRecType)
markTailCallsFnBody funName ddefs2 ty2 exp2  = case exp2 of 
                                VarE v -> (VarE v, NoTail)
                                LitE l -> (LitE l, NoTail)
                                CharE c -> (CharE c, NoTail)
                                FloatE f -> (FloatE f, NoTail)
                                LitSymE v -> (LitSymE v, NoTail)
                                AppE (v, t) locs args -> let results = P.map (markTailCallsFnBody funName ddefs2 ty2) args
                                                             args' = P.map (\(exp, tailty) -> exp) results
                                                             tailtys = P.map (\(exp, tailty) -> tailty) results
                                                           in (AppE (v, t) locs args', P.maximum tailtys)

                                PrimAppE p args -> let results = P.map (markTailCallsFnBody funName ddefs2 ty2) args
                                                       args' = P.map (\(exp, tailty) -> exp) results
                                                       tailtys = P.map (\(exp, tailty) -> tailty) results
                                                     in (PrimAppE p args', P.maximum tailtys)


                                LetE (v, loc, ty,rhs) bod -> case rhs of 
                                                            AppE (v', _) locs' args' -> if v' == funName 
                                                                                   then let tailCallType = markTailCallsFnBodyHelper ddefs2 bod ty2 0 {-dbgTraceIt ("Here markTailCallsFnBody then\n")-} 
                                                                                            (bod', tailCallType') = markTailCallsFnBody funName ddefs2 ty2 bod
                                                                                            rhs' = AppE (v', tailCallType) locs' args' 
                                                                                          in (LetE (v, loc, ty, rhs') bod', P.maximum [tailCallType, tailCallType'])  
                                                                                            
                                                                                   else let (bod', tailCallType) = markTailCallsFnBody funName ddefs2 ty2 bod {-dbgTraceIt ("Here markTailCallsFnBody else\n")-}
                                                                                          in (LetE (v, loc, ty, rhs) bod', tailCallType)
                                                            _ -> let (bod', tailCallTy) = markTailCallsFnBody funName ddefs2 ty2 bod {-dbgTraceIt ("Here markTailCallsFnBody RST\n")-}
                                                                   in (LetE (v, loc, ty, rhs) bod', tailCallTy)
                                IfE a b c -> let (a', t1) = markTailCallsFnBody funName ddefs2 ty2 a
                                                 (b', t2) = markTailCallsFnBody funName ddefs2 ty2 b
                                                 (c', t3) = markTailCallsFnBody funName ddefs2 ty2 c
                                              in (IfE a' b' c', P.maximum [t1, t2, t3])
                                MkProdE ls -> let results = P.map (markTailCallsFnBody funName ddefs2 ty2) ls
                                                  ls' = P.map (\(exp, tailty) -> exp) results
                                                  tailtys = P.map (\(exp, tailty) -> tailty) results
                                                 in (MkProdE ls', P.maximum tailtys)

                                ProjE i e -> let (e', t) = markTailCallsFnBody funName ddefs2 ty2 e
                                               in (ProjE i e', t)
                                -- [(DataCon, [(Var,loc)], EXP)]
                                CaseE scrt brs -> let results = P.map (\(a, b, c) -> let (c', t) = markTailCallsFnBody funName ddefs2 ty2 c
                                                                                       in ((a, b, c') , t)
                                                                      ) brs
                                                      brs' = P.map (\(t', _) -> t') results
                                                      tailtys = P.map (\(_, tailty) -> tailty) results 
                                                    in (CaseE scrt brs', P.maximum tailtys) 

                                DataConE loc c args -> let results = P.map (markTailCallsFnBody funName ddefs2 ty2) args
                                                           args' = P.map (\(exp, tailty) -> exp) results
                                                           tailtys = P.map (\(exp, tailty) -> tailty) results
                                                         in (DataConE loc c args', P.maximum tailtys)

                                TimeIt e d b -> let (e', t) = markTailCallsFnBody funName ddefs2 ty2 e 
                                                  in (TimeIt e' d b, t)
                                MapE d e -> let (e', t) = markTailCallsFnBody funName ddefs2 ty2 e
                                              in (MapE d e', t)
                                FoldE i it e -> let (e', t) = markTailCallsFnBody funName ddefs2 ty2 e
                                                 in (FoldE i it e', t)
                                SpawnE v locs exps -> let results = P.map (markTailCallsFnBody funName ddefs2 ty2) exps
                                                          exps' = P.map (\(exp, tailty) -> exp) results
                                                          tailtys = P.map (\(exp, tailty) -> tailty) results
                                                        in (SpawnE v locs exps', P.maximum tailtys)
                                SyncE -> (exp2, NoTail)
                                WithArenaE _v e -> let (e', t) = markTailCallsFnBody funName ddefs2 ty2 e
                                                     in (WithArenaE _v e', t)
                                Ext ext -> 
                                    case ext of 
                                       Old.LetRegionE r a b bod -> let (bod', t) = markTailCallsFnBody funName ddefs2 ty2 bod
                                                                     in (Ext $ Old.LetRegionE r a b bod', t)
                                       Old.LetParRegionE r a b bod -> let (bod', t) = markTailCallsFnBody funName ddefs2 ty2 bod
                                                                        in (Ext $ Old.LetParRegionE r a b bod', t)
                                       Old.LetLocE loc locexp bod -> let (bod', t) = markTailCallsFnBody funName ddefs2 ty2 bod
                                                                        in (Ext $ Old.LetLocE loc locexp bod', t)
                                       _ -> (Ext ext, NoTail)
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
                                    
markTailCallsFnBodyHelper :: NewL2.DDefs2 -> NewL2.Exp2 -> ArrowTy2 NewL2.Ty2 -> Int -> TailRecType 
markTailCallsFnBodyHelper ddefs exp2 ty2 depth = case exp2 of
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
                                                               DataConE loc d args -> markTailCallsFnBodyHelper ddefs bod ty2 (depth+1) {-dbgTraceIt ("Here2!") dbgTraceIt (sdoc rhs)-}
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