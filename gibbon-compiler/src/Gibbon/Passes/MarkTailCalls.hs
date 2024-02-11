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
   let fundefs' = M.fromList $ map (\f -> (funName f,f)) fds'
   dbgTraceIt (sdoc $ M.elems fundefs') return $ Prog ddefs fundefs' mainExp


markTailCallsFn :: NewL2.DDefs2 -> NewL2.FunDef2 -> PassM NewL2.FunDef2
markTailCallsFn ddefs f@FunDef{funName, funArgs, funTy, funMeta, funBody} = do 
   let tailCallTy = markTailCallsFnBody funName ddefs funTy funBody 
   if elem TC tailCallTy
   then
      let (ArrowTy2 locVars arrIns _arrEffs arrOut _locRets _isPar _) = funTy 
          funTy' = (ArrowTy2 locVars arrIns _arrEffs arrOut _locRets _isPar TC)
        in dbgTraceIt (sdoc (tailCallTy, funName)) dbgTraceIt "a" return $ FunDef funName funArgs funTy' funBody funMeta
   else if elem TMC tailCallTy
   then 
      let (ArrowTy2 locVars arrIns _arrEffs arrOut _locRets _isPar _) = funTy
          funTy' = (ArrowTy2 locVars arrIns _arrEffs arrOut _locRets _isPar TMC)
        in dbgTraceIt (sdoc (tailCallTy, funName)) dbgTraceIt "b" return $ FunDef funName funArgs funTy' funBody funMeta
   else dbgTraceIt (sdoc (tailCallTy, funName)) dbgTraceIt "c" pure f 
   --dbgTraceIt (sdoc tailCallTy) pure f


markTailCallsFnBody :: Var -> NewL2.DDefs2 -> ArrowTy2 NewL2.Ty2 -> NewL2.Exp2 -> [TailRecType] 
markTailCallsFnBody funName ddefs2 ty2 exp2  = case exp2 of 
                                VarE v -> [NoTail]
                                LitE _ -> [NoTail]
                                CharE{} -> [NoTail]
                                FloatE{} -> [NoTail]
                                LitSymE _ -> [NoTail]
                                AppE v locs args -> P.concatMap (markTailCallsFnBody funName ddefs2 ty2) args 
                                PrimAppE p args -> P.concatMap (markTailCallsFnBody funName ddefs2 ty2) args
                                LetE (v,_,_,rhs) bod -> case rhs of 
                                                            AppE v' locs' args' -> if v' == funName 
                                                                                   then [markTailCallsFnBodyHelper ddefs2 bod ty2 0]
                                                                                   else [NoTail]
                                                            _ -> [NoTail]
                                IfE a b c -> (markTailCallsFnBody funName ddefs2 ty2 a) ++ (markTailCallsFnBody funName ddefs2 ty2 b) ++ (markTailCallsFnBody funName ddefs2 ty2 c) 
                                MkProdE ls -> P.concatMap (markTailCallsFnBody funName ddefs2 ty2) ls
                                ProjE i e -> markTailCallsFnBody funName ddefs2 ty2 e
                                -- [(DataCon, [(Var,loc)], EXP)]
                                CaseE scrt brs -> P.concatMap (\(a, b, c) -> markTailCallsFnBody funName ddefs2 ty2 c) brs 
                                DataConE loc c args -> P.concatMap (markTailCallsFnBody funName ddefs2 ty2) args
                                TimeIt e _ _ -> markTailCallsFnBody funName ddefs2 ty2 e 
                                MapE _ e -> markTailCallsFnBody funName ddefs2 ty2 e
                                FoldE _ _ e -> markTailCallsFnBody funName ddefs2 ty2 e
                                SpawnE v locs _ -> [NoTail]
                                SyncE -> [NoTail] 
                                WithArenaE _v e -> markTailCallsFnBody funName ddefs2 ty2 e
                                Ext ext -> 
                                    case ext of 
                                       Old.LetRegionE r _ _ bod -> markTailCallsFnBody funName ddefs2 ty2 bod
                                       Old.LetParRegionE r _ _ bod -> markTailCallsFnBody funName ddefs2 ty2 bod
                                       Old.LetLocE loc locexp bod -> markTailCallsFnBody funName ddefs2 ty2 bod
                                       Old.StartOfPkdCursor v -> [NoTail]
                                       Old.TagCursor a b -> [NoTail]
                                       Old.RetE locs v -> [NoTail]
                                       Old.FromEndE loc -> [NoTail]
                                       Old.BoundsCheck _ reg cur -> [NoTail]
                                       Old.IndirectionE _ _ (a,b) (c,d) _ -> [NoTail]
                                       Old.AddFixed v _    -> [NoTail]
                                       Old.GetCilkWorkerNum -> [NoTail]
                                       Old.LetAvail vs bod -> [NoTail]
                                       Old.AllocateTagHere loc _ -> [NoTail]
                                       Old.AllocateScalarsHere loc -> [NoTail]
                                       Old.SSPush _ a b _ -> [NoTail]
                                       Old.SSPop _ a b -> [NoTail]
                                    
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
                                LetE (v,_,_,rhs) bod -> if depth == 0
                                                        then 
                                                          case rhs of 
                                                               DataConE loc d args -> markTailCallsFnBodyHelper ddefs bod ty2 (depth+1)
                                                                                      -- TODO: figure out a way to get the return type of the function
                                                                                      --let tyConOfDataConE = getTyOfDataCon ddefs d 
                                                                                      --    returnTy = outTy ty2
                                                                                      -- in if tyConOfDataConE == returnTy
                                                                                      --    then markTailCallsFnBodyHelper ddefs bod ty2 (depth+1)
                                                                                      --    else NoTail    
                                                               _ -> NoTail
                                                        else NoTail                                                                                
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