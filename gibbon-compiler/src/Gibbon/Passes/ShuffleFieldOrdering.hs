{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Gibbon.Passes.ShuffleFieldOrdering
    (shuffleDataCon) where  

import Data.Map as M

import Gibbon.Common
import Gibbon.L1.Syntax as L1 
import Prelude as P


--Data structure to store output from ILP solver.
--Maps DataCon to new indices of fields 
type FieldOrder = M.Map DataCon [Int]


shuffleDataCon :: Prog1 -> PassM Prog1
shuffleDataCon prg@Prog{ddefs,fundefs,mainExp} = do
    let fieldmap = M.fromList [("Layout1", [0, 2, 1])]  
    let shuffled_ddefs = findDataCon fieldmap ddefs
    fds' <- mapM (shuffleDataConFunBody) (M.elems fundefs)
    let fundefs' = M.fromList $ P.map (\f -> (funName f,f)) fds'
    mainExp' <- case mainExp of
        Nothing -> return Nothing
        Just (mn, ty)-> Just . (,ty) <$> shuffleDataConExp mn 
    let l1 = prg { ddefs = shuffled_ddefs
               , fundefs = fundefs' 
               , mainExp = mainExp'
               }
    pure l1

shuffleDataConFunBody :: FunDef1  -> PassM FunDef1
shuffleDataConFunBody f@FunDef{funBody}  = do                                   
  funBody' <- shuffleDataConExp funBody                        
  return $ f {funBody = funBody'}

shuffleDataConExp ::  Exp1 -> PassM Exp1
shuffleDataConExp ex = case ex of 
    DataConE loc dcon args -> do
            args' <- shuffleDataConArgs dcon args 
            return $ DataConE loc dcon args'
    VarE{}    -> return ex
    LitE{}    -> return ex
    CharE{}   -> return ex
    FloatE{}  -> return ex
    LitSymE{} -> return ex
    AppE f locs args -> AppE f locs <$> mapM shuffleDataConExp args
    PrimAppE f args  -> PrimAppE f <$> mapM shuffleDataConExp args
    LetE (v,loc,ty,rhs) bod -> do 
         LetE <$> (v,loc,ty,) <$> shuffleDataConExp rhs <*> shuffleDataConExp bod
    IfE a b c  -> IfE <$> shuffleDataConExp a <*> shuffleDataConExp b <*> shuffleDataConExp c
    MkProdE xs -> MkProdE <$> mapM shuffleDataConExp xs
    ProjE i e  -> ProjE i <$> shuffleDataConExp e
    CaseE scrt mp -> do 
                    mp' <- mapM (\(a,b,c) -> do
                                              b' <- shuffleDataConCase a b
                                              c' <- shuffleDataConExp c
                                              return $ (a,b',c')) mp 
                    return $ CaseE scrt mp'
    TimeIt e ty b -> do
      e' <- shuffleDataConExp e
      return $ TimeIt e' ty b
    WithArenaE v e -> do
      e' <- shuffleDataConExp e
      return $ WithArenaE v e'
    SpawnE f locs args -> SpawnE f locs <$> mapM shuffleDataConExp args
    SyncE   -> pure SyncE
    Ext _   -> return ex
    MapE{}  -> error "shuffleFieldOrdering: TODO MapE"
    FoldE{} -> error "shuffleFieldOrdering: TODO FoldE"

shuffleDataConArgs :: DataCon -> [Exp1] -> PassM [Exp1]
shuffleDataConArgs dcon exps = case dcon of 
                                    "Layout1" -> pure $ P.reverse exps
                                    _         -> pure exps  

shuffleDataConCase :: DataCon -> [(Var, ())] -> PassM [(Var, ())]
shuffleDataConCase dcon vs = case dcon of 
                                  "Layout1" -> pure $ P.reverse vs 
                                  _         -> pure vs


findDataCon :: FieldOrder -> DDefs (UrTy a) -> DDefs (UrTy a)
findDataCon fieldorder ddefs = M.fromList (go (M.toList ddefs))
    where
        go list = case list of 
                    [] -> [] 
                    x:xs -> case x of 
                                (var, ddef) -> let new_ddef = reverse_ddef fieldorder ddef
                                                   in [(var, new_ddef)] ++ (go xs)



reverse_ddef :: FieldOrder -> DDef (UrTy a) -> DDef (UrTy a)
reverse_ddef fieldorder DDef{tyName, tyArgs, dataCons} =  case tyName of 
    _ -> let newDataCons = reverse_dataCons fieldorder dataCons 
           in DDef{tyName, tyArgs, dataCons=newDataCons}
    -- _      -> DDef{tyName, tyArgs, dataCons}

reverse_dataCons :: FieldOrder -> [(DataCon, [(IsBoxed, UrTy a)])] -> [(DataCon, [(IsBoxed, UrTy a)])]
reverse_dataCons fieldorder list = case list of 
    [] -> []
    (layout_name, fields):xs -> if (M.member layout_name fieldorder)
                                    then let rev_fields = P.reverse fields
                                           in [(layout_name, rev_fields)] ++ (reverse_dataCons fieldorder xs)
                                else 
                                    [(layout_name, fields)] ++ (reverse_dataCons fieldorder xs)