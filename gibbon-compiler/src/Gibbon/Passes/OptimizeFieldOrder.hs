module Gibbon.Passes.OptimizeFieldOrder
    (shuffleDataCon) where  

import Data.Map as M
import Prelude as P
import Data.List as L

import Gibbon.Common
import Gibbon.L1.Syntax as L1

import Gibbon.Passes.AccessPatternsAnalysis (generateCfgFunctions)
import Gibbon.Passes.SolveLayoutConstrs (solveConstrs)

import System.IO.Unsafe as U


--Data structure to store output from ILP solver.
--Maps DataCon to bijection of new indices -> fields of datacon 
type FieldOrder = M.Map DataCon [Integer]


-- TODO: Make FieldOrder an argument passed to shuffleDataCon function.
shuffleDataCon :: Prog1 -> PassM Prog1
shuffleDataCon prg@Prog{ddefs,fundefs,mainExp} = do
    let (cfgs, fieldMap) = generateCfgFunctions (M.empty) (M.empty) (M.elems fundefs) "Layout1"
    -- TODO: probably better to make this a map from dcon to its num fields. 
    let field_len = P.length $ snd . snd $ lkp ddefs "Layout1"
    -- Instead of explicitly passing the function name, this should come from a annotation at the front end or something like that. 
    let fieldorder = locallyOptimizeFieldOrdering fieldMap ["Layout1"] (M.elems fundefs) "emphKeywordInContent" field_len (M.empty) 
    let functions  = M.elems fundefs
    -- NOTE : shuffling ddefs makes a lot of assumptions right now. 
    -- Mainly that we are just doing it for one function
    -- So we do not care out the globally optimal layout of the data constructor
    let shuffled_ddefs = findDataCon fieldorder ddefs 
    fds' <- mapM (shuffleDataConFunBody fieldorder) (M.elems fundefs)
    let fundefs' = M.fromList $ P.map (\f -> (funName f,f)) fds'
    mainExp' <- case mainExp of
        Nothing -> return Nothing
        Just (mn, ty)-> Just . (,ty) <$> shuffleDataConExp fieldorder mn 
    let l1 = prg { ddefs = shuffled_ddefs
               , fundefs = fundefs' 
               , mainExp = mainExp'
               }
    dbgTraceIt (sdoc fieldorder) dbgTraceIt ("\n") pure l1 --dbgTraceIt (sdoc fieldorder) dbgTraceIt ("\n")

-- This is pointless and just goes through the function we are locally optimizing for maybe a cleverer way to do in haskell
-- Since this problem is to locally optimize for a particular function right now we are not concerned with finding the best 
-- optimal layout for the complete program. 
locallyOptimizeFieldOrdering :: FieldMap -> [DataCon] -> [FunDef1] -> String -> Int -> FieldOrder -> FieldOrder 
locallyOptimizeFieldOrdering fieldMap dcons fundefs funcName field_len orderIn = case fundefs of
    [] -> orderIn
    x:xs -> let map' = generateLocallyOptimalOrderings fieldMap dcons x funcName field_len orderIn
                map'' = locallyOptimizeFieldOrdering fieldMap dcons xs funcName field_len map' 
              in map'

-- for the function for which we are locally optimizing for, find the optimal layout of the data constructors that we care about. 
-- "Locally optimizing for the function"
generateLocallyOptimalOrderings :: FieldMap -> [DataCon] -> FunDef1 -> String -> Int -> FieldOrder -> FieldOrder
generateLocallyOptimalOrderings fieldMap datacons fundef@FunDef{funName,funBody,funTy,funArgs} funcName field_len orderIn =
    if (fromVar funName) == funcName then
       let lstDconEdges = M.findWithDefault M.empty fundef fieldMap
        in case datacons of 
                []   -> orderIn
                x:xs -> let dconEdges = M.findWithDefault [] x lstDconEdges
                          in case dconEdges of 
                                  [] -> orderIn
                                  _  ->  let layout      =  U.unsafePerformIO $ (solveConstrs dconEdges)
                                             -- In case we don't get orderings for some of the fields in the data con 
                                             -- to be safe we should complete the layout orderings of the missing fields. 
                                             fix_missing = if (P.length layout) < field_len then 
                                                              let indices  = [0 .. (field_len - 1)]
                                                                  minuslist  = makeneg field_len 
                                                                  partial    = fillList minuslist layout
                                                                  avail      = P.map (\(a, b) -> a) layout
                                                                  navail     = deleteMany avail indices
                                                                  new        = fillminus1 partial navail
                                                               in new
                                                           else
                                                            P.map (\(a, b) ->  b) layout
                                             fieldorder = M.insert x (integerList fix_missing) orderIn
                                             fieldorder' = generateLocallyOptimalOrderings fieldMap xs fundef funcName field_len fieldorder
                                          in fieldorder' -- dbgTraceIt (sdoc dconEdges) dbgTraceIt ("\n") dbgTraceIt (sdoc fieldorder') dbgTraceIt ("\n")
    else 
      orderIn
                        
makeneg :: Int -> [Int]
makeneg len = if len <=0 then [] 
              else (makeneg (len -1)) ++ [-1]    
              
integerList :: [Int] -> [Integer]
integerList lst = case lst of 
                       [] -> [] 
                       x:xs -> [P.toInteger x] ++ (integerList xs)

fillList :: [Int] -> [(Int, Int)] -> [Int]
fillList old vals = case vals of 
                          [] -> old 
                          x:xs -> let (a, b)   = x
                                      edited   = (L.take b old) ++ [a] ++ (L.drop (b + 1) old)
                                    in fillList edited xs

-- https://www.reddit.com/r/haskell/comments/u841av/trying_to_remove_all_the_elements_that_occur_in/                                
deleteOne :: Eq a => a -> [a] -> [a]
deleteOne _ [] = [] -- Nothing to delete
deleteOne x (y:ys) | x == y = ys -- Drop exactly one matching item
deleteOne x (y:ys) = y : deleteOne x ys -- Drop one, but not this one (doesn't match).
                                
deleteMany :: Eq a => [a] -> [a] -> [a]
deleteMany [] = id -- Nothing to delete
deleteMany (x:xs) = deleteMany xs . deleteOne x -- Delete one, then the rest.


fillminus1 :: [Int] -> [Int] -> [Int]
fillminus1 lst indices = case lst of 
                               [] -> [] 
                               x:xs -> case indices of 
                                              [] -> lst                                 
                                              y:ys -> if x == -1 then [y] ++ fillminus1 xs ys
                                                      else [x] ++ fillminus1 xs indices       

shuffleDataConFunBody :: FieldOrder -> FunDef1  -> PassM FunDef1
shuffleDataConFunBody fieldorder f@FunDef{funBody}  = do                                   
  funBody' <- shuffleDataConExp fieldorder funBody                        
  return $ f {funBody = funBody'}

shuffleDataConExp :: FieldOrder -> Exp1 -> PassM Exp1
shuffleDataConExp fieldorder ex = case ex of 
    DataConE loc dcon args -> do
            args' <- shuffleDataConArgs fieldorder dcon args 
            return $ DataConE loc dcon args'
    VarE{}    -> return ex
    LitE{}    -> return ex
    CharE{}   -> return ex
    FloatE{}  -> return ex
    LitSymE{} -> return ex
    AppE f locs args -> AppE f locs <$> mapM (shuffleDataConExp fieldorder) args
    PrimAppE f args  -> PrimAppE f <$> mapM (shuffleDataConExp fieldorder) args
    LetE (v,loc,ty,rhs) bod -> do 
         LetE <$> (v,loc,ty,) <$> (shuffleDataConExp fieldorder) rhs <*> (shuffleDataConExp fieldorder) bod
    IfE a b c  -> IfE <$> (shuffleDataConExp fieldorder) a <*> (shuffleDataConExp fieldorder) b <*> (shuffleDataConExp fieldorder) c
    MkProdE xs -> MkProdE <$> mapM (shuffleDataConExp fieldorder) xs
    ProjE i e  -> ProjE i <$> (shuffleDataConExp fieldorder) e
    CaseE scrt mp -> do 
                    mp' <- mapM (\(a,b,c) -> do
                                              b' <- shuffleDataConCase fieldorder a b
                                              c' <- (shuffleDataConExp fieldorder) c
                                              return $ (a,b',c')) mp 
                    return $ CaseE scrt mp'
    TimeIt e ty b -> do
      e' <- (shuffleDataConExp fieldorder) e
      return $ TimeIt e' ty b
    WithArenaE v e -> do
      e' <- (shuffleDataConExp fieldorder) e
      return $ WithArenaE v e'
    SpawnE f locs args -> SpawnE f locs <$> mapM (shuffleDataConExp fieldorder) args
    SyncE   -> pure SyncE
    Ext _   -> return ex
    MapE{}  -> error "shuffleFieldOrdering: TODO MapE"
    FoldE{} -> error "shuffleFieldOrdering: TODO FoldE"

shuffleDataConArgs :: FieldOrder -> DataCon -> [Exp1] -> PassM [Exp1]
shuffleDataConArgs fieldorder dcon exps = if (M.member dcon fieldorder) then 
                                            pure $ permute (findWithDefault [] dcon fieldorder) exps
                                          else 
                                            pure exps


shuffleDataConCase :: FieldOrder -> DataCon -> [(Var, ())] -> PassM [(Var, ())]
shuffleDataConCase fieldorder dcon vs = if (M.member dcon fieldorder) then
                                          pure $ permute (findWithDefault [] dcon fieldorder) vs 
                                        else 
                                          pure vs


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

reverse_dataCons :: FieldOrder -> [(DataCon, [(IsBoxed, UrTy a)])] -> [(DataCon, [(IsBoxed, UrTy a)])]
reverse_dataCons fieldorder list = case list of 
    [] -> []
    (layout_name, fields):xs -> if (M.member layout_name fieldorder)
                                    then let rev_fields = permute (findWithDefault [] layout_name fieldorder)  fields
                                           in [(layout_name, rev_fields)] ++ (reverse_dataCons fieldorder xs)
                                else 
                                    [(layout_name, fields)] ++ (reverse_dataCons fieldorder xs)


permute :: [Integer] -> [a] -> [a]
permute indices list = case indices of 
    [] -> []
    x:xs -> [list !! (P.fromInteger x)] ++ permute xs list
