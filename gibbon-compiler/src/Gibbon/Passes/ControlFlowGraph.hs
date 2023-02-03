module Gibbon.Passes.ControlFlowGraph (generateCfg) where

import Data.Graph as G
import Data.Map as M
import Data.Set as S
import Data.List as L

import Gibbon.Common
import Gibbon.L1.Syntax as L1 
import Prelude as P
import Control.Monad as Mo


-- Type to store the CFG's for each function that apprears in code.
-- We should use annotations form the front end to onlt contruct CFG's 
-- for functions that are annotated. 
type CFGfunctionMap = M.Map FunDef1 (G.Graph, G.Vertex -> (Exp1, Int, [Int]), Int -> Maybe G.Vertex)

type VariableMap = M.Map Var Maybe (DataCon, Int)

-- For now make this return, the CFGFunctionMap 
-- But this should ideally return the Constraints 
-- which should then be passed to the constraint solver. 
generateCfg :: Prog1 -> PassM Prog1
generateCfg prg@Prog{ddefs, fundefs, mainExp} = do 
    let cfgFunctionMap = M.empty 
    newCfgFunctionMap <- generateCfgFunctions cfgFunctionMap (M.elems fundefs)
    let l1 = prg { ddefs = ddefs
               , fundefs = fundefs 
               , mainExp = mainExp
               }
    pure l1


generateCfgFunctions :: CFGfunctionMap -> [FunDef1] -> PassM CFGfunctionMap
generateCfgFunctions cfgMap defs = 
    case defs of 
        [] -> pure cfgMap 
        x:xs -> do 
            (cfgMapNew, edgeList) <- generateCfgFunction cfgMap x
            newMap <- generateCfgFunctions cfgMap xs 
            {-dbgTraceIt (sdoc edgeList) dbgTraceIt ("\n") dbgTraceIt ("\n") dbgTraceIt ("\n")-}
            pure newMap

        
        
generateCfgFunction :: CFGfunctionMap -> FunDef1 -> PassM (CFGfunctionMap, [(Exp1, Int, [Int])])
generateCfgFunction cfgMap f@FunDef { funName, funBody, funTy, funArgs } = do  
    (edgeList, succ, maxDepth) <- generateCFGExp 0 funBody
    let (graph, nodeFromVertex, vertexFromKey) = G.graphFromEdges edgeList
    -- let x  = topSort graph
    -- let x' = P.map nodeFromVertex x 

    dbgTraceIt (sdoc x') pure (cfgMap, edgeList)


generateCFGExp :: Int -> Exp1 -> PassM ( [(Exp1, Int, [Int])] , Int, Int)
generateCFGExp vertexCounter exp1 = case exp1 of 
    --Recursively do for args, for now assuming this is a leaf node (base case)
    DataConE loc dcon args -> do 
        let edge = (exp1, vertexCounter, [])
        pure ([edge], vertexCounter, vertexCounter)
    VarE{} -> do
        let edge = (exp1, vertexCounter, []) 
        pure ([edge], vertexCounter, vertexCounter)
    LitE{} -> do 
        let edge = (exp1, vertexCounter, [])
        pure ([edge], vertexCounter, vertexCounter)
    CharE{} -> do 
        let edge = (exp1, vertexCounter, [])
        pure ([edge], vertexCounter, vertexCounter)
    FloatE{} -> do 
        let edge = (exp1, vertexCounter, [])
        pure ([edge], vertexCounter, vertexCounter)
    LitSymE{} -> do 
        let edge = (exp1, vertexCounter, [])
        pure ([edge], vertexCounter, vertexCounter)
    AppE f locs args -> do
        (edgeList, succList, maxDepth) <- processExpSeqAppE (vertexCounter+1) args
        let edge     = ( (VarE f), vertexCounter, succList)
        let newEdges = edgeList ++ [edge]
        pure (newEdges, vertexCounter, maxDepth)
    PrimAppE f args -> do 
        (edgeList, succList, maxDepth) <- processExpSeqAppE (vertexCounter+1) args
        let edge     = (exp1, vertexCounter, succList)
        let newEdges = edgeList ++ [edge]
        pure (newEdges, vertexCounter, maxDepth)
    LetE (v,loc,ty,rhs) bod -> do 
        (edgeList, succ, maxDepth) <- generateCFGExp (vertexCounter+1) bod
        let exp'  = LetE (v, loc, ty, rhs) $ VarE v
        let edge = (exp', vertexCounter, [succ])
        let edgeList' = edgeList ++ [edge]
        pure (edgeList', vertexCounter, maxDepth)
    CaseE scrt mp -> do 
        (edgeList, succList, maxDepth) <- processExpSeqCase (vertexCounter+1) mp
        let edge     = (scrt, vertexCounter, succList)
        let newEdges  = edgeList ++ [edge]
        pure (newEdges, vertexCounter, maxDepth)
    IfE a b c -> do 
        (edgeListB, succB, d1) <- generateCFGExp (vertexCounter+1) b 
        (edgeListC, succC, d2) <- generateCFGExp (d1+1) c 
        let succList = [succB, succC]
        let edge     = (a, vertexCounter, succList)
        let newEdges = edgeListB ++ edgeListC ++ [edge]
        pure (newEdges, vertexCounter, P.maximum [d1, d2])
    MkProdE xs -> do 
        (edgeList, succList, maxDepth) <- processExpSeqAppE (vertexCounter+1) xs
        let edge      = (exp1, vertexCounter, succList)
        let newEdges  = edgeList ++ [edge]
        pure (newEdges, vertexCounter, maxDepth)
    ProjE i e -> error "ControlFlowGraph: TODO ProjE"
    TimeIt e ty b -> error "ControlFlowGraph: TODO TimeIt"
    WithArenaE v e -> error "ControlFlowGraph: TODO WithArenaE"
    SpawnE f locs args -> error "ControlFlowGraph: TODO SpawnE"
    SyncE   -> error "ControlFlowGraph: TODO SyncE"
    Ext _   -> error "ControlFlowGraph: TODO Ext"
    MapE{}  -> error "ControlFlowGraph: TODO MapE"
    FoldE{} -> error "ControlFlowGraph: TODO FoldE"


processExpSeqAppE :: Int -> [Exp1] -> PassM ([(Exp1, Int, [Int])] , [Int], Int)
processExpSeqAppE currVertex exp = case exp of 
    []   -> pure ([], [], currVertex)
    x:xs -> do 
        (edgeList, succ, maxDepth) <- generateCFGExp currVertex x
        (edgeList', succ', maxDepth') <- processExpSeqAppE (maxDepth+1) xs
        let newEdgeList = edgeList ++ edgeList'
        let succList    = [succ] ++ succ'
        pure (newEdgeList, succList, maxDepth') 

processExpSeqCase :: Int -> [(DataCon, [(Var, loc)] , Exp1)] -> PassM ( [(Exp1, Int, [Int])] , [Int], Int )
processExpSeqCase currVertex lst = case lst of 
    [] -> pure ([], [], currVertex)
    x:xs -> do 
        (edgeList, succ, maxDepth) <- generateVerticesCase currVertex x 
        (edgeList', succList, maxDepth') <- processExpSeqCase (maxDepth+1) xs
        let newEdgeList = edgeList ++ edgeList' 
        let succList'    = [succ] ++ succList
        pure (newEdgeList, succList', maxDepth')


generateVerticesCase :: Int -> (DataCon, [(Var, loc)] , Exp1) -> PassM ( [(Exp1, Int, [Int])] , Int, Int )
generateVerticesCase currVertex branch = do 
    let datacon      = fst3 branch 
    let fields_locs  = snd3 branch
    let fields       = P.map (\x -> ( VarE (fst x) )) fields_locs
    let dataconExp   = DataConE () datacon fields
    (edgeList, succ, maxDepth) <- generateCFGExp (currVertex+1) (thd3 branch) 
    let edge = (dataconExp, currVertex, [succ])
    let newEdges = edgeList ++ [edge]
    pure (newEdges, currVertex, maxDepth) 
    

generateVariableToDataCon :: [(Exp1, Int, [Int])] -> VariableMap
generateVariableToDataCon vertices = 


processVertex :: [(Exp1, Int, [Int])] -> (Exp1, Int, [Int]) -> VariableMap -> VariableMap
processVertex graph node map = case node of 
    (expression, id, succ) -> case expression of
        DataConE loc dcon args ->                    -- do you really need to do it for this expression the way we've designed our passes ??
        VarE var -> let status = getIndexFromVariableDataCon graph var
                      in case status of 
                          Nothing -> map
                          Maybe position -> --update the map 
        LitE val -> map
        CharE ch -> map
        FloatE val -> map
        LitSymE var -> let status = getIndexFromVariableDataCon graph var
                         in case status of 
                             Nothing -> map 
                             Maybe position -> --update the map
        AppE f locs args -> let freeVariables =  S.toList (gFreeVars args)
                                indexes       = P.map (getIndexFromVariableDataCon graph) freeVariables
                              in -- update the map with the mapping of the new variables. 
        PrimAppE f args -> let freeVariables = S.toList (gFreeVars args)
                               indexes       = P.map 
        LetE (v,loc,ty,rhs) bod -> 
        CaseE scrt mp -> 
        IfE a b c -> 
        MkProdE xs -> 
        ProjE i e -> error "processVertex: TODO ProjE"
        TimeIt e ty b -> error "processVertex: TODO TimeIt"
        WithArenaE v e -> error "processVertex: TODO WithArenaE"
        SpawnE f locs args -> error "processVertex: TODO SpawnE"
        SyncE   -> error "processVertex: TODO SyncE"
        Ext _   -> error "processVertex: TODO Ext"
        MapE{}  -> error "processVertex: TODO MapE"
        FoldE{} -> error "processVertex: TODO FoldE"

getIndexFromVariableDataCon :: [(Exp1, Int, [Int])] -> Var -> Maybe (DataCon, Int)
getIndexFromVariableDataCon graph variable = case graph of 
         [] -> Nothing 
         x:xs -> let status = checkIndexVertex x variable
                   in case status of 
                       Nothing -> getIndexFromVariableDataCon xs variable
                       Just val -> Just val 

checkIndexVertex :: (Exp1, Int, [Int]) -> Var -> Maybe (DataCon, Int)
checkIndexVertex node variable = case node of 
    (exp, id, _) -> case exp of 
        DataConE loc dcon args -> let variables = [var | VarE var <- args]
                                      results   = map (variable ==) variables
                                      maybeIndex = L.elemIndex True results
                                    in case maybeIndex of 
                                            Nothing  -> Nothing 
                                            Just val -> Just (dcon, val) 
        _ -> Nothing    



-- S.toList (gFreeVars rhs)