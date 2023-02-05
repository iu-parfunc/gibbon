module Gibbon.Passes.ControlFlowGraph (generateCfg) where

import Data.Graph as G
import Data.Map as M
import Data.Set as S
import Data.List as L

import Gibbon.Common
import Gibbon.L1.Syntax as L1 
import Prelude as P
import Control.Monad as Mo

import Data.Maybe as Mb


-- Type to store the CFG's for each function that apprears in code.
-- We should use annotations form the front end to onlt contruct CFG's 
-- for functions that are annotated. 

-- Need to store additional parameter of likelihood in the CFGMap
-- Should Thread an additional Float parameter that is the likelihood of hitting that vertex while making the CFG
type CFGfunctionMap = M.Map FunDef1 (G.Graph, G.Vertex -> ( Exp1, Int, [Int]), Int -> Maybe G.Vertex)

type VariableMap = M.Map Var (Maybe (DataCon, Int))

type FieldMap = M.Map FunDef1 (G.Graph, G.Vertex -> ((DataCon, Int), Int, [Int]), Int -> Maybe G.Vertex)

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


removeDuplicates :: [Var] -> [Var]
removeDuplicates l = case l of 
    [] -> []
    x:xs -> x:removeDuplicates (L.filter (/=x) xs)
        
        
generateCfgFunction :: CFGfunctionMap -> FunDef1 -> PassM (CFGfunctionMap, [(Exp1, Int, [Int])])
generateCfgFunction cfgMap f@FunDef { funName, funBody, funTy, funArgs } = do  
    (edgeList, succ, maxDepth) <- generateCFGExp 0 funBody
    let (graph, nodeFromVertex, vertexFromKey) = G.graphFromEdges edgeList
    let x  = topSort graph
    let x' = P.map nodeFromVertex x
    let datacon :: String = "Leaf"
    let map = generateVariableToDataCon x'
    --let varList = generateFieldGraph x'
    --let varList' = removeDuplicates varList
    let edges = constructFieldGraph Nothing nodeFromVertex vertexFromKey x' x' map datacon   

    -- dbgTraceIt (sdoc varList) dbgTraceIt ("\n") dbgTraceIt (sdoc varList') dbgTraceIt ("\n") 
    
    -- dbgTraceIt (sdoc x') dbgTraceIt ("\n") dbgTraceIt (sdoc map) dbgTraceIt ("\n") 
    -- pure (cfgMap, edgeList)
    dbgTraceIt (sdoc x') dbgTraceIt ("\n") dbgTraceIt (sdoc edges) dbgTraceIt ("\n") pure (cfgMap, edgeList)


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
generateVariableToDataCon vertices = case vertices of 
    [] -> M.empty
    x:xs -> let newMap = processVertex vertices x M.empty
                l      = M.toList (newMap)
                m      = generateVariableToDataCon xs
                l'     = M.toList m
                newMap' = M.fromList (l ++ l')
             in newMap'


processVertex :: [(Exp1, Int, [Int])] -> (Exp1, Int, [Int]) -> VariableMap -> VariableMap
processVertex graph node map = case node of 
    (expression, id, succ) -> let newMap = processExpVertex graph expression map
                                in newMap


processExpVertex :: [(Exp1, Int, [Int])] -> Exp1 -> VariableMap -> VariableMap
processExpVertex graph exp map = case exp of
    DataConE loc dcon args -> let freeVariables = L.concat (P.map (\x -> S.toList (gFreeVars x)) args)
                                  maybeIndexes  = P.map (getIndexFromVariableDataCon graph) freeVariables 
                                  l    = M.toList map 
                                  l'   = P.zipWith (\x y -> (x, y)) freeVariables maybeIndexes
                                in M.fromList (l ++ l')

    _                     -> map


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
                                      results   = P.map (variable ==) variables
                                      maybeIndex = L.elemIndex True results
                                    in case maybeIndex of 
                                            Nothing  -> Nothing 
                                            Just val -> Just (dcon, val) 
        _ -> Nothing


freeVarsCFG :: Exp1 -> [Var]
freeVarsCFG exp = case exp of
    DataConE loc dcon args -> let var_list_list = P.map (freeVarsCFG) args
                                  var_list      = L.concat var_list_list
                                in var_list  
    VarE var -> [var]
    LitE val -> []
    CharE char -> []
    FloatE val -> []
    LitSymE var -> [var]
    AppE f locs args -> let var_list_list = P.map (freeVarsCFG) args
                            var_list      = L.concat var_list_list
                         in var_list
                              
    PrimAppE f args -> let var_list_list = P.map (freeVarsCFG) args
                           var_list      = L.concat var_list_list
                         in var_list

    LetE (v,loc,ty,rhs) bod -> freeVarsCFG rhs 
    -- CaseE scrt mp -> not there in cfg node
    -- IfE a b c -> not there in cfg node
    MkProdE xs -> let var_list_list = P.map (freeVarsCFG) xs
                      var_list      = L.concat var_list_list
                    in var_list

    ProjE i e -> error "ControlFlowGraph: TODO ProjE"
    TimeIt e ty b -> error "ControlFlowGraph: TODO TimeIt"
    WithArenaE v e -> error "ControlFlowGraph: TODO WithArenaE"
    SpawnE f locs args -> error "ControlFlowGraph: TODO SpawnE"
    SyncE   -> error "ControlFlowGraph: TODO SyncE"
    Ext _   -> error "ControlFlowGraph: TODO Ext"
    MapE{}  -> error "ControlFlowGraph: TODO MapE"
    FoldE{} -> error "ControlFlowGraph: TODO FoldE" 


constructFieldGraph :: Maybe Int -> (G.Vertex -> ( Exp1, Int, [Int])) -> (Int -> Maybe G.Vertex) -> [(Exp1, Int, [Int])] -> [(Exp1, Int, [Int])] -> VariableMap -> DataCon -> [(Int, Int)]
constructFieldGraph currField nodeFromVertex vertexFromNode graph progress map datacon = case progress of 
               [] -> [] 
               x:xs -> let (exp, id, successors) = x
                         in case exp of 

                            LitE val -> []   ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon

                            CharE char -> []  ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon
                            
                            FloatE val -> []  ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon

                            DataConE loc dcon args -> [] ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon

                            VarE var -> case currField of 
                                               Nothing -> let fromDataCon  = M.findWithDefault Nothing var map
                                                             in case fromDataCon of 
                                                                       Nothing -> [] ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon
                                                                       Just field -> let succ'         = Mb.catMaybes $ P.map vertexFromNode successors
                                                                                         succVertices  = P.map nodeFromVertex succ'
                                                                                         succExp       = P.map (\x -> fst3 x) succVertices
                                                                                         succDataCon   = P.concat $ P.map (\x -> succDataCons x graph map datacon) succExp 
                                                                                         successorsIds = P.map (\x -> snd x) succDataCon
                                                                                         (datacon, id) = field 
                                                                                         newEdges      = P.map (\x -> (id, x)) successorsIds    
                                                                                       in case newEdges of 
                                                                                                [] -> [] ++ constructFieldGraph (Just id) nodeFromVertex vertexFromNode graph xs map datacon
                                                                                                _  -> newEdges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon
                                                
                                               Just pred -> let fromDataCon = M.findWithDefault Nothing var map
                                                               in case fromDataCon of 
                                                                         Nothing -> let succ'         = Mb.catMaybes $ P.map vertexFromNode successors
                                                                                        succVertices  = P.map nodeFromVertex succ'
                                                                                        succExp       = P.map (\x -> fst3 x) succVertices
                                                                                        succDataCon   = P.concat $ P.map (\x -> succDataCons x graph map datacon) succExp 
                                                                                        successorsIds = P.map (\x -> snd x) succDataCon 
                                                                                        newEdges      = P.map (\x -> (pred, x)) successorsIds    
                                                                                      in case newEdges of 
                                                                                                  [] -> [] ++ constructFieldGraph (Just pred) nodeFromVertex vertexFromNode graph xs map datacon
                                                                                                  _  -> newEdges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon 
                                                                         Just field -> let (datacon, id) = field 
                                                                                           edges = [(pred, id)]
                                                                                         in edges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon

                            LitSymE var -> case currField of 
                                               Nothing -> let fromDataCon  = M.findWithDefault Nothing var map
                                                             in case fromDataCon of 
                                                                       Nothing -> [] ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon
                                                                       Just field -> let succ'         = Mb.catMaybes $ P.map vertexFromNode successors
                                                                                         succVertices  = P.map nodeFromVertex succ'
                                                                                         succExp       = P.map (\x -> fst3 x) succVertices
                                                                                         succDataCon   = P.concat $ P.map (\x -> succDataCons x graph map datacon) succExp 
                                                                                         successorsIds = P.map (\x -> snd x) succDataCon
                                                                                         (datacon, id) = field 
                                                                                         newEdges      = P.map (\x -> (id, x)) successorsIds    
                                                                                       in case newEdges of 
                                                                                                [] -> [] ++ constructFieldGraph (Just id) nodeFromVertex vertexFromNode graph xs map datacon
                                                                                                _  -> newEdges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon
                                                
                                               Just pred -> let fromDataCon = M.findWithDefault Nothing var map
                                                               in case fromDataCon of 
                                                                         Nothing -> let succ'         = Mb.catMaybes $ P.map vertexFromNode successors
                                                                                        succVertices  = P.map nodeFromVertex succ'
                                                                                        succExp       = P.map (\x -> fst3 x) succVertices
                                                                                        succDataCon   = P.concat $ P.map (\x -> succDataCons x graph map datacon) succExp 
                                                                                        successorsIds = P.map (\x -> snd x) succDataCon 
                                                                                        newEdges      = P.map (\x -> (pred, x)) successorsIds    
                                                                                      in case newEdges of 
                                                                                                  [] -> [] ++ constructFieldGraph (Just pred) nodeFromVertex vertexFromNode graph xs map datacon
                                                                                                  _  -> newEdges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon 
                                                                         Just field -> let (datacon, id) = field 
                                                                                           edges = [(pred, id)]
                                                                                         in edges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon                                        

                            LetE (v,loc,ty,rhs) bod -> case currField of 
                                                            Nothing ->  let freeVars       =  freeVarsCFG rhs 
                                                                            fromDataCon'   = P.map (\v -> M.findWithDefault Nothing v map) freeVars
                                                                            fromDataCon''  = if P.null fromDataCon' then [Nothing] else fromDataCon'
                                                                            fromDataCon    = P.head fromDataCon''                          -- FIXME: This only takes the head, but what if there are more free variables bound that belong to more than one field ? 
                                                                         in case fromDataCon of 
                                                                                  Nothing -> [] ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon
                                                                                  Just field -> let succ'         = Mb.catMaybes $ P.map vertexFromNode successors
                                                                                                    succVertices  = P.map nodeFromVertex succ'
                                                                                                    succExp       = P.map (\x -> fst3 x) succVertices
                                                                                                    succDataCon   = P.concat $ P.map (\x -> succDataCons x graph map datacon) succExp 
                                                                                                    successorsIds = P.map (\x -> snd x) succDataCon
                                                                                                    (datacon, id) = field 
                                                                                                    newEdges      = P.map (\x -> (id, x)) successorsIds    
                                                                                                 in case newEdges of 
                                                                                                        [] -> [] ++ constructFieldGraph (Just (snd field)) nodeFromVertex vertexFromNode graph xs map datacon
                                                                                                        _  -> newEdges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon

                                                            Just pred -> let freeVars       = freeVarsCFG rhs 
                                                                             fromDataCon'   = P.map (\v -> M.findWithDefault Nothing v map) freeVars
                                                                             fromDataCon''  = if P.null fromDataCon' then [Nothing] else fromDataCon'
                                                                             fromDataCon    = P.head fromDataCon''
                                                                          in case fromDataCon of 
                                                                                    Nothing    -> let succ'         = Mb.catMaybes $ P.map vertexFromNode successors
                                                                                                      succVertices  = P.map nodeFromVertex succ'
                                                                                                      succExp       = P.map (\x -> fst3 x) succVertices
                                                                                                      succDataCon   = P.concat $ P.map (\x -> succDataCons x graph map datacon) succExp 
                                                                                                      successorsIds = P.map (\x -> snd x) succDataCon 
                                                                                                      newEdges      = P.map (\x -> (pred, x)) successorsIds    
                                                                                                    in case newEdges of 
                                                                                                            [] -> [] ++ constructFieldGraph (Just pred) nodeFromVertex vertexFromNode graph xs map datacon
                                                                                                            _  -> newEdges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon 

                                                                                    Just field -> let (datacon, id) = field 
                                                                                                      edges = [(pred, id)]
                                                                                                    in edges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon 





                            AppE f locs args  ->  case currField of 
                                                            Nothing ->  let freeVars       =  freeVarsCFG exp 
                                                                            fromDataCon'   = P.map (\v -> M.findWithDefault Nothing v map) freeVars
                                                                            fromDataCon''  = if P.null fromDataCon' then [Nothing] else fromDataCon'
                                                                            fromDataCon    = P.head fromDataCon''                          -- FIXME: This only takes the head, but what if there are more free variables bound that belong to more than one field ? 
                                                                         in case fromDataCon of 
                                                                                  Nothing -> [] ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon
                                                                                  Just field -> let succ'         = Mb.catMaybes $ P.map vertexFromNode successors
                                                                                                    succVertices  = P.map nodeFromVertex succ'
                                                                                                    succExp       = P.map (\x -> fst3 x) succVertices
                                                                                                    succDataCon   = P.concat $ P.map (\x -> succDataCons x graph map datacon) succExp 
                                                                                                    successorsIds = P.map (\x -> snd x) succDataCon
                                                                                                    (datacon, id) = field 
                                                                                                    newEdges      = P.map (\x -> (id, x)) successorsIds    
                                                                                                 in case newEdges of 
                                                                                                        [] -> [] ++ constructFieldGraph (Just (snd field)) nodeFromVertex vertexFromNode graph xs map datacon
                                                                                                        _  -> newEdges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon

                                                            Just pred -> let freeVars       = freeVarsCFG exp 
                                                                             fromDataCon'   = P.map (\v -> M.findWithDefault Nothing v map) freeVars
                                                                             fromDataCon''  = if P.null fromDataCon' then [Nothing] else fromDataCon'
                                                                             fromDataCon    = P.head fromDataCon''
                                                                          in case fromDataCon of 
                                                                                    Nothing    -> let succ'         = Mb.catMaybes $ P.map vertexFromNode successors
                                                                                                      succVertices  = P.map nodeFromVertex succ'
                                                                                                      succExp       = P.map (\x -> fst3 x) succVertices
                                                                                                      succDataCon   = P.concat $ P.map (\x -> succDataCons x graph map datacon) succExp 
                                                                                                      successorsIds = P.map (\x -> snd x) succDataCon 
                                                                                                      newEdges      = P.map (\x -> (pred, x)) successorsIds    
                                                                                                    in case newEdges of 
                                                                                                            [] -> [] ++ constructFieldGraph (Just pred) nodeFromVertex vertexFromNode graph xs map datacon
                                                                                                            _  -> newEdges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon 

                                                                                    Just field -> let (datacon, id) = field 
                                                                                                      edges = [(pred, id)]
                                                                                                    in edges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon


                            
                            PrimAppE f args  -> case currField of 
                                                            Nothing ->  let freeVars       =  freeVarsCFG exp 
                                                                            fromDataCon'   = P.map (\v -> M.findWithDefault Nothing v map) freeVars
                                                                            fromDataCon''  = if P.null fromDataCon' then [Nothing] else fromDataCon'
                                                                            fromDataCon    = P.head fromDataCon''                          -- FIXME: This only takes the head, but what if there are more free variables bound that belong to more than one field ? 
                                                                         in case fromDataCon of 
                                                                                  Nothing -> [] ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon
                                                                                  Just field -> let succ'         = Mb.catMaybes $ P.map vertexFromNode successors
                                                                                                    succVertices  = P.map nodeFromVertex succ'
                                                                                                    succExp       = P.map (\x -> fst3 x) succVertices
                                                                                                    succDataCon   = P.concat $ P.map (\x -> succDataCons x graph map datacon) succExp 
                                                                                                    successorsIds = P.map (\x -> snd x) succDataCon
                                                                                                    (datacon, id) = field 
                                                                                                    newEdges      = P.map (\x -> (id, x)) successorsIds    
                                                                                                 in case newEdges of 
                                                                                                        [] -> [] ++ constructFieldGraph (Just (snd field)) nodeFromVertex vertexFromNode graph xs map datacon
                                                                                                        _  -> newEdges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon

                                                            Just pred -> let freeVars       = freeVarsCFG exp 
                                                                             fromDataCon'   = P.map (\v -> M.findWithDefault Nothing v map) freeVars
                                                                             fromDataCon''  = if P.null fromDataCon' then [Nothing] else fromDataCon'
                                                                             fromDataCon    = P.head fromDataCon''
                                                                          in case fromDataCon of 
                                                                                    Nothing    -> let succ'         = Mb.catMaybes $ P.map vertexFromNode successors
                                                                                                      succVertices  = P.map nodeFromVertex succ'
                                                                                                      succExp       = P.map (\x -> fst3 x) succVertices
                                                                                                      succDataCon   = P.concat $ P.map (\x -> succDataCons x graph map datacon) succExp 
                                                                                                      successorsIds = P.map (\x -> snd x) succDataCon 
                                                                                                      newEdges      = P.map (\x -> (pred, x)) successorsIds    
                                                                                                    in case newEdges of 
                                                                                                            [] -> [] ++ constructFieldGraph (Just pred) nodeFromVertex vertexFromNode graph xs map datacon
                                                                                                            _  -> newEdges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon 

                                                                                    Just field -> let (datacon, id) = field 
                                                                                                      edges = [(pred, id)]
                                                                                                    in edges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon

                            MkProdE xss -> case currField of 
                                                            Nothing ->  let freeVars       =  freeVarsCFG exp 
                                                                            fromDataCon'   = P.map (\v -> M.findWithDefault Nothing v map) freeVars
                                                                            fromDataCon''  = if P.null fromDataCon' then [Nothing] else fromDataCon'
                                                                            fromDataCon    = P.head fromDataCon''                          -- FIXME: This only takes the head, but what if there are more free variables bound that belong to more than one field ? 
                                                                         in case fromDataCon of 
                                                                                  Nothing -> [] ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon
                                                                                  Just field -> let succ'         = Mb.catMaybes $ P.map vertexFromNode successors
                                                                                                    succVertices  = P.map nodeFromVertex succ'
                                                                                                    succExp       = P.map (\x -> fst3 x) succVertices
                                                                                                    succDataCon   = P.concat $ P.map (\x -> succDataCons x graph map datacon) succExp 
                                                                                                    successorsIds = P.map (\x -> snd x) succDataCon
                                                                                                    (datacon, id) = field 
                                                                                                    newEdges      = P.map (\x -> (id, x)) successorsIds    
                                                                                                 in case newEdges of 
                                                                                                        [] -> [] ++ constructFieldGraph (Just (snd field)) nodeFromVertex vertexFromNode graph xs map datacon
                                                                                                        _  -> newEdges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon

                                                            Just pred -> let freeVars       = freeVarsCFG exp 
                                                                             fromDataCon'   = P.map (\v -> M.findWithDefault Nothing v map) freeVars
                                                                             fromDataCon''  = if P.null fromDataCon' then [Nothing] else fromDataCon'
                                                                             fromDataCon    = P.head fromDataCon''
                                                                          in case fromDataCon of 
                                                                                    Nothing    -> let succ'         = Mb.catMaybes $ P.map vertexFromNode successors
                                                                                                      succVertices  = P.map nodeFromVertex succ'
                                                                                                      succExp       = P.map (\x -> fst3 x) succVertices
                                                                                                      succDataCon   = P.concat $ P.map (\x -> succDataCons x graph map datacon) succExp 
                                                                                                      successorsIds = P.map (\x -> snd x) succDataCon 
                                                                                                      newEdges      = P.map (\x -> (pred, x)) successorsIds    
                                                                                                    in case newEdges of 
                                                                                                            [] -> [] ++ constructFieldGraph (Just pred) nodeFromVertex vertexFromNode graph xs map datacon
                                                                                                            _  -> newEdges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon 

                                                                                    Just field -> let (datacon, id) = field 
                                                                                                      edges = [(pred, id)]
                                                                                                    in edges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon



succDataCons :: Exp1 -> [(Exp1, Int, [Int])] -> VariableMap -> DataCon -> [(DataCon, Int)]
succDataCons exp graph map datacon = case exp of
    VarE var -> let fromDataCon  = M.findWithDefault Nothing var map
                  in case fromDataCon of 
                       Nothing -> [] 
                       Just x  -> [x]

    LitSymE var -> let fromDataCon = M.findWithDefault Nothing var map
                    in case fromDataCon of 
                       Nothing -> [] 
                       Just x  -> [x]
                                            

    LetE (v,loc,ty,rhs) bod -> let freeVars = freeVarsCFG rhs 
                                   fromDataCon = P.map (\v -> M.findWithDefault Nothing v map) freeVars 
                                 in Mb.catMaybes fromDataCon 

    AppE f locs args  ->  let freeVars = freeVarsCFG exp 
                              fromDataCon = P.map (\v -> M.findWithDefault Nothing v map) freeVars
                            in Mb.catMaybes fromDataCon 
                            
    PrimAppE f args  ->  let freeVars =  freeVarsCFG exp 
                             fromDataCon = P.map (\v -> M.findWithDefault Nothing v map) freeVars
                            in Mb.catMaybes fromDataCon 

    LitE val -> []   
    CharE char -> [] 
    FloatE val -> []  

    DataConE loc dcon args -> [] 

    MkProdE xss -> let freeVars =  freeVarsCFG exp 
                       fromDataCon = P.map (\v -> M.findWithDefault Nothing v map) freeVars
                      in Mb.catMaybes fromDataCon 