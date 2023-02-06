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

{-
Type CFGfunctionMap: Mapping from function definition, to the control flow graph of the program. 
Edge : A tuple of expression and its likelihood. 
See Data.Graph in containers for more definitions. 
TODO: The functions for which the CFG should be annoted at the front-end level and they should be passable to this pass. 
Only generate CFG for functions which are annotated. 
-}
type CFGfunctionMap = M.Map FunDef1 (G.Graph, G.Vertex -> ( (Exp1, Int), Int, [Int]), Int -> Maybe G.Vertex)

{- Type VariableMap: Stores mapping from Variable to wheather it comes from a particular datacon. -}
type VariableMap = M.Map Var (Maybe (DataCon, Int))

{- Store the field graphs for each function-}
type FieldMap = M.Map FunDef1 (G.Graph, G.Vertex -> ((DataCon, Int), Int, [Int]), Int -> Maybe G.Vertex)


-- This is a PassM Prog1 just for testing purposes. So that is can be called in the pass pipeline. 
-- This should just be exportable as a function. ShuffleDataCon pass should call this pass eventually; Togehter with the constraint solver. 
generateCfg :: Prog1 -> PassM Prog1
generateCfg prg@Prog{ddefs, fundefs, mainExp} = do 
    let cfgFunctionMap = M.empty 
    newCfgFunctionMap <- generateCfgFunctions cfgFunctionMap (M.elems fundefs)
    let l1 = prg { ddefs = ddefs
               , fundefs = fundefs 
               , mainExp = mainExp
               }
    pure l1


{- Takes a map, list of function definitions, return update map with CFG for each funciton in the list -}
generateCfgFunctions :: CFGfunctionMap -> [FunDef1] -> PassM CFGfunctionMap
generateCfgFunctions cfgMap defs = 
    case defs of 
        [] -> pure cfgMap 
        x:xs -> do 
            (cfgMapNew, edgeList) <- generateCfgFunction cfgMap x
            newMap <- generateCfgFunctions cfgMap xs 
            {-dbgTraceIt (sdoc edgeList) dbgTraceIt ("\n") dbgTraceIt ("\n") dbgTraceIt ("\n")-}
            pure newMap
        
        
{- Generate a CFG for the corresponsing function -}            
generateCfgFunction :: CFGfunctionMap -> FunDef1 -> PassM (CFGfunctionMap, [((Exp1, Int) , Int, [Int])])
generateCfgFunction cfgMap f@FunDef { funName, funBody, funTy, funArgs } = do  
    (edgeList, succ, maxDepth) <- generateCFGExp 0 100 funBody
    let (graph, nodeFromVertex, vertexFromKey) = G.graphFromEdges edgeList
    let x  = topSort graph
    let x' = P.map nodeFromVertex x
    let datacon :: String = "Blog"
    let map = backtrackVariablesToDataConFields x'
    let edges = constructFieldGraph Nothing nodeFromVertex vertexFromKey x' x' map datacon   
    -- dbgTraceIt (sdoc varList) dbgTraceIt ("\n") dbgTraceIt (sdoc varList') dbgTraceIt ("\n") 
    -- dbgTraceIt (sdoc x') dbgTraceIt ("\n") dbgTraceIt (sdoc map) dbgTraceIt ("\n") 
    -- pure (cfgMap, edgeList)
    dbgTraceIt (sdoc x) dbgTraceIt ("\n") dbgTraceIt (sdoc edges) dbgTraceIt ("\n") pure (cfgMap, edgeList)


generateCFGExp :: Int -> Int -> Exp1 -> PassM ( [ ((Exp1, Int) , Int, [Int]) ] , Int, Int)
generateCFGExp vertexCounter edgeWeight exp1 = case exp1 of 
    -- Recursively do for args? for now assuming this is a leaf node (base case)
    -- In the future we should have a clear differentiation between a case binding that introduces variables that can be read.
    -- versus a let binding where a Data Constructure is being written and its fields are being materialized. 
    DataConE loc dcon args -> do 
        let edge = ( (exp1, edgeWeight), vertexCounter, [] )
        pure ([edge], vertexCounter, vertexCounter)
    VarE{} -> do
        let edge = ( (exp1, edgeWeight), vertexCounter, [] ) 
        pure ([edge], vertexCounter, vertexCounter)
    LitE{} -> do 
        let edge = ( (exp1, edgeWeight), vertexCounter, [] )
        pure ([edge], vertexCounter, vertexCounter)
    CharE{} -> do 
        let edge = ( (exp1, edgeWeight) , vertexCounter, [] )
        pure ([edge], vertexCounter, vertexCounter)
    FloatE{} -> do 
        let edge = ( (exp1, edgeWeight) , vertexCounter, [] )
        pure ([edge], vertexCounter, vertexCounter)
    LitSymE{} -> do 
        let edge = ( (exp1, edgeWeight) , vertexCounter, [])
        pure ([edge], vertexCounter, vertexCounter)
    AppE f locs args -> do
        (edgeList, succList, maxDepth) <- processExpListSeq (vertexCounter+1) edgeWeight args
        let edge     = ( ((VarE f), edgeWeight) , vertexCounter, succList )
        let newEdges = edgeList ++ [edge]
        pure (newEdges, vertexCounter, maxDepth)
    PrimAppE f args -> do 
        (edgeList, succList, maxDepth) <- processExpListSeq (vertexCounter+1) edgeWeight args
        let edge     = ( (exp1, edgeWeight) , vertexCounter, succList )
        let newEdges = edgeList ++ [edge]
        pure (newEdges, vertexCounter, maxDepth)
    LetE (v,loc,ty,rhs) bod -> do 
        (edgeList, succ, maxDepth) <- generateCFGExp (vertexCounter+1) edgeWeight bod
        let exp'  = LetE (v, loc, ty, rhs) $ VarE v
        let edge = ( (exp', edgeWeight) , vertexCounter, [succ] )
        let edgeList' = edgeList ++ [edge]
        pure (edgeList', vertexCounter, maxDepth)
    CaseE scrt mp -> do 
        (edgeList, succList, maxDepth) <- processExpSeqCase (vertexCounter+1) (edgeWeight `div` (P.length mp)) mp
        let edge     = ( (scrt, edgeWeight) , vertexCounter, succList )
        let newEdges  = edgeList ++ [edge]
        pure (newEdges, vertexCounter, maxDepth)
    IfE a b c -> do 
        (edgeListB, succB, d1) <- generateCFGExp (vertexCounter+1) (edgeWeight `div` 2) b 
        (edgeListC, succC, d2) <- generateCFGExp (d1+1) (edgeWeight `div` 2) c 
        let succList = [succB, succC]
        let edge     = ( (a, edgeWeight) , vertexCounter, succList )
        let newEdges = edgeListB ++ edgeListC ++ [edge]
        pure (newEdges, vertexCounter, P.maximum [d1, d2])
    MkProdE xs -> do 
        (edgeList, succList, maxDepth) <- processExpListSeq (vertexCounter+1) edgeWeight xs
        let edge      = ( (exp1, edgeWeight) , vertexCounter, succList )
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

{-
Process a list of expressions sequentially rather than in parallel as it would be though a Map 
Makes it much easier to thread, vertex id's and likelihoods. 
-}
processExpListSeq :: Int -> Int -> [Exp1] -> PassM ([((Exp1, Int), Int, [Int])] , [Int], Int)
processExpListSeq currVertex edgeWeight exp = case exp of 
    []   -> pure ([], [], currVertex)
    x:xs -> do 
        (edgeList, succ, maxDepth) <- generateCFGExp currVertex edgeWeight x
        (edgeList', succ', maxDepth') <- processExpListSeq (maxDepth+1) edgeWeight xs
        let newEdgeList = edgeList ++ edgeList'
        let succList    = [succ] ++ succ'
        pure (newEdgeList, succList, maxDepth') 

{-
Process list of case expressions sequentially. 
-}
processExpSeqCase :: Int -> Int -> [(DataCon, [(Var, loc)] , Exp1)] -> PassM ( [((Exp1, Int), Int, [Int])] , [Int], Int )
processExpSeqCase currVertex edgeWeight lst = case lst of 
    [] -> pure ([], [], currVertex)
    x:xs -> do 
        (edgeList, succ, maxDepth) <- generateVerticesCase currVertex edgeWeight x 
        (edgeList', succList, maxDepth') <- processExpSeqCase (maxDepth+1) edgeWeight xs
        let newEdgeList = edgeList ++ edgeList' 
        let succList'    = [succ] ++ succList
        pure (newEdgeList, succList', maxDepth')

{-
Helper function to generate a Vertex for each case binding. 
-}
generateVerticesCase :: Int -> Int -> (DataCon, [(Var, loc)] , Exp1) -> PassM ( [((Exp1, Int) , Int, [Int])] , Int, Int )
generateVerticesCase currVertex edgeWeight branch = do 
    let datacon      = fst3 branch 
    let fields_locs  = snd3 branch
    let fields       = P.map (\x -> ( VarE (fst x) )) fields_locs
    let dataconExp   = DataConE () datacon fields
    (edgeList, succ, maxDepth) <- generateCFGExp (currVertex+1) edgeWeight (thd3 branch) 
    let edge = ((dataconExp, edgeWeight) , currVertex, [succ])
    let newEdges = edgeList ++ [edge]
    pure (newEdges, currVertex, maxDepth) 
    

backtrackVariablesToDataConFields :: [((Exp1, Int) , Int, [Int])] -> VariableMap
backtrackVariablesToDataConFields graph = case graph of 
    [] -> M.empty -- No variable to process. 
    x:xs -> let newMap  = processVertex graph x M.empty
                mlist   = M.toList (newMap)
                m       = backtrackVariablesToDataConFields xs
                mlist'  = M.toList m
                newMap' = M.fromList (mlist ++ mlist')
             in newMap'


processVertex :: [((Exp1, Int) , Int, [Int])] -> ((Exp1, Int) , Int, [Int]) -> VariableMap -> VariableMap
processVertex graph node map = case node of 
    ((expression, likelihood) , id, succ) -> case expression of
                                                    DataConE loc dcon args -> let freeVariables = L.concat (P.map (\x -> S.toList (gFreeVars x)) args)
                                                                                  maybeIndexes  = P.map (getDataConIndexFromVariable graph) freeVariables 
                                                                                  mapList       = M.toList map 
                                                                                  newMapList    = P.zipWith (\x y -> (x, y)) freeVariables maybeIndexes
                                                                                in M.fromList (mapList ++ newMapList)

                                                    _                      -> map


getDataConIndexFromVariable :: [((Exp1, Int) , Int, [Int])] -> Var -> Maybe (DataCon, Int)
getDataConIndexFromVariable graph variable = case graph of 
         [] -> Nothing 
         x:xs -> let status = compareVariableWithDataConFields x variable
                   in case status of 
                       Nothing -> getDataConIndexFromVariable xs variable
                       Just val -> Just val 

compareVariableWithDataConFields :: ((Exp1, Int), Int, [Int]) -> Var -> Maybe (DataCon, Int)
compareVariableWithDataConFields node variable = case node of 
    ((exp, likelihood) , id, _) -> case exp of 
        DataConE loc dcon args -> let variables = [var | VarE var <- args]
                                      results   = P.map (variable ==) variables
                                      maybeIndex = L.elemIndex True results
                                    in case maybeIndex of 
                                            Nothing  -> Nothing 
                                            Just val -> Just (dcon, val) 
        _ -> Nothing


{- Return the freeVariables bound by an expression in Order -}
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


{-
From a given graph generate the Field ordering subgraph.
A subgraph that only contains Fields from the dataCons as Vertices. 
Edges amongst vertices amount to the READ ACCESS Patterns amongs the fields of the DataCon. 
For now, we only cares about read <-> read dependencies. 

RETURN: an edge list and corresponding weight of the the edges
Edge: a tuple from vertex to vertex, left dominates right. 

TODO: any FIXMEs in the function. 

c.) Multiple datacon fields read in the same expression, see FIXME below. 
    Since this will be run after flatten, it is safe to assume that only a maximum of two variables can be read in one let binding.  
-}

constructFieldGraph :: Maybe (DataCon, Int) -> (G.Vertex -> ( (Exp1, Int) , Int, [Int])) -> (Int -> Maybe G.Vertex) -> [((Exp1, Int) , Int, [Int])] -> [((Exp1, Int) , Int, [Int])] -> VariableMap -> DataCon -> [ ( (Int, Int) , Int ) ]
constructFieldGraph currField nodeFromVertex vertexFromNode graph progress map datacon = case progress of 
               [] -> [] 
               x:xs -> let ((exp, likelihood) , id'', successors) = x
                         in case exp of 
                            LitE val               -> []  ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon
                            CharE char             -> []  ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon
                            FloatE val             -> []  ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon
                            DataConE loc dcon args -> []  ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon

                            VarE var -> case currField of 
                                               {- No predessor from before demands an edge to be formed -}          
                                               Nothing -> let fromDataCon  = M.findWithDefault Nothing var map {- Check if the variable bound maps to a DataCon Field -}
                                                             in case fromDataCon of 

                                                                       {- Simple recursion onto next expression -}
                                                                       Nothing    -> [] ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon

                                                                       {- Yes, found a variable mapping to a DataCon -}
                                                                       {- Traverse, successors to check for available variables from dataCons -}
                                                                       Just field -> let (dcon, id) = field
                                                                                       in case (dcon == datacon) of 
                                                                                           True  ->   let succ'         = Mb.catMaybes $ P.map vertexFromNode successors
                                                                                                          succVertices  = P.map nodeFromVertex succ'
                                                                                                          succExp       = P.map (\x -> (fst . fst3) x) succVertices
                                                                                                          succprob      = P.map (\x -> (snd . fst3) x) succVertices
                                                                                                          {- list of list, where each list stores variables -}
                                                                                                          succDataCon   = P.map (\x -> findFieldInDataConFromVariableInExpression x graph map datacon) succExp 
                                                                                                          {- list of tuples, where each tuple == ([(dcon, id), ... ], likelihood)    -}
                                                                                                          succDataCon'  = P.zipWith (\x y -> (x, y)) succDataCon succprob 
                                                                                                          newEdges      = P.concat $ P.map (\x -> case x of 
                                                                                                                                             (varsl, prob) -> P.map (\y -> ( (id, snd y) , prob ) ) varsl       
                                                                                                                                    ) succDataCon'  
                                                                                                        in case newEdges of 
                                                                                                            {- No new edges, so recurse onto next expression -}
                                                                                                            [] -> [] ++ constructFieldGraph (Just field) nodeFromVertex vertexFromNode graph xs map datacon
                                                                                                            {- Found edges set currField to Nothing, recurse onto next node -}
                                                                                                            _  -> newEdges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon
                                                                                           _       -> [] ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon 
                                                                                         
                                               
                                               {- There is a predessor, waiting to find a partner to make an edge -}  
                                               Just (dcon, pred) -> case (dcon == datacon) of 
                                                                            True    -> let fromDataCon = M.findWithDefault Nothing var map
                                                                                         in case fromDataCon of 
                                                                                                    Nothing -> let  succ'         = Mb.catMaybes $ P.map vertexFromNode successors
                                                                                                                    succVertices  = P.map nodeFromVertex succ'
                                                                                                                    succExp       = P.map (\x -> (fst . fst3) x) succVertices
                                                                                                                    succprob      = P.map (\x -> (snd . fst3) x) succVertices
                                                                                                                    {- list of list, where each list stores variables -}
                                                                                                                    succDataCon   = P.map (\x -> findFieldInDataConFromVariableInExpression x graph map datacon) succExp 
                                                                                                                    {- list of tuples, where each tuple == ([(dcon, id), ... ], likelihood)    -}
                                                                                                                    succDataCon'  = P.zipWith (\x y -> (x, y)) succDataCon succprob 
                                                                                                                    newEdges      = P.concat $ P.map (\x -> case x of 
                                                                                                                        (varsl, prob) -> P.map (\y -> ( (pred, snd y) , prob ) ) varsl       
                                                                                                                        ) succDataCon'  
                                                                                                                 in case newEdges of 
                                                                                                                          [] -> [] ++ constructFieldGraph (Just (dcon, pred)) nodeFromVertex vertexFromNode graph xs map datacon
                                                                                                                          _  -> newEdges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon 
                                                                                                    {- Do I need to account for the successors here? -}
                                                                                                    Just field -> let (datacon, id') = field 
                                                                                                                      edges = [((pred, id'), likelihood)]
                                                                                                                    in edges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon
                                                                            {- This shouldn't technically arise the way we are desigining this pass -}
                                                                            _ -> error "ControlFlowGraph: This case shouldn't arise while making field dependence graph!"
                                                   
                                               
                                               
                                               
                                               
                                               


                            LitSymE var -> case currField of 
                                {- No predessor from before demands an edge to be formed -}          
                                Nothing -> let fromDataCon  = M.findWithDefault Nothing var map {- Check if the variable bound maps to a DataCon Field -}
                                              in case fromDataCon of 

                                                        {- Simple recursion onto next expression -}
                                                        Nothing    -> [] ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon

                                                        {- Yes, found a variable mapping to a DataCon -}
                                                        {- Traverse, successors to check for available variables from dataCons -}
                                                        Just field -> let (dcon, id) = field
                                                                        in case (dcon == datacon) of 
                                                                            True    -> let succ'         = Mb.catMaybes $ P.map vertexFromNode successors
                                                                                           succVertices  = P.map nodeFromVertex succ'
                                                                                           succExp       = P.map (\x -> (fst . fst3) x) succVertices
                                                                                           succprob      = P.map (\x -> (snd . fst3) x) succVertices
                                                                                           {- list of list, where each list stores variables -}
                                                                                           succDataCon   = P.map (\x -> findFieldInDataConFromVariableInExpression x graph map datacon) succExp 
                                                                                           {- list of tuples, where each tuple == ([(dcon, id), ... ], likelihood)    -}
                                                                                           succDataCon'  = P.zipWith (\x y -> (x, y)) succDataCon succprob 
                                                                                           newEdges      = P.concat $ P.map (\x -> case x of 
                                                                                            (varsl, prob) -> P.map (\y -> ( (id, snd y) , prob ) ) varsl       
                                                                                            ) succDataCon'   
                                                                                         in case newEdges of 
                                                                                             {- No new edges, so recurse onto next expression -}
                                                                                             [] -> [] ++ constructFieldGraph (Just field) nodeFromVertex vertexFromNode graph xs map datacon
                                                                                             {- Found edges set currField to Nothing, recurse onto next node -}
                                                                                             _  -> newEdges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon
                                                                            _       -> [] ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon 
                                                                          
                                
                                {- There is a predessor, waiting to find a partner to make an edge -}  
                                Just (dcon, pred) ->  case (dcon == datacon) of 
                                                             True -> let fromDataCon = M.findWithDefault Nothing var map
                                                                          in case fromDataCon of 
                                                                                     Nothing -> let  succ'         = Mb.catMaybes $ P.map vertexFromNode successors
                                                                                                     succVertices  = P.map nodeFromVertex succ'
                                                                                                     succExp       = P.map (\x -> (fst . fst3) x) succVertices
                                                                                                     succprob      = P.map (\x -> (snd . fst3) x) succVertices
                                                                                                     {- list of list, where each list stores variables -}
                                                                                                     succDataCon   = P.map (\x -> findFieldInDataConFromVariableInExpression x graph map datacon) succExp 
                                                                                                     {- list of tuples, where each tuple == ([(dcon, id), ... ], likelihood)    -}
                                                                                                     succDataCon'  = P.zipWith (\x y -> (x, y)) succDataCon succprob 
                                                                                                     newEdges      = P.concat $ P.map (\x -> case x of 
                                                                                                        (varsl, prob) -> P.map (\y -> ( (pred, snd y) , prob ) ) varsl       
                                                                                                      ) succDataCon'  
                                                                                                  in case newEdges of 
                                                                                                           [] -> [] ++ constructFieldGraph (Just (dcon, pred)) nodeFromVertex vertexFromNode graph xs map datacon
                                                                                                           _  -> newEdges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon 
                                                                      
                                                                                     Just field -> let (datacon, id) = field 
                                                                                                       edges = [((pred, id), likelihood) ]
                                                                                                     in edges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon
                                                             {- This shouldn't technically arise the way we are desigining this pass -}
                                                             _ -> error "ControlFlowGraph: This case shouldn't arise while making field dependence graph!"                                        

                            LetE (v,loc,ty,rhs) bod -> case currField of 
                                                            Nothing ->  let freeVars       =  freeVarsCFG rhs 
                                                                            fromDataCon'   = P.map (\v -> M.findWithDefault Nothing v map) freeVars
                                                                            fromDataCon''  = if P.null fromDataCon' then [Nothing] else fromDataCon'
                                                                            {- 
                                                                             FIXME: This only takes the head. This is not complete though. 
                                                                                    This makes the assumption that the current let binding or expression 
                                                                                    can only have only have one variable as a Field from the dataCon. 
                                                                                    This is not complete, if there are more than one fields, then 
                                                                                    draw edges from left to right order or them being found. 
                                                                                    Since freeVarsCFG is order preseving. 
                                                                                    Also duplicates should be removed since we care about first use. 
                                                                            -}
                                                                            fromDataCon    = P.head fromDataCon''                          
                                                                         in case fromDataCon of 
                                                                                  Nothing    -> [] ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon
                                                                                  Just field -> let (dcon, id) = field 
                                                                                                  in case (dcon == datacon) of 
                                                                                                          True ->    let succ'         = Mb.catMaybes $ P.map vertexFromNode successors
                                                                                                                         succVertices  = P.map nodeFromVertex succ'
                                                                                                                         succExp       = P.map (\x -> (fst . fst3) x) succVertices
                                                                                                                         succprob      = P.map (\x -> (snd . fst3) x) succVertices
                                                                                                                         {- list of list, where each list stores variables -}
                                                                                                                         succDataCon   = P.map (\x -> findFieldInDataConFromVariableInExpression x graph map datacon) succExp 
                                                                                                                         {- list of tuples, where each tuple == ([(dcon, id), ... ], likelihood)    -}
                                                                                                                         succDataCon'  = P.zipWith (\x y -> (x, y)) succDataCon succprob 
                                                                                                                         newEdges      = P.concat $ P.map (\x -> case x of 
                                                                                                                            (varsl, prob) -> P.map (\y -> ( (id, snd y) , prob ) ) varsl       
                                                                                                                          ) succDataCon'  
                                                                                                                      in case newEdges of 
                                                                                                                              [] -> [] ++ constructFieldGraph (Just field) nodeFromVertex vertexFromNode graph xs map datacon
                                                                                                                              _  -> newEdges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon
                                                                                                          _       -> [] ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon                                                      

                                                            Just (dcon, idd) -> case (dcon == datacon) of 
                                                                                      True ->    let freeVars       = freeVarsCFG rhs 
                                                                                                     fromDataCon'   = P.map (\v -> M.findWithDefault Nothing v map) freeVars
                                                                                                     fromDataCon''  = if P.null fromDataCon' then [Nothing] else fromDataCon'
                                                                                                     fromDataCon    = P.head fromDataCon''
                                                                                                  in case fromDataCon of 
                                                                                                            Nothing    -> let  succ'         = Mb.catMaybes $ P.map vertexFromNode successors
                                                                                                                               succVertices  = P.map nodeFromVertex succ'
                                                                                                                               succExp       = P.map (\x -> (fst . fst3) x) succVertices
                                                                                                                               succprob      = P.map (\x -> (snd . fst3) x) succVertices
                                                                                                                               {- list of list, where each list stores variables -}
                                                                                                                               succDataCon   = P.map (\x -> findFieldInDataConFromVariableInExpression x graph map datacon) succExp 
                                                                                                                               {- list of tuples, where each tuple == ([(dcon, id), ... ], likelihood)    -}
                                                                                                                               succDataCon'  = P.zipWith (\x y -> (x, y)) succDataCon succprob 
                                                                                                                               newEdges      = P.concat $ P.map (\x -> case x of 
                                                                                                                                (varsl, prob) -> P.map (\y -> ( (idd, snd y) , prob ) ) varsl       
                                                                                                                                ) succDataCon'   
                                                                                                                            in case newEdges of 
                                                                                                                                      [] -> [] ++ constructFieldGraph (Just (dcon, idd)) nodeFromVertex vertexFromNode graph xs map datacon
                                                                                                                                      _  -> newEdges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon 
         
                                                                                                            Just field -> let (dcon', id) = field 
                                                                                                                              edges = [((idd, id), likelihood)]
                                                                                                                            in edges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon
                                                                                      _       -> error "ControlFlowGraph: This case shouldn't arise while making field dependence graph!"


                            AppE f locs args  ->  case currField of 
                                Nothing ->  let freeVars       =  freeVarsCFG exp 
                                                fromDataCon'   = P.map (\v -> M.findWithDefault Nothing v map) freeVars
                                                fromDataCon''  = if P.null fromDataCon' then [Nothing] else fromDataCon'
                                                {- 
                                                 FIXME: This only takes the head. This is not complete though. 
                                                        This makes the assumption that the current let binding or expression 
                                                        can only have only have one variable as a Field from the dataCon. 
                                                        This is not complete, if there are more than one fields, then 
                                                        draw edges from left to right order or them being found. 
                                                        Since freeVarsCFG is order preseving. 
                                                        Also duplicates should be removed since we care about first use. 
                                                -}
                                                fromDataCon    = P.head fromDataCon''                          
                                             in case fromDataCon of 
                                                      Nothing    -> [] ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon
                                                      Just field -> let (dcon, id) = field 
                                                                      in case (dcon == datacon) of 
                                                                              True    -> let succ'         = Mb.catMaybes $ P.map vertexFromNode successors
                                                                                             succVertices  = P.map nodeFromVertex succ'
                                                                                             succExp       = P.map (\x -> (fst . fst3) x) succVertices
                                                                                             succprob      = P.map (\x -> (snd . fst3) x) succVertices
                                                                                             {- list of list, where each list stores variables -}
                                                                                             succDataCon   = P.map (\x -> findFieldInDataConFromVariableInExpression x graph map datacon) succExp 
                                                                                             {- list of tuples, where each tuple == ([(dcon, id), ... ], likelihood)    -}
                                                                                             succDataCon'  = P.zipWith (\x y -> (x, y)) succDataCon succprob 
                                                                                             newEdges      = P.concat $ P.map (\x -> case x of 
                                                                                                                                      (varsl, prob) -> P.map (\y -> ( (id, snd y) , prob ) ) varsl       
                                                                                                                              ) succDataCon'    
                                                                                          in case newEdges of 
                                                                                                  [] -> [] ++ constructFieldGraph (Just field) nodeFromVertex vertexFromNode graph xs map datacon
                                                                                                  _  -> newEdges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon
                                                                              _       -> [] ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon                                                      

                                Just (dcon, idd) -> case (dcon == datacon) of 
                                                          True    -> let freeVars       = freeVarsCFG exp 
                                                                         fromDataCon'   = P.map (\v -> M.findWithDefault Nothing v map) freeVars
                                                                         fromDataCon''  = if P.null fromDataCon' then [Nothing] else fromDataCon'
                                                                         fromDataCon    = P.head fromDataCon''
                                                                      in case fromDataCon of 
                                                                                Nothing    ->  let succ'         = Mb.catMaybes $ P.map vertexFromNode successors
                                                                                                   succVertices  = P.map nodeFromVertex succ'
                                                                                                   succExp       = P.map (\x -> (fst . fst3) x) succVertices
                                                                                                   succprob      = P.map (\x -> (snd . fst3) x) succVertices
                                                                                                   {- list of list, where each list stores variables -}
                                                                                                   succDataCon   = P.map (\x -> findFieldInDataConFromVariableInExpression x graph map datacon) succExp 
                                                                                                   {- list of tuples, where each tuple == ([(dcon, id), ... ], likelihood)    -}
                                                                                                   succDataCon'  = P.zipWith (\x y -> (x, y)) succDataCon succprob 
                                                                                                   newEdges      = P.concat $ P.map (\x -> case x of 
                                                                                                                                              (varsl, prob) -> P.map (\y -> ( (idd, snd y) , prob ) ) varsl       
                                                                                                                                    ) succDataCon'   
                                                                                                in case newEdges of 
                                                                                                          [] -> [] ++ constructFieldGraph (Just (dcon, idd)) nodeFromVertex vertexFromNode graph xs map datacon
                                                                                                          _  -> newEdges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon 

                                                                                Just field -> let (dcon', id) = field 
                                                                                                  edges = [((idd, id), likelihood)]
                                                                                                in edges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon
                                                          _       -> error "ControlFlowGraph: This case shouldn't arise while making field dependence graph!"


                            
                            PrimAppE f args  -> case currField of 
                                Nothing ->  let freeVars       =  freeVarsCFG exp 
                                                fromDataCon'   = P.map (\v -> M.findWithDefault Nothing v map) freeVars
                                                fromDataCon''  = if P.null fromDataCon' then [Nothing] else fromDataCon'
                                                {- 
                                                 FIXME: This only takes the head. This is not complete though. 
                                                        This makes the assumption that the current let binding or expression 
                                                        can only have only have one variable as a Field from the dataCon. 
                                                        This is not complete, if there are more than one fields, then 
                                                        draw edges from left to right order or them being found. 
                                                        Since freeVarsCFG is order preseving. 
                                                        Also duplicates should be removed since we care about first use. 
                                                -}
                                                fromDataCon    = P.head fromDataCon''                          
                                             in case fromDataCon of 
                                                      Nothing    -> [] ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon
                                                      Just field -> let (dcon, id) = field 
                                                                      in case (dcon == datacon) of 
                                                                              True ->    let succ'         = Mb.catMaybes $ P.map vertexFromNode successors
                                                                                             succVertices  = P.map nodeFromVertex succ'
                                                                                             succExp       = P.map (\x -> (fst . fst3) x) succVertices
                                                                                             succprob      = P.map (\x -> (snd . fst3) x) succVertices
                                                                                             {- list of list, where each list stores variables -}
                                                                                             succDataCon   = P.map (\x -> findFieldInDataConFromVariableInExpression x graph map datacon) succExp 
                                                                                             {- list of tuples, where each tuple == ([(dcon, id), ... ], likelihood)    -}
                                                                                             succDataCon'  = P.zipWith (\x y -> (x, y)) succDataCon succprob 
                                                                                             newEdges      = P.concat $ P.map (\x -> case x of 
                                                                                                                                       (varsl, prob) -> P.map (\y -> ( (id, snd y) , prob ) ) varsl       
                                                                                                                              ) succDataCon'  
                                                                                          in case newEdges of 
                                                                                                  [] -> [] ++ constructFieldGraph (Just field) nodeFromVertex vertexFromNode graph xs map datacon
                                                                                                  _  -> newEdges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon
                                                                              _       -> [] ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon                                                      

                                Just (dcon, idd) -> case (dcon == datacon) of 
                                                          True    -> let freeVars       = freeVarsCFG exp 
                                                                         fromDataCon'   = P.map (\v -> M.findWithDefault Nothing v map) freeVars
                                                                         fromDataCon''  = if P.null fromDataCon' then [Nothing] else fromDataCon'
                                                                         fromDataCon    = P.head fromDataCon''
                                                                      in case fromDataCon of 
                                                                                Nothing    ->  let succ'         = Mb.catMaybes $ P.map vertexFromNode successors
                                                                                                   succVertices  = P.map nodeFromVertex succ'
                                                                                                   succExp       = P.map (\x -> (fst . fst3) x) succVertices
                                                                                                   succprob      = P.map (\x -> (snd . fst3) x) succVertices
                                                                                                   {- list of list, where each list stores variables -}
                                                                                                   succDataCon   = P.map (\x -> findFieldInDataConFromVariableInExpression x graph map datacon) succExp 
                                                                                                   {- list of tuples, where each tuple == ([(dcon, id), ... ], likelihood)    -}
                                                                                                   succDataCon'  = P.zipWith (\x y -> (x, y)) succDataCon succprob 
                                                                                                   newEdges      = P.concat $ P.map (\x -> case x of 
                                                                                                                                            (varsl, prob) -> P.map (\y -> ( (idd, snd y) , prob ) ) varsl       
                                                                                                                                    ) succDataCon'
                                                                                                in case newEdges of 
                                                                                                          [] -> [] ++ constructFieldGraph (Just (dcon, idd)) nodeFromVertex vertexFromNode graph xs map datacon
                                                                                                          _  -> newEdges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon 

                                                                                Just field -> let (dcon', id) = field 
                                                                                                  edges = [((idd, id), likelihood)]
                                                                                                in edges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon
                                                          _       -> error "ControlFlowGraph: This case shouldn't arise while making field dependence graph!"

                            MkProdE xss -> case currField of 
                                Nothing ->  let freeVars       =  freeVarsCFG exp 
                                                fromDataCon'   = P.map (\v -> M.findWithDefault Nothing v map) freeVars
                                                fromDataCon''  = if P.null fromDataCon' then [Nothing] else fromDataCon'
                                                {- 
                                                 FIXME: This only takes the head. This is not complete though. 
                                                        This makes the assumption that the current let binding or expression 
                                                        can only have only have one variable as a Field from the dataCon. 
                                                        This is not complete, if there are more than one fields, then 
                                                        draw edges from left to right order or them being found. 
                                                        Since freeVarsCFG is order preseving. 
                                                        Also duplicates should be removed since we care about first use. 
                                                -}
                                                fromDataCon    = P.head fromDataCon''                          
                                             in case fromDataCon of 
                                                      Nothing    -> [] ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon
                                                      Just field -> let (dcon, id) = field 
                                                                      in case (dcon == datacon) of 
                                                                              True    -> let succ'         = Mb.catMaybes $ P.map vertexFromNode successors
                                                                                             succVertices  = P.map nodeFromVertex succ'
                                                                                             succExp       = P.map (\x -> (fst . fst3) x) succVertices
                                                                                             succprob      = P.map (\x -> (snd . fst3) x) succVertices
                                                                                             {- list of list, where each list stores variables -}
                                                                                             succDataCon   = P.map (\x -> findFieldInDataConFromVariableInExpression x graph map datacon) succExp 
                                                                                             {- list of tuples, where each tuple == ([(dcon, id), ... ], likelihood)    -}
                                                                                             succDataCon'  = P.zipWith (\x y -> (x, y)) succDataCon succprob 
                                                                                             newEdges      = P.concat $ P.map (\x -> case x of 
                                                                                                                                       (varsl, prob) -> P.map (\y -> ( (id, snd y) , prob ) ) varsl       
                                                                                                                              ) succDataCon'  
                                                                                          in case newEdges of 
                                                                                                  [] -> [] ++ constructFieldGraph (Just field) nodeFromVertex vertexFromNode graph xs map datacon
                                                                                                  _  -> newEdges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon
                                                                              _       -> [] ++ constructFieldGraph currField nodeFromVertex vertexFromNode graph xs map datacon                                                      

                                Just (dcon, idd) -> case (dcon == datacon) of 
                                                          True    -> let freeVars       = freeVarsCFG exp 
                                                                         fromDataCon'   = P.map (\v -> M.findWithDefault Nothing v map) freeVars
                                                                         fromDataCon''  = if P.null fromDataCon' then [Nothing] else fromDataCon'
                                                                         fromDataCon    = P.head fromDataCon''
                                                                      in case fromDataCon of 
                                                                                Nothing    ->  let succ'         = Mb.catMaybes $ P.map vertexFromNode successors
                                                                                                   succVertices  = P.map nodeFromVertex succ'
                                                                                                   succExp       = P.map (\x -> (fst . fst3) x) succVertices
                                                                                                   succprob      = P.map (\x -> (snd . fst3) x) succVertices
                                                                                                   {- list of list, where each list stores variables -}
                                                                                                   succDataCon   = P.map (\x -> findFieldInDataConFromVariableInExpression x graph map datacon) succExp 
                                                                                                   {- list of tuples, where each tuple == ([(dcon, id), ... ], likelihood)    -}
                                                                                                   succDataCon'  = P.zipWith (\x y -> (x, y)) succDataCon succprob 
                                                                                                   newEdges      = P.concat $ P.map (\x -> case x of 
                                                                                                                                            (varsl, prob) -> P.map (\y -> ( (idd, snd y) , prob ) ) varsl       
                                                                                                                                    ) succDataCon'  
                                                                                                in case newEdges of 
                                                                                                          [] -> [] ++ constructFieldGraph (Just (dcon, idd)) nodeFromVertex vertexFromNode graph xs map datacon
                                                                                                          _  -> newEdges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon 

                                                                                Just field -> let (dcon', id) = field 
                                                                                                  edges = [((idd, id), likelihood)]
                                                                                                in edges ++ constructFieldGraph Nothing nodeFromVertex vertexFromNode graph xs map datacon
                                                          _       -> error "ControlFlowGraph: This case shouldn't arise while making field dependence graph!"


{- 
From an expression provided, Recursively find all the variables that come from a DataCon expression, that is, are fields in a DataConE.  
-}
findFieldInDataConFromVariableInExpression :: Exp1 -> [((Exp1, Int) , Int, [Int])] -> VariableMap -> DataCon -> [(DataCon, Int)]
findFieldInDataConFromVariableInExpression exp graph map datacon = case exp of
    VarE var -> let fromDataCon  = M.findWithDefault Nothing var map
                  in case fromDataCon of 
                       Nothing -> [] 
                       Just (dcon, id)  -> if dcon == datacon then [(dcon, id)] else []

    LitSymE var -> let fromDataCon = M.findWithDefault Nothing var map
                    in case fromDataCon of 
                       Nothing -> [] 
                       Just (dcon, id)  -> if dcon == datacon then [(dcon, id)] else []
                                            

    LetE (v,loc,ty,rhs) bod -> let freeVars = freeVarsCFG rhs 
                                   fromDataCon  = P.map (\v -> M.findWithDefault Nothing v map) freeVars 
                                   removeMaybe  = Mb.catMaybes fromDataCon 
                                   newDatacons  = [ if dcon == datacon then Just (dcon, id) else Nothing | (dcon, id) <- removeMaybe ]
                                   newDatacons' = Mb.catMaybes newDatacons
                                 in newDatacons'

    AppE f locs args  ->  let freeVars = freeVarsCFG exp 
                              fromDataCon = P.map (\v -> M.findWithDefault Nothing v map) freeVars
                              removeMaybe  = Mb.catMaybes fromDataCon 
                              newDatacons  = [ if dcon == datacon then Just (dcon, id) else Nothing | (dcon, id) <- removeMaybe ]
                              newDatacons' = Mb.catMaybes newDatacons
                            in newDatacons' 
                            
    PrimAppE f args  ->  let freeVars =  freeVarsCFG exp 
                             fromDataCon = P.map (\v -> M.findWithDefault Nothing v map) freeVars
                             removeMaybe  = Mb.catMaybes fromDataCon 
                             newDatacons  = [ if dcon == datacon then Just (dcon, id) else Nothing | (dcon, id) <- removeMaybe ]
                             newDatacons' = Mb.catMaybes newDatacons
                            in newDatacons'

    LitE val -> []   
    CharE char -> [] 
    FloatE val -> []  

    DataConE loc dcon args -> [] 

    MkProdE xss -> let freeVars =  freeVarsCFG exp 
                       fromDataCon = P.map (\v -> M.findWithDefault Nothing v map) freeVars
                       removeMaybe  = Mb.catMaybes fromDataCon 
                       newDatacons  = [ if dcon == datacon then Just (dcon, id) else Nothing | (dcon, id) <- removeMaybe ]
                       newDatacons' = Mb.catMaybes newDatacons
                     in newDatacons' 