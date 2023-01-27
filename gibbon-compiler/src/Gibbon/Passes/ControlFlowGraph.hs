module Gibbon.Passes.ControlFlowGraph (generateCfg) where

import Data.Graph as G
import Data.Map as M

import Gibbon.Common
import Gibbon.L1.Syntax as L1 
import Prelude as P

-- Type to store the CFG's for each function that apprears in code.
-- We should use annotations form the front end to onlt contruct CFG's 
-- for functions that are annotated. 
type CFGfunctionMap = M.Map FunDef1 (G.Graph, G.Table Exp1)

-- For now make this return, the CFGFunctionMap 
-- But this should ideally return the Constraints 
-- which should then be passed to the constraint solver. 
generateCfg :: Prog1 -> PassM CFGfunctionMap
generateCfg prg@Prog{ddefs, fundefs, mainExp} = do 
    let cfgFunctionMap = M.empty 
    newCfgFunctionMap <- generateCfgFunction cfgFunctionMap (M.elems fundefs)
    pure newCfgFunctionMap


generateCfgFunction :: CFGfunctionMap -> [FunDef1] -> PassM CFGfunctionMap
generateCfgFunction cfgMap defs = do 
    pure cfgMap
        
        
    


