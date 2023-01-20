{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Gibbon.Passes.ShuffleFieldOrdering
    (shuffleDataCon) where  


import Control.Monad ( when, forM )
import Data.Foldable
import Data.List as L
import Data.Map as M
import Data.Maybe ( fromJust )
import qualified Data.Set as Set

import Text.PrettyPrint.GenericPretty

import Gibbon.Common
import Gibbon.DynFlags
import Gibbon.Passes.AddTraversals ( needsTraversalCase )
import Gibbon.L1.Syntax as L1 
import Gibbon.L2.Syntax as L2

import Prelude as P


shuffleDataCon :: Prog1 -> PassM Prog1
shuffleDataCon prg@Prog{ddefs,fundefs,mainExp} = do
    let shuffled_ddefs = findDataCon ddefs
    let l1 = prg { ddefs = shuffled_ddefs
               , fundefs = fundefs 
               , mainExp = mainExp
               }
    pure l1

findDataCon :: DDefs (UrTy a) -> DDefs (UrTy a)
findDataCon ddefs = M.fromList (go (M.toList ddefs))
    where
        go list = case list of 
                    [] -> [] 
                    x:xs -> case x of 
                                (var, ddef) -> let new_ddef = reverse_ddef ddef
                                                   in [(var, new_ddef)] ++ (go xs)
                                _  -> [x] ++ (go xs)



reverse_ddef :: DDef (UrTy a) -> DDef (UrTy a)
reverse_ddef DDef{tyName, tyArgs, dataCons} =  case tyName of 
    "Blog" -> let newDataCons = reverse_dataCons dataCons 
                in DDef{tyName, tyArgs, dataCons=newDataCons}
    _      -> DDef{tyName, tyArgs, dataCons}

reverse_dataCons :: [(DataCon, [(IsBoxed, UrTy a)])] -> [(DataCon, [(IsBoxed, UrTy a)])]
reverse_dataCons list = case list of 
    [] -> []
    (layout_name, fields):xs -> 
        case layout_name of 
            "Layout1" -> let rev_fields = P.reverse fields
                            in [(layout_name, rev_fields)] ++ (reverse_dataCons xs)
            _        -> [(layout_name, fields)] ++ (reverse_dataCons xs)