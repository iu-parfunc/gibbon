

module Packed.FirstOrder.Passes.Fusion2 (fusion2) where

import Control.Exception
import Data.Loc
import qualified Data.Map as M
import qualified Data.List as L
import Data.Symbol
import Packed.FirstOrder.Common
import Packed.FirstOrder.L1.Syntax as L1




toExp1:: L Exp1-> Exp1 
toExp1  (L  _ exp) = exp


data DefTableEntry = DefTableEntry{ def::Exp1, fun_uses :: [(Exp1,Int)], all_use_count:: Int  }

-- this def table is inteneded to map variable that corresponds to results of function calls to their defs and fun_uses 
type DefTable = M.Map Symbol DefTableEntry  
type PotentialPair = (Symbol, Symbol)


-- rewrite this using foldl or foldr/
buildDefTable_args:: Exp1 -> [L Exp1] -> DefTable -> Int-> DefTable 
buildDefTable_args _ [] table _ = table
buildDefTable_args callExp (h:tail)  table index=
       case (toExp1 h ) of -- inspecting the head argument if its a reference ..
          (VarE (Var sym) ) -> buildDefTable_args callExp tail (M.update f sym table) (index+1)
                               where f (DefTableEntry  def fun_uses c) = Just ( DefTableEntry def ((callExp,index):fun_uses) (c+1)) 
          _                -> buildDefTable_args callExp tail  table (index+1)


buildDefTable:: Exp1 -> DefTable ->DefTable

buildDefTable (VarE (Var sym)) table = 
  (M.update f sym table) 
  where f (DefTableEntry  def fun_uses c) =Just ( DefTableEntry def fun_uses (c+1))          


buildDefTable callExp@(AppE _ _ (L _  (MkProdE argsList))) table =  
  buildDefTable_args  callExp argsList table 0

buildDefTable (PrimAppE _ ls) table=
   foldl f table ls
     where f tbl exp = buildDefTable (toExp1 exp) tbl

buildDefTable (LetE ((Var sym),_,_,bind) body) table  = 
  let table' = buildDefTable (toExp1 bind) table 
  in let table'' = case (toExp1 bind) of
                     (AppE _ _ _)   ->   M.insert sym (DefTableEntry {def=(toExp1 bind), fun_uses=[], all_use_count = 0} ) table'
                     _              -> table'
      
      in buildDefTable (toExp1 body) table'' 
     

buildDefTable (IfE cond thenBody elseBody) table =
    let table' = buildDefTable (toExp1 cond) table 
    in let table'' = buildDefTable ( toExp1 thenBody) table'
    in   buildDefTable (toExp1 elseBody) table''


buildDefTable (CaseE e ls) table = 
  let table' = buildDefTable ( toExp1 e) table 
  in foldl f table' ls
   where f tbl (_, _, exp ) = buildDefTable (toExp1 exp) tbl

buildDefTable (DataConE _ _ ls) table = 
   foldl f table ls
     where f tbl exp = buildDefTable (toExp1 exp) tbl

buildDefTable (TimeIt exp _ _) table = 
  buildDefTable (toExp1 exp) table

buildDefTable _ _ =  M.empty 


type PotentialsList = [DefTableEntry] 
findPotentials:: DefTable -> PotentialsList
findPotentials table = M.foldrWithKey f ([]) table 
 where f sym entry@(DefTableEntry _ fun_uses use_count ) ls = if (use_count==1) then entry:ls  else  ls
                 

fusion2:: L1.Prog -> SyM L1.Prog
fusion2 (L1.Prog defs funs main) = do

      let defTable  = case main of 
                       Nothing -> M.empty
                       Just (L _ main') ->buildDefTable main' M.empty
      let potentials =findPotentials defTable 
      
      return $ L1.Prog defs funs main
    



