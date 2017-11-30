

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}

module Packed.FirstOrder.Passes.Fusion2 (fusion2) where
import Control.Exception
import Data.Loc
import qualified Data.Map as M
import qualified Data.List as L
import Data.Symbol
import Packed.FirstOrder.Common
import Packed.FirstOrder.L1.Syntax as L1
import Debug.Trace
import Control.DeepSeq
import GHC.Generics (Generic, Generic1)
import Packed.FirstOrder.Common
import Data.Tuple.All

-- Helper Functions
debug = flip trace
getExp:: L Exp1-> Exp1 
getExp  (L  _ exp) = exp


-- Types
data DefTableEntry = DefTableEntry{ def::Exp1, fun_uses :: [(Exp1,Int)], all_use_count:: Int  } deriving (Show , Generic, NFData)
type DefTable = M.Map Symbol DefTableEntry 
type PotentialPair = (Symbol, Symbol)
type PotentialsList = [DefTableEntry] 


--Rewrite this using a case .. will look nicer i think!

buildDefTable:: Exp1 -> DefTable ->DefTable

buildDefTable ex table = 
  case ex of
    VarE (Var sym) ->  M.update f sym table 
                       where f (DefTableEntry  def fun_uses c) =Just ( DefTableEntry def fun_uses (c+1))          
 
    callExp@(AppE _ _ (L _  (MkProdE argsList))) ->
                       buildDefTable_args  callExp argsList table 0
                       where  buildDefTable_args _ [] table _ = table
                              buildDefTable_args callExp (h:tail)  table index=
                               case getExp h  of 
                                  VarE (Var sym)    -> buildDefTable_args callExp tail (M.update f sym table) (index+1)
                                               where f (DefTableEntry  def fun_uses c) = Just ( DefTableEntry def ((callExp,index):fun_uses) (c+1)) 
                                  _                 -> buildDefTable_args callExp tail  table (index+1)

    callExp@(AppE _ _ (L _  (VarE (Var sym ))))  ->
                       M.update f sym table 
                       where f (DefTableEntry  def fun_uses c) = Just ( DefTableEntry def ((callExp,-1):fun_uses) (c+1)) -- -1 means there is no ProdE ( only one argument)

    PrimAppE _ ls ->  foldl f table ls
                      where f tbl exp = buildDefTable (getExp exp) tbl

    LetE ((Var sym),_,_,bind) body ->
                      let table' =  (buildDefTable (getExp bind) table) 
                      in let table'' =  case (getExp bind) of
                                          AppE _ _ _     ->  M.insert sym (DefTableEntry {def=(getExp bind), fun_uses=[], all_use_count = 0} ) table'
                                          _              ->  table' 
          
                      in buildDefTable (getExp body) table''
     

    IfE cond thenBody elseBody      ->
                      let table' = buildDefTable (getExp cond) table 
                      in let table'' = buildDefTable ( getExp thenBody) table'
                      in   buildDefTable (getExp elseBody) table''


    CaseE e ls      -> let table' = buildDefTable ( getExp e) table 
                       in foldl f table' ls
                       where f tbl (_, _, exp ) = buildDefTable (getExp exp) tbl
    
    DataConE _ _ ls -> foldl f table ls where f tbl exp = buildDefTable (getExp exp) tbl

    TimeIt exp _ _  -> buildDefTable (getExp exp) table

    _               -> table --`debug` ("Defualt"++ show a) 



findPotentials:: DefTable -> PotentialsList
findPotentials table =
  M.foldrWithKey f ([]) table 
  where f sym entry@(DefTableEntry _ fun_uses use_count ) ls = if (use_count==1) then entry:ls  else  ls
                 

inline :: FunDef Ty1 (L Exp1)-> FunDef Ty1 (L Exp1) -> Int  ->  FunDef Ty1 (L Exp1)
inline inner_fun outer_fun arg_pos  =  
  let old_exp  = l $ case ( snd (funArg outer_fun)) of
                    ProdTy _    ->  ProjE arg_pos (l (VarE (fst (funArg outer_fun) )) )
                    _             ->  VarE (fst (funArg outer_fun)) 

  in let newFunArg =  let argType= snd (funArg outer_fun) 
                          argVar = fst ( funArg outer_fun)   
                      in case  argType  of 
                              ProdTy ls ->  (argVar, ProdTy (take arg_pos ls ++ drop (1 + arg_pos) ls) )
                              _         ->  (argVar, ProdTy [])  

  in outer_fun { funArg  = newFunArg ,
                 funBody = substE old_exp (funBody inner_fun) (funBody outer_fun) }


-- **** THERE IS AN ASSUMPTION THAT DataConstE takes only list of variable references ( no nested pattern matching) 
simplifyCases :: FunDef Ty1 ( L Exp1) -> FunDef Ty1 (L Exp1)
simplifyCases function = 
  function {funBody = rec ( funBody function) }
  where rec ( L l ex)  = case  ex of
                 

                  CaseE e1@(L l (CaseE e2 ls2)) ls1          -> -- TODO : USE MAP INSTEAD OF FOLD HERE !!
                               rec $ L l $ CaseE e2 (map f ls2)
                               where f oldItem = (upd3 newMember oldItem)  where newMember = L l (CaseE (sel3 oldItem) ls1)
                  
                  CaseE e1@(L l (DataConE loc k ls)) ls1     -> 
                                let newBody = L.find predicate ls1 where predicate item = if(sel1 item == k) then True else False
                                in case newBody of 
                                   Nothing -> error "unmatched construcotor!"
                                   Just (k, vars, exp)  -> rec $case_subst ls vars exp
                                   where case_subst (x1:l1) (x2:l2) exp =
                                            subst (fst x2) ( x1) (case_subst l1 l2 exp)
                                         case_subst [] [] exp = exp
                  
                                
                  CaseE e1@(L l' (LetE bind body )  ) ls1    ->
                                 L l' $ LetE bind (rec $ L l $ CaseE body (ls1) )

                  CaseE e1 ls1             ->  L l $ CaseE e1 (map f ls1) where f item = (upd3 (rec (sel3 item)) item)   
                  LetE (v,loc,t,rhs) bod   ->  L l $ LetE (v,loc,t, (rec rhs)) (rec bod)  
                  AppE v loc e             ->  L l $ AppE v loc $ rec e
                  IfE e1 e2 e3             ->  L l $ IfE (rec e1) ( rec e2) ( rec e3)
                  TimeIt e d b             ->  L l $ TimeIt ( rec e) d b 

                  _            -> L l ex
 
fusion2:: L1.Prog -> SyM L1.Prog
fusion2 (L1.Prog defs funs main) = do
  let defTable  = case main of
                    Nothing ->  M.empty `debug` ("empty main") 
                    Just (L _ main') -> (buildDefTable main' M.empty) `debug`("building def table for the body of the main") 

  let potentials = findPotentials defTable
  let firstPot = head potentials
  let getSymbol exp = case (exp) of 
                       AppE v _ _ -> v
      
  let inner_fun =  funs M.! (getSymbol (def firstPot)) 
  let outer_fun =  funs M.! (getSymbol ( fst (head (fun_uses firstPot)) ) )
  let old_name = funName outer_fun 
  let arg_pos = snd (head (fun_uses firstPot) )
  inline_name <-  gensym_tag old_name "inline" 
  let inlined_fun = (inline inner_fun outer_fun arg_pos) {funName = inline_name} 
  case_simple_name <-  gensym_tag old_name "simpleCase" 
  let simple_case = (simplifyCases inlined_fun){funName = case_simple_name}
  return $ L1.Prog defs (M.insert (funName simple_case) simple_case (M.insert (funName inlined_fun) inlined_fun funs))  main  `debug` ("deftable is : "++ (show defTable) ++ "\n\n potentials are :" ++ (show potentials) ++"\n\n the new function is "++ (show simple_case) )
    