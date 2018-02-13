

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}

module Packed.FirstOrder.Passes.Fusion2 (fusion2) where
import Control.Exception
import Data.Loc
import qualified Data.Map as M
import qualified Data.List as L
import qualified  Data.Set as S
import qualified Data.Vector as V
import Data.Symbol
import Packed.FirstOrder.Common
import Packed.FirstOrder.L1.Syntax as L1
import Debug.Trace
import Control.DeepSeq
import GHC.Generics (Generic, Generic1)
import Packed.FirstOrder.Common
import Data.Tuple.All
import Data.Vector as V


-- Helper Functions
debug = flip trace
getExp:: L Exp1-> Exp1 
getExp  (L  _ exp) = exp


-- Types
data DefTableEntry = DefTableEntry {def::Exp1, fun_uses :: [(Exp1,Int)], all_use_count:: Int  } deriving (Show , Generic, NFData)
type DefTable = M.Map Symbol DefTableEntry 
type PotentialPair = (Symbol, Symbol)
type PotentialsList = [DefTableEntry] 


--Rewrite this using a case .. will look nicer i think!

buildDefTable:: Exp1 -> DefTable ->DefTable

buildDefTable ex table = 
  case ex of
    VarE (Var sym) ->  M.update f sym table 
                       where f (DefTableEntry  def fun_uses c) = Just ( DefTableEntry def fun_uses (c+1))          
 
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

    PrimAppE _ ls ->  L.foldl f table ls
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
                       in L.foldl f table' ls
                       where f tbl (_, _, exp ) = buildDefTable (getExp exp) tbl
    
    DataConE _ _ ls -> L.foldl f table ls where f tbl exp = buildDefTable (getExp exp) tbl

    TimeIt exp _ _  -> buildDefTable (getExp exp) table

    _               -> table --`debug` ("Defualt" L.++ show a) 



findPotentials:: DefTable -> PotentialsList
findPotentials table =
  M.foldrWithKey f ([]) table 
  where f sym entry@(DefTableEntry _ fun_uses use_count ) ls = if ((L.length fun_uses)==1) then entry:ls  else  ls
                 

inline :: FunDef Ty1 (L Exp1)-> FunDef Ty1 (L Exp1) -> Int  ->  FunDef Ty1 (L Exp1)
inline inlined_fun outer_fun arg_pos  =  
  -- right now i am only handling when the outer function have one argument which is the tree prodcued by the inlined function 
  -- in the future i shoul allow multiple arguments ...
  -- why dont we always make the type prod for simplification !

  -- also right now we assume that the inlined fuction takes one argument which is the input tree
  let old_exp  = l $ case ( snd (funArg outer_fun)) of
                    ProdTy _      ->  error "not handled yet" --ProjE arg_pos (l (VarE (fst (funArg outer_fun) )) )
                    _             ->  VarE (fst (funArg outer_fun)) 


  in let newFunArg =  let argType_outer= snd  (funArg outer_fun) 
                          argVar_outer = fst  (funArg outer_fun)   
                          argType_inlined= snd  (funArg inlined_fun) 
                          argVar_inlined = fst  (funArg inlined_fun)   

                      in case  argType_outer  of 
                              ProdTy ls -> error "not supported yet" -- (argVar, ProdTy (take arg_pos ls  L.++ drop (1 + arg_pos) ls) )
                              _         -> case  argType_inlined  of 
                                                 ProdTy ls -> error "not supported yet" -- (argVar, ProdTy (take arg_pos ls  L.++ drop (1 + arg_pos) ls) )
                                                 _         ->  funArg inlined_fun

  in outer_fun { funArg  = newFunArg ,
                 funBody = substE old_exp (funBody inlined_fun) (funBody outer_fun) }


-- **** THERE IS AN ASSUMPTION THAT DataConstE takes only list of variable references ( no nested pattern matching) 
simplifyCases :: FunDef Ty1 ( L Exp1) -> FunDef Ty1 (L Exp1)
simplifyCases function = 
  function {funBody = rec ( funBody function) }
  where rec ( L l ex)  = case  ex of
                 

                  CaseE e1@(L l (CaseE e2 ls2)) ls1          -> -- TODO : USE MAP INSTEAD OF FOLD HERE !!
                               rec $ L l $ CaseE e2 (L.map f ls2)
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

                  CaseE e1 ls1             ->  L l $ CaseE e1 (L.map f ls1) where f item = (upd3 (rec (sel3 item)) item)   
                  LetE (v,loc,t,rhs) bod   ->  L l $ LetE (v,loc,t, (rec rhs)) (rec bod)  
                  AppE v loc e             ->  L l $ AppE v loc $ rec e
                  IfE e1 e2 e3             ->  L l $ IfE (rec e1) ( rec e2) ( rec e3)
                  TimeIt e d b             ->  L l $ TimeIt ( rec e) d b 

                  _            -> L l ex


-- this one as well assumes that one arg functions (outer and inner )
rewriteSeqCalls:: (Var, Var, Int,Var )-> DefTable ->FunDef Ty1 ( L Exp1)  ->FunDef Ty1 ( L Exp1) 
rewriteSeqCalls rule@(outerName, innerName, argPos, newName)  defTable fun = 
 fun{ funBody=rec (funBody fun)}where
       rec (L l ex ) = case ex of 
                          AppE v loc argList -> 
                              let ignore = L l $ AppE v loc (rec argList) 
                                 in if(v ==  outerName)  then 
                                      case (getExp argList ) of 
                                          VarE (Var sym) ->  if( innerName == getDefiningFuntion sym)  
                                                      then
                                                             L l  $AppE  newName loc (getArgs sym) `debug` ("here2")
                                                                                        
                                                      else
                                                         ignore`debug` ("here1")   -- the outer has only one argument which is the tree itself so 
                                          
                                           where getDefiningFuntion x = case (M.lookup x defTable) of
                                                                                  Nothing    -> error  "error in rewriteSeqCalls"
                                                                                  Just entry -> case (def  entry) of 
                                                                                                AppE v _ _   -> v
                                                                                                _            -> error  ("ops" L.++ (show(def  entry)))
                                                 getArgs x  = case (M.lookup x defTable) of
                                                                                               Nothing    -> error  "error in rewriteSeqCalls"
                                                                                               Just entry -> case (def  entry) of 
                                                                                                                AppE _ _ args   -> args
                                                                                                                _               -> error  ("ops" L.++ (show(def  entry)))

                                                     
                                    else
                                                        ignore`debug` ("here2")   

                          LetE (v,loc,t,rhs) bod   ->  L l $ LetE (v,loc,t, (rec rhs)) (rec bod)  
                          IfE e1 e2 e3             ->  L l $ IfE (rec e1) ( rec e2) ( rec e3)
                          CaseE e1 ls1             ->  L l $ CaseE e1 (L.map f ls1) where f (dataCon,x,exp) = (dataCon, x, (rec exp))  

                          otherwise -> L l ex 

removeUnusedDefs :: FunDef Ty1 ( L Exp1) -> FunDef Ty1 ( L Exp1) 
removeUnusedDefs f =
 f{funBody= rec (funBody f)} where
     rec (L l ex) = 
      let defTable = buildDefTable (getExp (funBody f )) M.empty 
      in case ex of 
                    LetE (Var s,loc,t,rhs) bod   -> case (M.lookup s defTable ) of
                                                      Nothing ->L l $ LetE (Var s,loc,t, (rec rhs))  (rec bod)  --error  ("error in removeUnusedDefs" L.++ show s)
                                                      Just ( DefTableEntry _ _ 0) -> (rec bod) 
                                                      Just _  -> L l $ LetE (Var s,loc,t, (rec rhs))  (rec bod) 
                    IfE e1 e2 e3             ->  L l $ IfE (rec e1) ( rec e2) ( rec e3)
                    CaseE e1 ls1             ->  L l $ CaseE e1 (L.map f ls1) where f (dataCon,x,exp) = (dataCon, x, (rec exp))  
                    AppE v loc e             ->  L l $ AppE v loc $ rec e

                    otherwise -> L l ex 

mergeListOfFunctions :: DDefs Ty1 -> [FunDef Ty1 ( L Exp1)] ->  FunDef Ty1 ( L Exp1) 
mergeListOfFunctions  ddefs ls = do
  let newName = L.foldl  appendName "" ls where appendName tmp function = (tmp  L.++ (fromVar (funName function)))  
  -- we want to prepare the body of each function seperatly now
  let lsVector    = V.fromList ls 
  let retTypes    = V.map (\f ->(funRetTy f)) lsVector
  let newRetType  = ProdTy (V.toList retTypes)  

  let newArgVar = toVar "input"
  let traversedType =snd (funArg( L.head ls))
  let newArgs = (newArgVar,traversedType)
  let step1 = L.map getBody ls where getBody fun = substE  (l (VarE (fst (funArg( L.head ls)))) ) (l (VarE newArgVar)) (funBody fun)  

  let step2 = L.map mapAndSplit step1  
                      where mapAndSplit (L _(CaseE e ls)) = L.foldr f M.empty ls 
                              where f (dataCons, varls, exp) mp = M.insert dataCons (subsVars exp) mp
                                      where subsVars ex = V.ifoldr subsVar ex (V.fromList varls) 
  
                                             where subsVar index v ex   = substE  (l (VarE (fst v) )) (l (VarE (toVar (dataCons L.++ (show index))))) ex  
 

  let inputDef = lookupDDef ddefs ( case  traversedType of (PackedTy typeStr _) -> (toVar typeStr))
  let dataConsList = dataCons inputDef
  let createOutVar index =  (toVar ("f" L.++(show index)L.++"out" )) 

  let outPart = l ( MkProdE  genL  )
         where genL = V.toList (  V.imap  (\index _ ->l (VarE ( createOutVar index) ) ) (V.fromList ls))

  let retAllExp = MkProdE (V.toList ( V.imap (\index _ ->l (VarE (createOutVar index))) lsVector ) )
  let reduceStep = L.foldr reduce (CaseE (l (VarE newArgVar))[]) dataConsList where
                              reduce (dataCons, varls) (CaseE ex ls ) = CaseE ex ((consBody):ls)
                                       where consBody = (dataCons, newVarsList, combinedBodies) where 
                                              newVarsList     =  V.toList( V.imap (\index _ -> ( toVar (dataCons L.++ (show index)) ,() ) ) (V.fromList varls) ) where 
                                              combinedBodies  =  V.ifoldr (\index body res  -> l (LetE ((createOutVar index),[],(unsafeIndex retTypes index),body) res )) (l retAllExp) (V.fromList partialList)
                                                                  where partialList =  L.map (\m -> case ( M.lookup dataCons m) of 
                                                                                                        (Just a ) -> a 
                                                                                                        (Nothing) ->error "ooopsa1")  step2 
  --let reduce = L.fold                                                                                             
  FunDef  (toVar ("merged__" L.++ newName) )newArgs newRetType (l reduceStep) 
  --let newArgs = ... 

renameFunction:: FunDef Ty1 ( L Exp1) -> Var -> FunDef Ty1 ( L Exp1) 
renameFunction function newName = 
  function{funName=newName, funBody = rec (funBody function)} where 
    rec (L l ex) = 
      let oldName = funName function in 
            case ex of 
                  AppE name loc e          ->  L l $ AppE (if name==oldName then newName else name) loc (rec e) 
                  PrimAppE x ls            ->  L l $ PrimAppE x (L.map f ls) where f item = (rec item)
                  LetE (v,loc,t,rhs) bod   ->  L l $ LetE (v,loc,t, (rec rhs)) (rec bod)  
                  IfE e1 e2 e3             ->  L l $ IfE (rec e1) ( rec e2) ( rec e3)
                  MkProdE ls               ->  L l $ MkProdE (L.map (\x-> rec x) ls) 
                  ProjE index exp          ->  L l $ ProjE index (rec exp)
                  CaseE e1 ls1             ->  L l $ CaseE e1 (L.map f ls1) where f (dataCon,x,exp) = (dataCon, x, (rec exp))  
                  DataConE loc datacons ls ->  L l $ DataConE loc datacons (L.map (\x-> rec x) ls)
                  TimeIt e d b             ->  L l $ TimeIt ( rec e) d b 
                  otherwise                -> L l ex



buildUseTable::  FunDef Ty1 ( L Exp1) -> M.Map Var [(Var, Exp1)]
buildUseTable ex  = rec (funBody ex) (M.fromList []) where
  rec (L l ex) tb = case ex of 
                  AppE _ _ _               ->  tb 
                  PrimAppE _ _             ->  tb
                  LetE (v,_,_,(L _ callexp@(AppE _ _ (L _ (VarE par )) ) )) body   ->  M.alter f par (rec body tb) where --add a check to make sure that the call exp represent a traversal and is not a simple functions call
                    f Nothing   = Just [(v,callexp)]
                    f (Just ls) = Just ((v,callexp):ls) 
                  LetE (v,_,_,rhs) body    -> let t1 = rec rhs tb 
                                              in rec body t1  

                  IfE e1 e2 e3             ->  let t1 = rec e1 tb 
                                               in let t2 = rec e2 t1
                                               in rec e3 t2 

                  MkProdE ls               ->  tb 
                  ProjE index exp          ->  tb
                  CaseE e1 ls1             ->  let t1 = rec e1 tb in 
                                               L.foldl f t1 ls1 where f table (_ ,_ ,exp) = rec exp tb 
                  DataConE loc datacons ls ->  L.foldl f tb ls where f table exp = rec exp tb 
                  TimeIt exp _ _           ->  rec exp tb  
                  otherwise                -> tb


--mergeFunctionDefinitons:: 
fusion2:: L1.Prog -> SyM L1.Prog 
fusion2 (L1.Prog defs funs main) = do
  let defTable  = case main of
                    Nothing          ->  M.empty `debug` ("empty main") 
                    Just (L _ main') -> (buildDefTable main' M.empty) `debug`("building def table for the body of the main") 

  let potentials = findPotentials defTable

  let firstPot =  L.head potentials
  let getSymbol exp = case (exp) of 
                       AppE v _ _ -> v
      
  let inner_fun =  funs M.! (getSymbol (def firstPot)) 
  let outer_fun =  funs M.! (getSymbol ( fst ( L.head (fun_uses firstPot)) ) )

  let old_name = funName outer_fun 
  let arg_pos = snd ( L.head (fun_uses firstPot) )
 
  inline_name <-  gensym_tag old_name "inline" 
  let inlined_fun =  (inline inner_fun outer_fun arg_pos) {funName = inline_name}
  
  case_simple_name <-  gensym_tag old_name "simpleCase" 
  let simple_case = renameFunction (simplifyCases inlined_fun) case_simple_name
  
  rewriteName <- gensym_tag old_name "rewrite1"
  let rewrite1 = let rule = ((funName outer_fun), (funName inner_fun), arg_pos, rewriteName) 
                 in  renameFunction (rewriteSeqCalls rule (buildDefTable (getExp( funBody simple_case)) M.empty) simple_case ) rewriteName
 
  clean1Name    <- gensym_tag old_name "clean1"
  let clean1 =  renameFunction (removeUnusedDefs  rewrite1) clean1Name 

  -- repeat.............................
  let defTable2 = (buildDefTable (getExp (funBody clean1)) M.empty) `debug`("building def table for the body of clean1") 
 
  let potentials2 = findPotentials defTable2

  let firstPot2 =  L.head potentials2
  let getSymbol exp = case (exp) of 
                       AppE v _ _ -> v

  let inner_fun2 =  funs M.! (getSymbol (def firstPot2)) 
  let outer_fun2 =  funs M.! (getSymbol ( fst ( L.head (fun_uses firstPot2)) ) )
   

  let old_name2 = funName outer_fun2 
  let arg_pos2 = snd ( L.head (fun_uses firstPot2) )
 
  inline_name2 <-  gensym_tag old_name2 "inline" 
  let inlined_fun2 =  (inline inner_fun2 outer_fun2 arg_pos2) {funName = inline_name2}
  

  case_simple_name2 <-  gensym_tag old_name2 "simpleCase" 
  let simple_case2 = renameFunction (simplifyCases inlined_fun2) case_simple_name2
  
  rewriteName2 <- gensym_tag old_name2 "rewrite1"
  let rewrite1_2 = let rule = ((funName outer_fun2), (funName inner_fun2), arg_pos2, rewriteName2) 
                 in  renameFunction (rewriteSeqCalls rule (buildDefTable (getExp( funBody inlined_fun2)) M.empty) simple_case2 ) rewriteName2
 
  clean1Name2    <- gensym_tag old_name2 "clean1"
  let clean1_2 =  renameFunction (removeUnusedDefs  rewrite1_2) clean1Name2 

  rewriteValName <- gensym_tag old_name "rewriteVal"
  let rewriteVal = let rule = ((funName outer_fun2), (funName inner_fun2), arg_pos, clean1Name2) 
                 in  renameFunction (removeUnusedDefs (rewriteSeqCalls rule (buildDefTable (getExp( funBody clean1)) M.empty) clean1 )) rewriteValName
 


 -- let update1 =  M.insert  inline_name inlined_fun funs
 -- let update2 =  M.insert  case_simple_name simple_case update1
 -- let update3 =  M.insert  rewriteName rewrite1 update2
  --let update4 =  M.insert   clean1Name clean1 update3
  let update4         = M.insert   clean1Name clean1 funs
  let update5         = M.insert  clean1Name2 clean1_2 update4
  let update6         = M.insert  rewriteValName rewriteVal update5
  let useTable        = buildUseTable rewriteVal
  ----- The New Suff ... need to be isolated in fuctions to compose them next 
  let useTableFiltered = M.filter f useTable where f a = if (L.length a) =2 then True else False  

  --move this to a function
  let pairsToMerge    = M.foldl f S.empty useTableFiltered where
       f set ls = S.insert (L.foldl f2 S.empty ls) set 
          where f2 s (_,(AppE fName _ _ )) = S.insert fName s

  -- move this as well to a funtion 
  let functionsToMerge = L.map toList (S.toList pairsToMerge) where
       toList item = L.foldl f2 [] item where
          f2 ls fname = case ( M.lookup fname update6 ) of 
                          Nothing  ->error "not expected" 
                          Just fundef   -> fundef:ls
  -- create a list of merged function 
  let mergedFunctions = L.map (mergeListOfFunctions defs) functionsToMerge
  let update7 = L.foldr (\funDef mp -> M.insert (funName funDef) funDef mp ) update6 mergedFunctions
  
  -- for now assume that the merged function are of length 2 ( 2 functions are merged at a time)
  let rewrtieRules = L.foldr mergedFunctions () 

  --rewrite in each function with in update 7 
  -- Now we want to look with in the function for multiple traversal that traverse the input tree
  --let update5 =  M.insert  inline_name2 inlined_fun2 update4


  return $ L1.Prog defs  update7 main `debug` ("deftable is : " L.++ (show functionsToMerge)  L.++ "\n\n potentials are :"  L.++ (show potentials2)  L.++"\n\n the new function is " L.++ (show simple_case) )
    