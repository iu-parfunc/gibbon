{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Insert end witnesses in an L2 program by changing function types,
-- and updating expressions to pass (second-class) end locations around
-- via RetE in tail position and an extended binding form in LetE.
-- Assumes that expressions are flattened and in ANF, and that location
-- symbols are all unique! Failure to meet these assumptions will cause
-- this pass to fail or possibly produce wrong output.
--
--- Steps:
---
--- 1. For each function type, inspect its input parameter type and traversal
---    effect to determine which packed arguments are completely traversed,
---    and update the type to indicate that the EndOf witness for each
---    of these traversed arguments is returned.
---
--- 2. For each function body, walk through the let spine, updating bindings
---    to include locations returned as end witnesses, and add coersions from
---    EndOf witnesses to expected locations. Upon reaching tail position,
---    emit a RetE form and compute what the proper EndOf locations are.
---
--- 3. For the main body, do the same thing minus inserting the RetE in tail
---    position. 

module Packed.FirstOrder.Passes.RouteEnds2
    ( routeEnds ) where

import Packed.FirstOrder.Common
import Packed.FirstOrder.L2.Syntax as L2
import qualified Packed.FirstOrder.L1.Syntax as L1
import Data.Set as S
import Data.Map as M
import Data.List as L
import Control.Monad 


-- | Data structure that accumulates what we know about the relationship
-- between locations and EndOf witnesses.
--
-- Performing a lookup (to find the end of a given location) will first check
-- if there exists a mapping in the endOf map for that location, then will check
-- if there exists a mapping in the equivTo map, and if so will recur to find
-- the end of that location.
--
-- This is used for when we perform pattern matching. The end of some binary tree
-- (for example) is the same as the end of its second node, so we want to record
-- that knowledge as we traverse the AST.
data EndOfRel = EndOfRel
    {
      endOf :: M.Map LocVar LocVar -- ^ Map a location to it's EndOf witness
    , equivTo :: M.Map LocVar LocVar -- ^ Map of a location to a known equivalent location
    }

-- | Create an empty EndOfRel
emptyRel :: EndOfRel
emptyRel = EndOfRel M.empty M.empty

-- | Assert that one location's end is equivalent to another's end.
-- Order is important here: we expect to look up the end of the first
-- location argument, not the second. 
mkEqual :: LocVar -> LocVar -> EndOfRel -> EndOfRel
mkEqual l1 l2 EndOfRel{endOf,equivTo} =
    EndOfRel{endOf=endOf,equivTo=M.insert l1 l2 equivTo}

-- | Assert that we have found an EndOf relation between two locations.
mkEnd :: LocVar -> LocVar -> EndOfRel -> EndOfRel
mkEnd l1 l2 EndOfRel{endOf,equivTo} =
    EndOfRel{endOf=M.insert l1 l2 endOf,equivTo=equivTo}

-- | Look up the end of a location.
findEnd :: LocVar -> EndOfRel -> LocVar
findEnd l EndOfRel{endOf,equivTo} =
    case M.lookup l endOf of -- Can we immediately look up the end of l?
      Nothing -> case M.lookup l equivTo of -- Is there an equivalent location to use?
                   Nothing -> error $ "Failed finding the end of " ++ (show l)
                   Just leq -> findEnd leq EndOfRel{endOf,equivTo}
      Just lend -> lend


-- | Process an L2 Prog and thread through explicit end-witnesses.
-- Requires Gensym and runs in SyM. Assumes the Prog has been flattened.
routeEnds :: Prog -> SyM Prog
routeEnds Prog{ddefs,fundefs,mainExp} = do

  -- Handle functions in two steps (to account for mutual recursion):
  --
  -- First, compute the new types, and build a new fundefs structure:
  fds' <- mapM fdty $ M.elems fundefs
  let fundefs' = M.fromList $ L.map (\f -> (funname f,f)) fds'
  -- Then process the actual function bodies using the new fundefs structure:
  fds'' <- mapM (fd fundefs') fds'
  let fundefs'' = M.fromList $ L.map (\f -> (funname f,f)) fds''


  -- Handle the main expression (if it exists):
  mainExp' <- case mainExp of
                Nothing -> return Nothing
                Just (e,t) -> do e' <- exp fundefs'' [] emptyRel M.empty M.empty e
                                 return $ Just (e',t)

  -- Return the updated Prog
  return $ Prog ddefs fundefs'' mainExp'


  where
    -- Helper functions:

    -- | Process function types (but don't handle bodies)
    fdty :: L2.FunDef -> SyM L2.FunDef 
    fdty L2.FunDef{funname,funty,funarg,funbod} =
        do let (ArrowTy locin tyin eff tyout _locout) = funty
               handleLoc (LRM l r m) ls = if S.member (Traverse l) eff then (LRM l r m):ls else ls
               locout' = L.map EndOf $ L.foldr handleLoc [] locin
           return L2.FunDef{funname,funty=(ArrowTy locin tyin eff tyout locout'),funarg,funbod}
                                          

    -- | Process function bodies
    fd :: NewFuns -> L2.FunDef -> SyM L2.FunDef
    fd fns L2.FunDef{funname,funty,funarg,funbod} =
        do let (ArrowTy locin tyin eff _tyout _locout) = funty
               handleLoc (LRM l _r _m) ls = if S.member (Traverse l) eff then l:ls else ls
               retlocs = L.foldr handleLoc [] locin
               lenv = case tyin of
                        PackedTy _n l -> M.insert funarg l $ M.empty
                        ProdTy _tys -> error "Multiple function args not handled yet in RouteEnds2"
                        _ -> M.empty
           funbod' <- exp fns retlocs emptyRel lenv M.empty funbod
           return L2.FunDef{funname,funty,funarg,funbod=funbod'}


    -- | Process expressions.
    -- Takes the following arguments:
    -- 1. a function environment
    -- 2. a list of locations we need to return the ends of
    -- 3. an end-of relation
    -- 4. a map of var to location
    -- 5. a map from location to location after it
    -- 6. the expression to process
    exp :: NewFuns -> [LocVar] -> EndOfRel -> M.Map Var LocVar -> M.Map LocVar LocVar -> Exp2 -> SyM Exp2
    exp fns retlocs eor lenv afterenv e =

        case e of

          -- Variable case, *should* be the base case assuming our expression was
          -- properly put in ANF.
          -- We generate our RetE form here. By this point we should know the ends
          -- of each of the locactions in relocs.
          VarE v -> mkRet retlocs $ VarE v

          -- This is the most interesting case: a let bound function application.
          -- We need to update the let binding's extra location binding list with
          -- the end witnesses returned from the function.
          LetE (v,_ls,ty,(AppE f lsin e1)) e2 -> do
                                          
                 let fty = funtype f
                     rets = S.fromList $ locRets fty
                     -- The travlist is a list of pair (location, bool) where the bool is
                     -- if the location was traversed, and the location is from the
                     -- AppE call.
                     travlist = zip lsin $ L.map (\l -> S.member (EndOf l) rets)  (locVars fty)
                     lenv' = case ty of
                               PackedTy _n l -> M.insert v l lenv
                               _ -> lenv

                 -- For each traversed location, gensym a new variable for its end,
                 -- and generate a list of (location, endof location) pairs. 
                 let handleTravList lst (_l,False) = return lst
                     handleTravList lst (l,True) = gensym "endof" >>= \l' -> return $ (l,l'):lst

                 -- Walk through our pairs of (location, endof location) and update the
                 -- endof relation. 
                 let mkEor (l1,l2) eor = mkEnd l1 l2 eor

                 -- We may need to emit some additional let bindings if we've reached
                 -- an end witness that is equivalent to the after location of something.
                 let wrapBody e ((l1,l2):ls) = case M.lookup l1 afterenv of
                                                 Nothing -> wrapBody e ls
                                                 Just la -> wrapBody (Ext (LetLocE la (FromEndLE l2) e)) ls
                     wrapBody e [] = e

                 newls <- foldM handleTravList [] travlist
                 let eor' = L.foldr mkEor eor newls
                 let outlocs = L.map snd newls
                 e2' <- exp fns retlocs eor' lenv' afterenv e2
                 return $ LetE (v,outlocs,ty,AppE f lsin e1) (wrapBody e2' newls)

          CaseE (VarE x) brs -> do
                 -- We will need to gensym while processing the case clauses, so
                 -- it has to be in the SyM monad
                 brs' <- forM brs $ \(dc, vls, e) -> do
                                          let need = snd $ last vls
                                              argtys = lookupDataCon ddefs dc
                                              lx = case M.lookup x lenv of
                                                     Nothing -> error $ "Failed to find " ++ (show x)
                                                     Just l -> l
                                              -- we know lx and need have the same end, since
                                              -- lx is the whole packed thing and need is its
                                              -- last field, so when we look up the end of lx
                                              -- what we really want is the end of need.
                                              eor' = mkEqual lx need eor
                                              f (l1,l2) env = M.insert l1 l2 env
                                              afterenv' = L.foldr f afterenv $ zip (L.map snd vls) (tail $ L.map snd vls)
                                              -- two cases here for handing bound parameters:
                                              -- we have a packed type: 
                                              handleLoc (eor,e) (_,(PackedTy _ _)) = return (eor,e)
                                              -- or we have a non-packed type, and we need to "jump" over it and
                                              -- bind a location to after it
                                              handleLoc (eor,e) (l1,_ty) = do l2 <- gensym "jump"
                                                                              let eor' = mkEnd l1 l2 eor
                                                                                  e' = Ext $ LetLocE l2 (AfterConstantLE 1 l1 l2) e
                                                                              return (eor',e')
                                          
                                          (eor'',e') <- foldM handleLoc (eor',e) $ zip (L.map snd vls) argtys
                                          e'' <- exp fns retlocs eor'' lenv afterenv' e'
                                          return (dc, vls, e'')
                 return $ CaseE (VarE x) brs'






          -------------------------------------------------------------------------------------------------------------------------
          --- That's all the interesting cases. The rest are straightforward:
                        

          -- This shouldn't happen, but as a convenience we can ANF-ify this AppE
          -- by gensyming a new variable, sticking the AppE in a LetE, and recuring.
          -- Question: should this fail instead? I'm not sure.
          AppE v ls e -> do
                 v' <- gensym "tailapp"
                 let ty = arrOut $ funtype v
                     e' = LetE (v',[],ty,AppE v ls e) (VarE v')
                 exp fns retlocs eor lenv afterenv e'
                 
          -- Same as above. This could just fail, instead of trying to repair
          -- the program.
          PrimAppE pr es -> do
                 v' <- gensym "tailprim"
                 let ty = case pr of
                            L1.AddP -> IntTy
                            L1.SubP -> IntTy
                            L1.MulP -> IntTy
                            L1.MkTrue -> BoolTy
                            L1.MkFalse -> BoolTy
                            _ -> error "fixme, PrimAppE in tail"
                     e' = LetE (v',[],ty,PrimAppE pr es) (VarE v')
                 exp fns retlocs eor lenv afterenv e'

          -- Less exciting LetE case, just recur on the body with an updated lenv
          LetE (v,ls,PackedTy n l,e1) e2 -> do
                 e2' <- exp fns retlocs eor (M.insert v l lenv) afterenv e2
                 return $ LetE (v,ls,PackedTy n l,e1) e2'

          -- Most boding LetE case, just recur on body
          LetE (v,ls,ty,e1) e2 -> do
                 e2' <- exp fns retlocs eor lenv afterenv e2
                 return $ LetE (v,ls,ty,e1) e2'

          IfE e1 e2 e3 -> do
                 e2' <- exp fns retlocs eor lenv afterenv e2
                 e3' <- exp fns retlocs eor lenv afterenv e3
                 return $ IfE e1 e2' e3'

          -- Not worth trying to fix the un-flattened program here, since we would need
          -- to know the types of the exps in es to make a let binding.
          MkProdE _es -> internalError $ "Found complex expression in tail: " ++ (show e)

          ProjE _i _e -> internalError $ "Found complex expression in tail: " ++ (show e)

          -- Could fail here, but try to fix the broken program
          DataConE l dc es -> do
                 v' <- gensym "taildc"
                 let ty = PackedTy (getTyOfDataCon ddefs dc) l
                     e' = LetE (v',[],ty,DataConE l dc es) (VarE v')
                 exp fns retlocs eor lenv afterenv e'

          LitE i -> return $ LitE i

          LitSymE v -> return $ LitSymE v

          TimeIt e ty b -> do
                 e' <- exp fns retlocs eor lenv afterenv e
                 return $ TimeIt e' ty b

          Ext (LetRegionE r e) -> do
                 e' <- exp fns retlocs eor lenv afterenv e
                 return $ Ext (LetRegionE r e')

          Ext (LetLocE v (StartOfLE l r) e) -> do
                 e' <- exp fns retlocs eor lenv afterenv e
                 return $ Ext (LetLocE v (StartOfLE l r) e')

          Ext (LetLocE v (AfterConstantLE i l1 l2) e) -> do
                 e' <- exp fns retlocs eor lenv afterenv e
                 return $ Ext (LetLocE v (AfterConstantLE i l1 l2) e')

          Ext (LetLocE v (AfterVariableLE x l1 l2) e) -> do
                 e' <- exp fns retlocs eor lenv afterenv e
                 return $ Ext (LetLocE v (AfterVariableLE x l1 l2) e')

          _ -> internalError $ "RouteEnds: Unsupported expression: " ++ (show e)



        where  mkRet :: [LocVar] -> Exp2 -> SyM Exp2
               mkRet ls (VarE v) = let ends = L.map (\l -> findEnd l eor) ls
                                   in return $ Ext (RetE ends v)
               mkRet _ e = error $ "Expected variable reference in tail call, got " ++ (show e)

               funtype :: Var -> ArrowTy Ty2
               funtype v = case M.lookup v fns of
                             Nothing -> error $ "Function " ++ (show v) ++ " not found"
                             Just fundef -> funty fundef






-------------------------------------------------------------------------------------------------
-- Tests:
-------------------------------------------------------------------------------------------------


tester :: Exp2 -> Prog
tester e =
    let p = L2.withAdd1Prog (Just (e,IntTy))
    in fst $ runSyM 1 $ routeEnds p

test1 :: Exp2
test1 = Ext $ LetRegionE (VarR "r") $ Ext $ LetLocE "ltest" (StartOfLE "l" (VarR "r")) $
        Ext $ LetLocE "ltest1" (AfterConstantLE 1 "ltest" "ltest1") $ 
        LetE ("x", [], PackedTy "Tree" "ltest1", DataConE "ltest1" "Leaf" [LitE 1]) $
        Ext $ LetLocE "ltest2" (AfterVariableLE "x" "ltest1" "ltest2") $
        LetE ("y", [], PackedTy "Tree" "ltest2", DataConE "ltest2" "Leaf" [LitE 2]) $
        LetE ("z", [], PackedTy "Tree" "ltest", DataConE "ltest" "Node" [VarE "x", VarE "y"]) $
        Ext $ LetRegionE (VarR "o") $ Ext $ LetLocE "lo" (StartOfLE "lo" (VarR "o")) $
        AppE "add1" ["l","lo"] (VarE "z")

-- Prog {ddefs = fromList [(Var "Tree",DDef {tyName = Var "Tree", dataCons = [("Leaf",[(False,IntTy)]),("Node",[(False,PackedTy "Tree" (Var "l")),(False,PackedTy "Tree" (Var "l"))])]})], fundefs = fromList [(Var "add1",FunDef {funname = Var "add1", funty = ArrowTy {locVars = [LRM (Var "lin") (VarR (Var "r1")) Input,LRM (Var "lout") (VarR (Var "r1")) Output], arrIn = PackedTy "tree" (Var "lin"), arrEffs = fromList [Traverse (Var "lin")], arrOut = PackedTy "tree" (Var "lout"), locRets = [EndOf (LRM (Var "lin") (VarR (Var "r1")) Input)]}, funarg = Var "tr", funbod = CaseE (VarE (Var "tr")) [("Leaf",[(Var "n",Var "l0")],Ext (LetLocE (Var "jump1") (AfterConstantC 1 (Var "l0") (Var "jump1")) (LetE (Var "v",[],IntTy,PrimAppE AddP [VarE (Var "n"),LitE 1]) (Ext (RetE [Var "jump1"] (Var "v")))))),("Node",[(Var "x",Var "l1"),(Var "y",Var "l2")],Ext (LetLocE (Var "lout1") (AfterConstantC 1 (Var "lout") (Var "lout1")) (LetE (Var "x1",[Var "endof2"],PackedTy "Tree" (Var "lout1"),AppE (Var "add1") [Var "l1",Var "lout1"] (VarE (Var "x"))) (Ext (LetLocE (Var "l2") (FromEndC (Var "endof2")) (Ext (LetLocE (Var "lout2") (AfterVariableC (Var "x1") (Var "lout1") (Var "lout2")) (LetE (Var "y1",[Var "endof3"],PackedTy "Tree" (Var "lout2"),AppE (Var "add1") [Var "l2",Var "lout2"] (VarE (Var "y"))) (LetE (Var "z",[],PackedTy "Tree" (Var "lout"),DataConE (Var "lout") "Node" [VarE (Var "x1"),VarE (Var "y1")]) (Ext (RetE [Var "endof3"] (Var "z"))))))))))))]})], mainExp = Just (Ext (LetRegionE (VarR (Var "r")) (Ext (LetLocE (Var "ltest") (StartOfC (Var "l") (VarR (Var "r"))) (Ext (LetLocE (Var "ltest1") (AfterConstantC 1 (Var "ltest") (Var "ltest1")) (LetE (Var "x",[],PackedTy "Tree" (Var "ltest1"),DataConE (Var "ltest1") "Leaf" [LitE 1]) (Ext (LetLocE (Var "ltest2") (AfterVariableC (Var "x") (Var "ltest1") (Var "ltest2")) (LetE (Var "y",[],PackedTy "Tree" (Var "ltest2"),DataConE (Var "ltest2") "Leaf" [LitE 2]) (LetE (Var "z",[],PackedTy "Tree" (Var "ltest"),DataConE (Var "ltest") "Node" [VarE (Var "x"),VarE (Var "y")]) (Ext (LetRegionE (VarR (Var "o")) (Ext (LetLocE (Var "lo") (StartOfC (Var "lo") (VarR (Var "o"))) (LetE (Var "tailapp4",[Var "endof5"],PackedTy "tree" (Var "lout"),AppE (Var "add1") [Var "l",Var "lo"] (VarE (Var "z"))) (Ext (RetE [] (Var "tailapp4")))))))))))))))))),IntTy)}

-- FunDef {funname = Var "add1",
-- funty = ArrowTy {locVars = [LRM (Var "lin") (VarR (Var "r1")) Input,LRM (Var "lout") (VarR (Var "r1")) Output],
--                  arrIn = PackedTy "tree" (Var "lin"),
--                  arrEffs = fromList [Traverse (Var "lin")],
--                  arrOut = PackedTy "tree" (Var "lout"),
--                  locRets = [EndOf (LRM (Var "lin") (VarR (Var "r1")) Input)]},
-- funarg = Var "tr",
-- funbod = CaseE (VarE (Var "tr"))
--          [("Leaf",[(Var "n",Var "l0")],
--           Ext (LetLocE (Var "jump1") (AfterConstantC 1 (Var "l0") (Var "jump1"))
--           (LetE (Var "v",[],IntTy,PrimAppE AddP [VarE (Var "n"),LitE 1]) (Ext (RetE [Var "jump1"] (Var "v")))))),
--           ("Node",[(Var "x",Var "l1"),(Var "y",Var "l2")],
--           Ext (LetLocE (Var "lout1") (AfterConstantC 1 (Var "lout") (Var "lout1"))
--           (LetE (Var "x1",[Var "endof2"],PackedTy "Tree" (Var "lout1"),AppE (Var "add1") [Var "l1",Var "lout1"] (VarE (Var "x")))
--           (Ext (LetLocE (Var "l2") (FromEndC (Var "endof2")) (Ext (LetLocE (Var "lout2") (AfterVariableC (Var "x1") (Var "lout1") (Var "lout2"))
--           (LetE (Var "y1",[Var "endof3"],PackedTy "Tree" (Var "lout2"),AppE (Var "add1") [Var "l2",Var "lout2"] (VarE (Var "y")))
--           (LetE (Var "z",[],PackedTy "Tree" (Var "lout"),DataConE (Var "lout") "Node" [VarE (Var "x1"),VarE (Var "y1")])
--           (ext (RetE [Var "endof3"] (Var "z"))))))))))))]}

