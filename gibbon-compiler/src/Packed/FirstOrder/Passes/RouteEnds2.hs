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

module Packed.FirstOrder.Passes.RouteEnds2
    ( routeEnds ) where

import Packed.FirstOrder.Common
import Packed.FirstOrder.L2.Syntax as L2
import qualified Packed.FirstOrder.L1.Syntax as L1
import Data.Set as S
import Data.Map as M
import Data.List as L
import Control.Monad 

-- | Chatter level for this module:
lvl :: Int
lvl = 4


data EndOfRel = EndOfRel { endOf :: M.Map LocVar LocVar, equivTo :: M.Map LocVar LocVar }

emptyRel :: EndOfRel
emptyRel = EndOfRel M.empty M.empty

mkEqual :: LocVar -> LocVar -> EndOfRel -> EndOfRel
mkEqual l1 l2 EndOfRel{endOf,equivTo} =
    EndOfRel{endOf=endOf,equivTo=M.insert l1 l2 equivTo}

mkEnd :: LocVar -> LocVar -> EndOfRel -> EndOfRel
mkEnd l1 l2 EndOfRel{endOf,equivTo} =
    EndOfRel{endOf=M.insert l1 l2 endOf,equivTo=equivTo}

findEnd :: LocVar -> EndOfRel -> LocVar
findEnd l EndOfRel{endOf,equivTo} =
    case M.lookup l endOf of
      Nothing -> case M.lookup l equivTo of
                   Nothing -> error $ "Failed finding the end of " ++ (show l)
                   Just leq -> findEnd leq EndOfRel{endOf,equivTo}
      Just lend -> lend

routeEnds :: Prog -> SyM Prog
routeEnds Prog{ddefs,fundefs,mainExp} = do
  fds' <- mapM fdty $ M.elems fundefs
  let fundefs' = M.fromList $ L.map (\f -> (funname f,f)) fds'
  fds'' <- mapM (fd fundefs') fds'
  let fundefs'' = M.fromList $ L.map (\f -> (funname f,f)) fds''
  mainExp' <- case mainExp of
                Nothing -> return Nothing
                Just (e,t) -> do e' <- exp fundefs'' [] emptyRel M.empty M.empty e
                                 return $ Just (e',t)
  return $ Prog ddefs fundefs'' mainExp'

  where
    
    fdty :: L2.FunDef -> SyM L2.FunDef 
    fdty L2.FunDef{funname,funty,funarg,funbod} =
        do let (ArrowTy locin tyin eff tyout _locout) = funty
               handleLoc (LRM l r m) ls = if S.member (Traverse l) eff then (LRM l r m):ls else ls
               locout' = L.map EndOf $ L.foldr handleLoc [] locin
           return L2.FunDef{funname,funty=(ArrowTy locin tyin eff tyout locout'),funarg,funbod}
                                          

    fd :: NewFuns -> L2.FunDef -> SyM L2.FunDef
    fd fns L2.FunDef{funname,funty,funarg,funbod} =
        do let (ArrowTy locin tyin eff _tyout _locout) = funty
               handleLoc (LRM l _r _m) ls = if S.member (Traverse l) eff then l:ls else ls
               retlocs = L.foldr handleLoc [] locin
               lenv = case tyin of
                        PackedTy _n l -> M.insert funarg l $ M.empty
                        ProdTy _tys -> error "Multiple function args not handled yet"
                        _ -> M.empty
           funbod' <- exp fns retlocs emptyRel lenv M.empty funbod
           return L2.FunDef{funname,funty,funarg,funbod=funbod'}

    exp :: NewFuns -> [LocVar] -> EndOfRel -> M.Map Var LocVar -> M.Map LocVar LocVar -> Exp2 -> SyM Exp2
    exp fns retlocs eor lenv afterenv e =
        case e of
          
          VarE v -> mkRet retlocs $ VarE v

          LitE i -> return $ LitE i

          LitSymE v -> return $ LitSymE v

          AppE v ls e -> do
                 v' <- gensym "tailapp"
                 let ty = arrOut $ funtype v
                     e' = LetE (v',[],ty,AppE v ls e) (VarE v')
                 exp fns retlocs eor lenv afterenv e'
                 
          PrimAppE _pr _es -> error $ "Found complex expression in tail: " ++ (show e)

          LetE (v,_ls,ty,(AppE f lsin e1)) e2 -> do
                 let fty = funtype f
                     rets = S.fromList $ locRets fty
                     travlist = zip lsin $ L.map (\l -> S.member (EndOf l) rets)  (locVars fty)
                     lenv' = case ty of
                               PackedTy _n l -> M.insert v l lenv
                               _ -> lenv

                 let handleTravList lst (_l,False) = return lst
                     handleTravList lst (l,True) = gensym "endof" >>= \l' -> return $ (l,l'):lst

                 let mkEor (l1,l2) eor = mkEnd l1 l2 eor

                 let wrapBody e ((l1,l2):ls) = case M.lookup l1 afterenv of
                                                 Nothing -> wrapBody e ls
                                                 Just la -> wrapBody (Ext (LetLocE la (FromEndC l2) e)) ls
                     wrapBody e [] = e

                 newls <- foldM handleTravList [] travlist
                 let eor' = L.foldr mkEor eor newls
                 let outlocs = L.map snd newls
                 e2' <- exp fns retlocs eor' lenv' afterenv e2
                 return $ LetE (v,outlocs,ty,AppE f lsin e1) (wrapBody e2' newls)

          LetE (v,ls,PackedTy n l,e1) e2 -> do
                 e2' <- exp fns retlocs eor (M.insert v l lenv) afterenv e2
                 return $ LetE (v,ls,PackedTy n l,e1) e2'

          LetE (v,ls,ty,e1) e2 -> do
                 e2' <- exp fns retlocs eor lenv afterenv e2
                 return $ LetE (v,ls,ty,e1) e2'

          IfE e1 e2 e3 -> do
                 e2' <- exp fns retlocs eor lenv afterenv e2
                 e3' <- exp fns retlocs eor lenv afterenv e3
                 return $ IfE e1 e2' e3'

          MkProdE _es -> error $ "Found complex expression in tail: " ++ (show e)

          ProjE _i _e -> error $ "Found complex expression in tail: " ++ (show e)

          CaseE (VarE x) brs -> do
                 brs' <- forM brs $ \(dc, vls, e) -> do
                                          let need = snd $ last vls
                                              argtys = lookupDataCon ddefs dc
                                              lx = case M.lookup x lenv of
                                                     Nothing -> error $ "Failed to find " ++ (show x)
                                                     Just l -> l
                                              eor' = mkEqual lx need eor
                                              f (l1,l2) env = M.insert l1 l2 env
                                              afterenv' = L.foldr f afterenv $ zip (L.map snd vls) (tail $ L.map snd vls)
                                              handleLoc (eor,e) (_,(PackedTy _ _)) = return (eor,e)
                                              handleLoc (eor,e) (l1,_ty) = do l2 <- gensym "jump"
                                                                              let eor' = mkEnd l1 l2 eor
                                                                                  e' = Ext $ LetLocE l2 (AfterConstantC 1 l1 l2) e
                                                                              return (eor',e')
                                          -- e' <- exp fns retlocs eor' lenv afterenv' e
                                          (eor'',e') <- foldM handleLoc (eor',e) $ zip (L.map snd vls) argtys
                                          e'' <- exp fns retlocs eor'' lenv afterenv' e'
                                          return (dc, vls, e'')
                 return $ CaseE (VarE x) brs'

          DataConE _l _dc _es -> error $ "Found complex expression in tail: " ++ (show e)

          TimeIt e ty b -> do
                 e' <- exp fns retlocs eor lenv afterenv e
                 return $ TimeIt e' ty b

          Ext (LetRegionE r e) -> do
                 e' <- exp fns retlocs eor lenv afterenv e
                 return $ Ext (LetRegionE r e')

          Ext (LetLocE v (StartOfC l r) e) -> do
                 e' <- exp fns retlocs eor lenv afterenv e
                 return $ Ext (LetLocE v (StartOfC l r) e')

          Ext (LetLocE v (AfterConstantC i l1 l2) e) -> do
                 e' <- exp fns retlocs eor lenv afterenv e
                 return $ Ext (LetLocE v (AfterConstantC i l1 l2) e')

          Ext (LetLocE v (AfterVariableC x l1 l2) e) -> do
                 e' <- exp fns retlocs eor lenv afterenv e
                 return $ Ext (LetLocE v (AfterVariableC x l1 l2) e')

          _ -> error $ "Unsupported expression: " ++ (show e)


        where  mkRet :: [LocVar] -> Exp2 -> SyM Exp2
               mkRet ls (VarE v) = let ends = L.map (\l -> findEnd l eor) ls
                                   in return $ Ext (RetE ends v)
               mkRet _ e = error $ "Expected variable reference in tail call, got " ++ (show e)

               funtype :: Var -> ArrowTy Ty2
               funtype v = case M.lookup v fns of
                             Nothing -> error $ "Function " ++ (show v) ++ " not found"
                             Just fundef -> funty fundef

ddtree :: DDefs Ty
ddtree = (fromListDD [DDef (toVar "Tree") 
                              [ ("Leaf",[(False,IntTy)])
                              , ("Node",[(False,PackedTy "Tree" "l")
                                        ,(False,PackedTy "Tree" "l")])]])

tester' :: Exp -> Prog
tester' e =
    let ddfs = ddtree
        funs = (M.fromList [(toVar "add1",exadd1)])
    in Prog ddfs funs (Just (e,IntTy))

exadd1 :: L2.FunDef
exadd1 = L2.FunDef "add1" exadd1ty "tr" exadd1bod

exadd1ty :: ArrowTy Ty2
exadd1ty = (ArrowTy
            [LRM "lin" (VarR "r1") Input, LRM "lout" (VarR "r1") Output]
            (PackedTy "tree" "lin")
            (S.fromList [Traverse "lin"])
            (PackedTy "tree" "lout")
            [EndOf $ LRM "lin" (VarR "r1") Input])

exadd1bod :: Exp2
exadd1bod =
    CaseE (VarE "tr") $
      [ ("Leaf", [("n","l0")], LetE ("v",[],IntTy,PrimAppE L1.AddP [VarE "n", LitE 1]) (VarE "v"))
      , ("Node", [("x","l1"),("y","l2")],
         Ext $ LetLocE "lout1" (AfterConstantC 1 "lout" "lout1") $
         LetE ("x1",[],PackedTy "Tree" "lout1", AppE "add1" ["l1","lout1"] (VarE "x")) $
         Ext $ LetLocE "lout2" (AfterVariableC "x1" "lout1" "lout2") $
         LetE ("y1",[],PackedTy "Tree" "lout2", AppE "add1" ["l2","lout2"] (VarE "y")) $
         LetE ("z",[],PackedTy "Tree" "lout", 
                  DataConE "lout" "Node" [ VarE "x1" , VarE "y1"]) $
         VarE "z")
      ]

tester e =
    let p = tester' e
    in fst $ runSyM 1 $ routeEnds p

test1 :: Exp2
test1 = Ext $ LetRegionE (VarR "r") $ Ext $ LetLocE "ltest" (StartOfC "l" (VarR "r")) $
        Ext $ LetLocE "ltest1" (AfterConstantC 1 "ltest" "ltest1") $ 
        LetE ("x", [], PackedTy "Tree" "ltest1", DataConE "ltest1" "Leaf" [LitE 1]) $
        Ext $ LetLocE "ltest2" (AfterVariableC "x" "ltest1" "ltest2") $
        LetE ("y", [], PackedTy "Tree" "ltest2", DataConE "ltest2" "Leaf" [LitE 2]) $
        LetE ("z", [], PackedTy "Tree" "ltest", DataConE "ltest" "Node" [VarE "x", VarE "y"]) $
        Ext $ LetRegionE (VarR "o") $ Ext $ LetLocE "lo" (StartOfC "lo" (VarR "o")) $
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
