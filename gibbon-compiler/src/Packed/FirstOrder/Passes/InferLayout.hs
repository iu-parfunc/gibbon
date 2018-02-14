{-# LANGUAGE OverloadedStrings #-}

module Packed.FirstOrder.Passes.InferLayout where


import Data.Loc
import Data.List as L
import Data.Map as M
import Text.PrettyPrint.GenericPretty

import Packed.FirstOrder.Common
import Packed.FirstOrder.L1.Syntax

--------------------------------------------------------------------------------


type Size = Int
type SEnv = M.Map Var Size

type FunDef1 = FunDef Ty1 (L Exp1)

addLayout :: Prog -> SyM Prog
addLayout prg@Prog{ddefs,fundefs,mainExp} = do
  let sddefs = toSizedDDefs ddefs
  funs <- mapM (\(nm,f) -> (nm,) <$> addLayoutFun sddefs f) (M.toList fundefs)
  mainExp' <-
    case mainExp of
      Just ex -> fmap Just (addLayoutExp sddefs M.empty ex)
      Nothing -> return Nothing
  return prg { ddefs = sddefs
             , fundefs = M.fromList funs
             , mainExp = mainExp'
             }

addLayoutFun :: DDefs Ty1 -> FunDef1 -> SyM FunDef1
addLayoutFun ddfs fd@FunDef{funBody} = do
  bod <- addLayoutExp ddfs M.empty funBody
  return $ fd{funBody = bod}


-- | Add layout information to the AST
--
-- (1) Convert DDefs to (Sized DDefs)
--
-- (2) All data constructors that should have size fields are transformed to "sized" data constructors.
--     And the size fields are added at appropriate places (before all other arguments so that they're
--     written immediately after the tag).
--
-- (3) Case expressions are modified to work with the "sized" data constructors.
--     Pattern matches for these constructors now bind the additional size fields too.
--
addLayoutExp :: Out a => DDefs (UrTy a) -> SEnv -> L Exp1 -> SyM (L Exp1)
addLayoutExp ddfs senv (L p ex) = L p <$>
  case ex of
    DataConE loc dcon args ->
      case numPackedDataCon ddfs dcon of
        Just n  -> do
          let needSizeOf = take n args
          szs <- mapM (\arg -> do
                         v <- gensym "sz"
                         case inferSize senv arg of
                           Just sz -> return (v,[],IntTy, l$ LitE sz)
                           Nothing -> return (v,[],IntTy, l$ PrimAppE SizeOf [arg]))
                 needSizeOf
          let szVars = L.map (\(v,_,_,_) -> v) szs
              szExps = L.map (l . VarE) szVars
          return $ unLoc $ mkLets szs (l$ DataConE loc (toSizedDataCon dcon) (szExps ++ args))
        Nothing -> return ex

    -- standard recursion here
    VarE{}    -> return ex
    LitE{}    -> return ex
    LitSymE{} -> return ex
    AppE f locs arg -> AppE f locs <$> go arg
    PrimAppE f args -> PrimAppE f <$> mapM go args
    LetE (v,loc,ty,rhs) bod -> do
      let senv' = case inferSize senv rhs of
                    Nothing -> senv
                    Just sz -> M.insert v sz senv
      LetE <$> (v,loc,ty,) <$> go rhs <*> addLayoutExp ddfs senv' bod
    IfE a b c  -> IfE <$> go a <*> go b <*> go c
    MkProdE xs -> MkProdE <$> mapM go xs
    ProjE i e  -> ProjE i <$> go e
    CaseE scrt mp -> CaseE scrt <$> mapM docase mp
    TimeIt e ty b -> do
      e' <- go e
      return $ TimeIt e' ty b
    Ext _ -> return ex
    MapE{}  -> error "addLayoutExp: TODO MapE"
    FoldE{} -> error "addLayoutExp: TODO FoldE"

  where
    go = addLayoutExp ddfs senv

    docase :: (DataCon, [(Var,())], L Exp1) -> SyM (DataCon, [(Var,())], L Exp1)
    docase (dcon,vs,bod) = do
      case numPackedDataCon ddfs dcon of
        Just n -> do
          szVars <- mapM (\_ -> (, ()) <$> gensym "sz") [1..n]
          (toSizedDataCon dcon, szVars ++ vs,) <$> go bod
        Nothing -> (dcon,vs,) <$> go bod

    -- | Minor optimization. Add literal int size fields where we can
    inferSize :: SEnv -> L Exp1 -> Maybe Size
    inferSize env (L _ ex') =
      case ex' of
        VarE v -> M.lookup v env
        LitE{}       -> sizeOf IntTy
        LitSymE{}    -> sizeOf SymTy
        PrimAppE f _ -> sizeOf (primRetTy f) -- or 8 ?
        LetE (v,_,_,rhs) bod -> case inferSize env rhs of
                                  Just sz -> inferSize (M.insert v sz env) bod
                                  Nothing -> inferSize env bod
        MkProdE ls -> sum <$> mapM (inferSize env) ls
        Ext _      -> Nothing
        DataConE _ _ args -> fmap (+1) $ sum <$> (mapM (inferSize env) args)
        _ -> Nothing
        -- AppE{} ->
        -- IfE a b c -> __
        -- ProjE i e -> __
        -- CaseE _ mp -> __
        -- TimeIt e ty b -> __


------------------------------------------------------------------------------------------

{-

Note [Sized DDefs]:
~~~~~~~~~~~~~~~~~~~

ddtree :: DDefs Ty1
ddtree = fromListDD [DDef (toVar "Tree")
                      [ ("Leaf",[(False,IntTy)])
                      , ("Node",[ (False,PackedTy "Tree" ())
                                , (False,PackedTy "Tree" ())])
                      ]]

becomes,

ddtree :: DDefs Ty1
ddtree = fromListDD [DDef (toVar "Tree")
                     [ ("Leaf"      ,[(False,IntTy)])

                     , ("Node",     [ (False,PackedTy "Tree" ())
                                    , (False,PackedTy "Tree" ())])

                     , (Sized_Node, [ (False, IntTy) -- size field
                                    , (False,PackedTy "Tree" ())
                                    , (False,PackedTy "Tree" ())])
                     ]]


TODO: Need to encode this information in the type


Location Inference:
~~~~~~~~~~~~~~~~~~~

To get a location l' after a _sized_ tag at l,

let skip = 1 + (8 * #indirections_for_tag)
in  l'   = AfterConstantLE skip l

-}
