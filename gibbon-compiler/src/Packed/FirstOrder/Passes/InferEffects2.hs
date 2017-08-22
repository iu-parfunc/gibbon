{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

-- | An intermediate language with an effect system that captures traversals.
--
-- ASSUMES that the flatten pass has run, and thus we have trivial AppE operands.
--

module Packed.FirstOrder.Passes.InferEffects2
  (inferEffects) where

import Data.Loc
import Data.List as L
import Data.Set as S
import Data.Map as M
import Text.PrettyPrint.GenericPretty
import Debug.Trace

import Packed.FirstOrder.L2.Syntax
import Packed.FirstOrder.Common hiding (FunDef)
import Packed.FirstOrder.L1.Syntax hiding (Prog, FunDef, ddefs, fundefs, mainExp)

--------------------------------------------------------------------------------

-- | Chatter level for this module:
lvl :: Int
lvl = 5

type FunEnv = M.Map Var (ArrowTy Ty2)

locsEffect :: [LocVar] -> Set Effect
locsEffect = S.fromList . L.map Traverse

type LocEnv = M.Map Var LocVar

-- | We initially populate all functions with MAXIMUM effect signatures.
--   Subsequently, these monotonically SHRINK until a fixpoint.
--   We also associate fresh location variables with packed types.
initialEnv :: NewFuns -> FunEnv
initialEnv mp = M.map go mp
  where
    go :: FunDef -> ArrowTy Ty2
    go fn@FunDef{funty} =
      let locs       = getArrowTyLocs funty
          maxEffects = locsEffect locs
      in funty { arrEffs = maxEffects }


inferEffects :: Prog -> SyM Prog
inferEffects prg@Prog{ddefs,fundefs} = do
  let finalFunTys = fixpoint 1 fundefs (initialEnv fundefs)
      funs = M.map (\fn@FunDef{funname} ->
                       fn{ funty = finalFunTys ! funname })
             fundefs
  return $ prg { fundefs = funs }
  where
    fixpoint :: Int -> NewFuns -> FunEnv -> FunEnv
    fixpoint iter funs fenv =
       let funtys = M.map (inferFunDef ddefs fenv) funs
       in
         if fenv == funtys
         then dbgTrace 4 ("\n<== Fixpoint completed after iteration "++show iter++" ==>") $ fenv
         else fixpoint (iter+1) funs funtys


inferFunDef :: DDefs Ty2 -> FunEnv -> FunDef -> ArrowTy Ty2
inferFunDef ddfs fenv fn@FunDef{funarg,funbod,funty} =
  let env0 = M.singleton funarg inLoc
      inLoc = head $ L.map (\(LRM l _ _) -> l) $
              L.filter (\(LRM _ _ m) -> m == Input) (locVars funty)
      (eff,_)  = inferExp ddfs fenv env0 funbod
      eff'    = S.filter ((==) (Traverse inLoc)) eff
  in (funty {arrEffs = eff'})


-- | TODO: same location variables for the identity function. add missing cases
inferExp :: DDefs Ty2 -> FunEnv -> LocEnv -> L Exp2 -> (Set Effect, Maybe LocVar)
inferExp ddfs fenv env (L p exp) =
  case exp of
    -- QUESTION: does a variable reference count as traversing to the end?
    -- If so, the identity function has the traverse effect.
    -- I'd prefer that the identity function get type (Tree_p -{}-> Tree_p).
    VarE v -> (S.empty, Just $ env # v)

    LitE _ -> (S.empty, Nothing)

    LetE (v,locs,ty,rhs) bod ->
      let (effRhs,_) = inferExp ddfs fenv env rhs
          (effBod,_) = inferExp ddfs fenv env bod
      in (S.union effRhs effBod, Nothing)

    AppE v locs e ->
      -- Substitue locations used at this particular call-site in the function
      -- effects computed so far
      let orgLocs = getArrowTyLocs (fenv # v)
          locMap  = M.fromList $ zip orgLocs locs
          eff     = arrEffs (fenv # v)
      in (substEffs locMap eff, Nothing)

    -- If rands are already trivial, no traversal effects can occur here.
    -- All primitives operate on non-packed data.
    PrimAppE _ rands -> assertTrivs rands (S.empty, Nothing)

    CaseE e mp ->
      let (eff,loc1) = inferExp ddfs fenv env e
          (bools,effsLocs) = unzip $ L.map caserhs mp
          (effs,_) = unzip effsLocs
          -- Critical policy point!  We only get to the end if ALL
          -- branches get to the end.
          end = if all id bools
                then case loc1 of
                       Just v  -> S.singleton (Traverse v)
                       Nothing -> S.empty
                else S.empty
          ret = S.union (S.union eff end)
                        (L.foldl1 S.intersection effs)
      in (ret, Nothing)

    -- Construct output packed data.  We will always "scroll to the end" of
    -- output values, so they are not interesting for this effect analysis.
    DataConE _loc _dcon es -> assertTrivs es (S.empty, Nothing)

    Ext (LetRegionE _ rhs) -> inferExp ddfs fenv env rhs
    Ext (LetLocE _ _ rhs)  -> inferExp ddfs fenv env rhs
    Ext (RetE _ _)         -> (S.empty, Nothing)
    Ext (FromEndE _ )      -> (S.empty, Nothing)

    oth -> error $ "FINISHME: inferExp " ++ sdoc oth

  where
    caserhs :: (DataCon, [(Var,LocVar)], L Exp2) -> (Bool, (Set Effect, Maybe LocVar))
    -- We've gotten "to the end" of a nullary constructor just by matching it:
    caserhs (dcon,[],e) = ( True , inferExp ddfs fenv env e )
    caserhs (dcon,patVs,e) =
      let (vars,locs) = L.unzip patVs
          tys    = lookupDataCon ddfs dcon
          zipped = fragileZip' vars tys ("Error in "++ dcon ++" case: "
                                         ++"pattern vars, "++show vars++
                                         ", do not match the number of types "
                                         ++show tys)
          packedOnly = L.filter (\(_,t) -> hasPacked t) zipped

          (eff,_) = inferExp ddfs fenv env e
          winner = dbgTrace lvl ("\nInside caserhs, for "++show (dcon,patVs,tys)
                        -- ++ "\n  freevars "++show freeRHS
                        -- ++",\n  env "++show env'++",\n  eff "++show eff
                                ) $
                   -- If there is NO packed child data, then our object has static size:
                   (L.all (not . hasPacked) tys) ||

                   -- Or if the last non-static item was in fact traversed:
                   (case packedOnly of
                         []  -> False
                         _:_ -> let patVMap       = M.fromList patVs
                                    lastPackedLoc = patVMap ! (fst$last packedOnly)
                                in S.member (Traverse lastPackedLoc) eff)
                   -- Or maybe the last-use rule applies:
                   -- TODO

          -- Also, in any binding form we are obligated to not return
          -- our local bindings in traversal side effects:
          isLocal (Traverse v) = L.elem v locs
          stripped = S.filter isLocal eff
      in ( winner, (stripped,Nothing) )


test1 = runSyM 0 $ inferEffects add1

add1 :: Prog
add1 = Prog { ddefs = add1DDefs
            , fundefs = M.fromList [("add1", add1FunDef )]
            , mainExp = Nothing
            }

test2 = runSyM 0 $ inferEffects useAdd1

useAdd1 :: Prog
useAdd1 = Prog { ddefs = add1DDefs
               , fundefs = M.fromList [("add1", add1FunDef ),
                                       ("useAdd1", useAdd1FunDef)]
               , mainExp = Nothing
               }

useAdd1FunDef :: FunDef
useAdd1FunDef = FunDef
                { funname = "useAdd1"
                , funty = ArrowTy { locVars = [LRM "lin10" (VarR "r10") Input,
                                                 LRM "lout10" (VarR "r10") Output]
                                    , arrIn = PackedTy "tree" "lin10"
                                    , arrEffs = S.fromList []
                                    , arrOut = PackedTy "tree" "lout10"
                                    , locRets = [EndOf (LRM "lin10" (VarR "r10") Input)]
                                    }
                , funarg = "uatr"
                , funbod = l$ LetE ("x10",
                                      [],
                                      PackedTy "Tree" "lout10",
                                      l$AppE "add1"
                                      ["lin10","lout10"]
                                      (l$VarE "uatr"))
                            (l$ VarE "x10")
                }

add1DDefs = M.fromList
            [("Tree",
              DDef {tyName = "Tree",
                    dataCons = [ ("Leaf", [(False, IntTy)]),
                                 ("Node",
                                  [(False, PackedTy "Tree" "l"),(False, PackedTy "Tree" "l")])
                               ]})]


add1FunDef = FunDef { funname = "add1"
                    , funty   = ArrowTy {locVars = [LRM "lin" (VarR "r1") Input,
                                                    LRM "lout" (VarR "r1") Output],
                                         arrIn = PackedTy "tree" "lin",
                                         arrEffs = S.fromList [],
                                         arrOut = PackedTy "tree" "lout",
                                         locRets = [EndOf (LRM "lin" (VarR "r1") Input)]
                                        }
                    , funarg = "tr"
                    , funbod = l$ CaseE (l$ VarE "tr")
                               [ ("Leaf",
                                   [("n", "l0")],
                                   l$ LetE ("v", [], IntTy, l$PrimAppE AddP [l$VarE "n", l$LitE 1])
                                   (l$ VarE "v")),

                                 ("Node",
                                   [("x", "lx"),("y", "ly")],
                                   l$Ext (LetLocE "lx1"
                                         (AfterConstantLE 1 "lout")
                                         (l$LetE ("x1",
                                                 [],
                                                 PackedTy "Tree" "lx1",
                                                 l$AppE "add1"
                                                 ["lx","lx1"]
                                                 (l$VarE "x"))
                                           (l$Ext (LetLocE "ly1"
                                                  (AfterVariableLE "x1"
                                                    "lx1")
                                                  (l$LetE ("y1",
                                                          [],
                                                          PackedTy "Tree"
                                                          "ly1",
                                                          l$AppE "add1"
                                                          ["ly", "ly1"]
                                                          (l$VarE "y"))
                                                    (l$LetE ("z",
                                                            [],
                                                            PackedTy "Tree"
                                                            "lout",
                                                            l$DataConE "lout"
                                                            "Node"
                                                            [l$VarE "x1", l$VarE "y1"])
                                                      (l$VarE "z"))))))))
                               ]
                    }
