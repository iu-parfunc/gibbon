{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

-- | Repair mutable-cursor L3 after cursorization.
--
-- Cursorize still owns the source-level translation. This pass is deliberately
-- smaller: it works over the already cursorized L3 let-chain and repairs places
-- where a temporary cursor-array is used as a mutable output cursor for a call.
-- In that shape, the callee mutates the temporary in place, so the owning
-- mutable cursor-array must be updated from it before later code reads the
-- owner as the current cursor state.
module Gibbon.Passes.MutableCursorFutures
  ( repairMutableCursorFutures
  , repairMutableCursorBounds
  ) where

import qualified Data.List as L
import qualified Data.Map as M

import Gibbon.Common
import Gibbon.Language
import qualified Gibbon.L3.Syntax as L3

type Bind3 = (Var, [()], L3.Ty3, L3.Exp3)

data CursorArrayCopy = CursorArrayCopy Var Var L3.Ty3
  deriving (Eq, Ord, Show)

data MutableCallShape =
  MutableCallShape
    { mcsOutputEndIx :: Int
    , mcsOutputCurrentIx :: Int
    , mcsCursorArrayTy :: L3.Ty3
    , mcsInstallAfterCall :: Bool
    }
  deriving (Eq, Ord, Show)

type FunShapes = M.Map Var MutableCallShape

repairMutableCursorFutures :: L3.Prog3 -> PassM L3.Prog3
repairMutableCursorFutures = repairMutableCursorFuturesWith False

repairMutableCursorBounds :: L3.Prog3 -> PassM L3.Prog3
repairMutableCursorBounds = repairMutableCursorFuturesWith False

repairMutableCursorFuturesWith :: Bool -> L3.Prog3 -> PassM L3.Prog3
repairMutableCursorFuturesWith installPostCalls prog@Prog{fundefs, mainExp} = do
  let funShapes = M.mapMaybe mutableCallShape fundefs
  fds_p <- mapM (repairFun installPostCalls funShapes) (M.elems fundefs)
  mainExp_p <- mapM (\(e, ty) -> (,ty) <$> repairExp installPostCalls funShapes e) mainExp
  pure $
    prog
      { fundefs = M.fromList [ (funName f, f) | f <- fds_p ]
      , mainExp = mainExp_p
      }

repairFun :: Bool -> FunShapes -> L3.FunDef3 -> PassM L3.FunDef3
repairFun installPostCalls funShapes fn@FunDef{funBody} = do
  let env0 = M.fromList $ zip (funArgs fn) (fst $ funTy fn)
      currentOutputEnd =
        case funArgs fn of
          v:_ -> Just v
          [] -> Nothing
  funBody_p <- repairExpWith installPostCalls funShapes currentOutputEnd env0 funBody
  pure fn { funBody = funBody_p }

repairExp :: Bool -> FunShapes -> L3.Exp3 -> PassM L3.Exp3
repairExp installPostCalls funShapes = repairExpWith installPostCalls funShapes Nothing M.empty

repairExpWith :: Bool -> FunShapes -> Maybe Var -> M.Map Var L3.Ty3 -> L3.Exp3 -> PassM L3.Exp3
repairExpWith installPostCalls funShapes currentOutputEnd env ex0 = do
  (binds, tailExp) <- repairLets installPostCalls funShapes currentOutputEnd env ex0
  pure $ L3.mkLets binds tailExp

repairLets :: Bool -> FunShapes -> Maybe Var -> M.Map Var L3.Ty3 -> L3.Exp3 -> PassM ([Bind3], L3.Exp3)
repairLets installPostCalls funShapes currentOutputEnd env ex =
  case ex of
    L3.LetE (v, locs, ty, rhs) bod -> do
      rhs_p <- repairNested installPostCalls funShapes currentOutputEnd env rhs
      let env_p = M.insert v ty env
      (bodBinds, tailExp) <- repairLets installPostCalls funShapes currentOutputEnd env_p bod
      post <- if installPostCalls then postCallInstalls funShapes env rhs_p else pure []
      pre <- preTailCallFutureInstall env v rhs_p bodBinds tailExp
      pure (pre ++ (v, locs, ty, rhs_p) : post ++ bodBinds, tailExp)
    _ -> ([],) <$> repairNested installPostCalls funShapes currentOutputEnd env ex

preTailCallFutureInstall :: M.Map Var L3.Ty3 -> Var -> L3.Exp3 -> [Bind3] -> L3.Exp3 -> PassM [Bind3]
preTailCallFutureInstall env callResult rhs bodBinds tailExp =
  case (rhs, bodBinds, tailExp) of
    (L3.AppE{}, (_, _, L3.CursorTy, L3.VarE future) : _, L3.VarE ret)
      | ret == callResult
      , Just mutCur <- singleMutableCursorCallArg rhs
      , mutCur /= future
      , M.lookup mutCur env == Just L3.MutCursorTy -> do
          void <- gensym "install_future_cursor"
          pure [(void, [], L3.ProdTy [], L3.Ext $ L3.WriteCursorMutable mutCur (L3.VarE future))]
    _ -> pure []

singleMutableCursorCallArg :: L3.Exp3 -> Maybe Var
singleMutableCursorCallArg rhs =
  case rhs of
    L3.AppE _ _ _ (L3.VarE endArg : L3.VarE curArg : _)
      | endArg == curArg -> Just curArg
    _ -> Nothing

repairNested :: Bool -> FunShapes -> Maybe Var -> M.Map Var L3.Ty3 -> L3.Exp3 -> PassM L3.Exp3
repairNested installPostCalls funShapes currentOutputEnd env ex =
  case ex of
    L3.AppE fn cty locs args ->
      L3.AppE fn cty locs . repairMutableProducerBounds funShapes currentOutputEnd fn
        <$> mapM (repairExpWith installPostCalls funShapes currentOutputEnd env) args
    L3.IfE a b c ->
      L3.IfE <$> repairExpWith installPostCalls funShapes currentOutputEnd env a
             <*> repairExpWith installPostCalls funShapes currentOutputEnd env b
             <*> repairExpWith installPostCalls funShapes currentOutputEnd env c
    L3.CaseE scrt brs ->
      L3.CaseE <$> repairExpWith installPostCalls funShapes currentOutputEnd env scrt
               <*> mapM (\(dc, vars, rhs) -> (dc, vars,) <$> repairExpWith installPostCalls funShapes currentOutputEnd env rhs) brs
    L3.MkProdE ls -> L3.MkProdE <$> mapM (repairExpWith installPostCalls funShapes currentOutputEnd env) ls
    L3.ProjE i e -> L3.ProjE i <$> repairExpWith installPostCalls funShapes currentOutputEnd env e
    L3.PrimAppE p args -> L3.PrimAppE p <$> mapM (repairExpWith installPostCalls funShapes currentOutputEnd env) args
    L3.TimeIt e ty b -> L3.TimeIt <$> repairExpWith installPostCalls funShapes currentOutputEnd env e <*> pure ty <*> pure b
    L3.WithArenaE v e -> L3.WithArenaE v <$> repairExpWith installPostCalls funShapes currentOutputEnd env e
    L3.SpawnE fn locs args -> L3.SpawnE fn locs <$> mapM (repairExpWith installPostCalls funShapes currentOutputEnd env) args
    L3.MapE (v, ty, rhs) bod ->
      L3.MapE <$> ((v, ty,) <$> repairExpWith installPostCalls funShapes currentOutputEnd env rhs)
              <*> repairExpWith installPostCalls funShapes currentOutputEnd (M.insert v ty env) bod
    L3.FoldE (v1, ty1, rhs1) (v2, ty2, rhs2) bod ->
      L3.FoldE
        <$> ((v1, ty1,) <$> repairExpWith installPostCalls funShapes currentOutputEnd env rhs1)
        <*> ((v2, ty2,) <$> repairExpWith installPostCalls funShapes currentOutputEnd env rhs2)
        <*> repairExpWith installPostCalls funShapes currentOutputEnd (M.insert v1 ty1 (M.insert v2 ty2 env)) bod
    L3.DataConE loc dc args -> L3.DataConE loc dc <$> mapM (repairExpWith installPostCalls funShapes currentOutputEnd env) args
    L3.Ext ext -> L3.Ext <$> repairExt installPostCalls funShapes currentOutputEnd env ext
    _ -> pure ex

repairExt :: Bool -> FunShapes -> Maybe Var -> M.Map Var L3.Ty3 -> L3.E3Ext () L3.Ty3 -> PassM (L3.E3Ext () L3.Ty3)
repairExt installPostCalls funShapes currentOutputEnd env ext =
  case ext of
    L3.ForE idx bound bod ->
      L3.ForE idx <$> repairExpWith installPostCalls funShapes currentOutputEnd env bound
                  <*> repairExpWith installPostCalls funShapes currentOutputEnd (M.delete idx env) bod
    L3.WhileCursor cur bod -> L3.WhileCursor cur <$> repairExpWith installPostCalls funShapes currentOutputEnd env bod
    L3.WhileCursorEnd cur end bod -> L3.WhileCursorEnd cur end <$> repairExpWith installPostCalls funShapes currentOutputEnd env bod
    L3.WriteScalar s cur rhs -> L3.WriteScalar s cur <$> repairExpWith installPostCalls funShapes currentOutputEnd env rhs
    L3.WriteTagPacked cur rhs -> L3.WriteTagPacked cur <$> repairExpWith installPostCalls funShapes currentOutputEnd env rhs
    L3.WriteTaggedCursor cur rhs -> L3.WriteTaggedCursor cur <$> repairExpWith installPostCalls funShapes currentOutputEnd env rhs
    L3.WriteCursorMutable cur rhs -> L3.WriteCursorMutable cur <$> repairExpWith installPostCalls funShapes currentOutputEnd env rhs
    L3.WriteList cur rhs ty -> (\rhs_p -> L3.WriteList cur rhs_p ty) <$> repairExpWith installPostCalls funShapes currentOutputEnd env rhs
    L3.WriteVector cur rhs ty -> (\rhs_p -> L3.WriteVector cur rhs_p ty) <$> repairExpWith installPostCalls funShapes currentOutputEnd env rhs
    L3.AddCursor cur rhs -> L3.AddCursor cur <$> repairExpWith installPostCalls funShapes currentOutputEnd env rhs
    L3.BumpCursorMutable cur rhs -> L3.BumpCursorMutable cur <$> repairExpWith installPostCalls funShapes currentOutputEnd env rhs
    L3.AddrOfCursor rhs -> L3.AddrOfCursor <$> repairExpWith installPostCalls funShapes currentOutputEnd env rhs
    L3.LetAvail vars bod -> L3.LetAvail vars <$> repairExpWith installPostCalls funShapes currentOutputEnd env bod
    L3.Assert rhs -> L3.Assert <$> repairExpWith installPostCalls funShapes currentOutputEnd env rhs
    L3.WriteCursorSelectiveIndirection cur target end mask ->
      L3.WriteCursorSelectiveIndirection cur target end <$> repairExpWith installPostCalls funShapes currentOutputEnd env mask
    _ -> pure ext

repairMutableProducerBounds :: FunShapes -> Maybe Var -> Var -> [L3.Exp3] -> [L3.Exp3]
repairMutableProducerBounds funShapes currentOutputEnd fn args =
  case M.lookup fn funShapes of
    Just{} -> repairProducerArgs
    Nothing -> args
  where
    repairProducerArgs =
      case (currentOutputEnd, args) of
        (Just ownerEnd, endArg : curArg : rest) ->
          let (endBinds, endCore) = peelLets endArg
              (curBinds, curCore) = peelLets curArg
           in case (endCore, curCore) of
                (L3.VarE endVar, L3.VarE curVar)
                  | endVar == curVar ->
                      L3.VarE ownerEnd : rebuildLets (endBinds ++ curBinds) curCore : rest
                _ -> args
        _ -> args

    peelLets e =
      case e of
        L3.LetE b bod ->
          let (bnds, core) = peelLets bod
           in (b : bnds, core)
        _ -> ([], e)

    rebuildLets bnds e = L3.mkLets bnds e


repairTraversalCopies :: M.Map Var Var -> L3.Exp3 -> L3.Exp3
repairTraversalCopies traverseEnds ex =
  case ex of
    L3.LetE (v, locs, ty, rhs) bod ->
      let rhs0 = repairTraversalCopiesNested traverseEnds rhs
          (rhs_p, traverseEnds_alias) = repairTraversalAlias traverseEnds v rhs0
          traverseEnds_p = recordTraversal traverseEnds_alias rhs_p
       in L3.LetE (v, locs, ty, rhs_p) (repairTraversalCopies traverseEnds_p bod)
    _ -> repairTraversalCopiesNested traverseEnds ex


repairTraversalAlias :: M.Map Var Var -> Var -> L3.Exp3 -> (L3.Exp3, M.Map Var Var)
repairTraversalAlias traverseEnds binder rhs =
  case rhs of
    L3.VarE src
      | Just advanced <- M.lookup src traverseEnds
      , not (L.isPrefixOf "reg_cursor_ptr" (fromVar binder)) ->
          (L3.VarE advanced, M.insert binder advanced traverseEnds)
    _ -> (rhs, traverseEnds)

repairTraversalCopiesNested :: M.Map Var Var -> L3.Exp3 -> L3.Exp3
repairTraversalCopiesNested traverseEnds ex =
  case ex of
    L3.IfE a b c -> L3.IfE (go a) (repairTraversalCopies traverseEnds b) (repairTraversalCopies traverseEnds c)
    L3.CaseE scrt brs -> L3.CaseE (go scrt) [(dc, vars, repairTraversalCopies traverseEnds rhs) | (dc, vars, rhs) <- brs]
    L3.MkProdE es -> L3.MkProdE (map go es)
    L3.ProjE i e -> L3.ProjE i (go e)
    L3.PrimAppE p args -> L3.PrimAppE p (map go args)
    L3.TimeIt e ty b -> L3.TimeIt (go e) ty b
    L3.WithArenaE v e -> L3.WithArenaE v (repairTraversalCopies traverseEnds e)
    L3.SpawnE fn locs args -> L3.SpawnE fn locs (map go args)
    L3.MapE (v, ty, rhs) bod -> L3.MapE (v, ty, go rhs) (repairTraversalCopies traverseEnds bod)
    L3.FoldE (v1, ty1, rhs1) (v2, ty2, rhs2) bod ->
      L3.FoldE (v1, ty1, go rhs1) (v2, ty2, go rhs2) (repairTraversalCopies traverseEnds bod)
    L3.DataConE loc dc args -> L3.DataConE loc dc (map go args)
    L3.Ext ext -> L3.Ext (repairTraversalCopiesExt traverseEnds ext)
    _ -> ex
  where
    go = repairTraversalCopies traverseEnds

repairTraversalCopiesExt :: M.Map Var Var -> L3.E3Ext () L3.Ty3 -> L3.E3Ext () L3.Ty3
repairTraversalCopiesExt traverseEnds ext =
  case ext of
    L3.MemCpy dst src ty -> L3.MemCpy dst (M.findWithDefault src src traverseEnds) ty
    L3.ForE idx bound bod -> L3.ForE idx (repairTraversalCopies traverseEnds bound) (repairTraversalCopies traverseEnds bod)
    L3.WhileCursor cur bod -> L3.WhileCursor cur (repairTraversalCopies traverseEnds bod)
    L3.WhileCursorEnd cur end bod -> L3.WhileCursorEnd cur end (repairTraversalCopies traverseEnds bod)
    L3.WriteScalar s cur rhs -> L3.WriteScalar s cur (repairTraversalCopies traverseEnds rhs)
    L3.WriteTagPacked cur rhs -> L3.WriteTagPacked cur (repairTraversalCopies traverseEnds rhs)
    L3.WriteTaggedCursor cur rhs -> L3.WriteTaggedCursor cur (repairTraversalCopies traverseEnds rhs)
    L3.WriteCursorMutable cur rhs -> L3.WriteCursorMutable cur (repairTraversalCopies traverseEnds rhs)
    L3.WriteList cur rhs ty -> L3.WriteList cur (repairTraversalCopies traverseEnds rhs) ty
    L3.WriteVector cur rhs ty -> L3.WriteVector cur (repairTraversalCopies traverseEnds rhs) ty
    L3.AddCursor cur rhs -> L3.AddCursor cur (repairTraversalCopies traverseEnds rhs)
    L3.BumpCursorMutable cur rhs -> L3.BumpCursorMutable cur (repairTraversalCopies traverseEnds rhs)
    L3.AddrOfCursor rhs -> L3.AddrOfCursor (repairTraversalCopies traverseEnds rhs)
    L3.LetAvail vars bod -> L3.LetAvail vars (repairTraversalCopies traverseEnds bod)
    L3.Assert rhs -> L3.Assert (repairTraversalCopies traverseEnds rhs)
    L3.WriteCursorSelectiveIndirection cur target end mask ->
      L3.WriteCursorSelectiveIndirection cur target end (repairTraversalCopies traverseEnds mask)
    _ -> ext

recordTraversal :: M.Map Var Var -> L3.Exp3 -> M.Map Var Var
recordTraversal traverseEnds ex =
  case ex of
    L3.AppE fn _ _ (L3.VarE endVar : L3.VarE curVar : _)
      | L.isPrefixOf "_traverse_" (fromVar fn) -> M.insert endVar curVar traverseEnds
    _ -> traverseEnds

postCallInstalls :: FunShapes -> M.Map Var L3.Ty3 -> L3.Exp3 -> PassM [Bind3]
postCallInstalls funShapes env rhs =
  case rhs of
    L3.AppE fn _ _ args ->
      concat <$> mapM installForCopyCandidate (callCursorArrayCopies funShapes env fn args)
    _ -> pure []

callCursorArrayCopies :: FunShapes -> M.Map Var L3.Ty3 -> Var -> [L3.Exp3] -> [CursorArrayCopy]
callCursorArrayCopies funShapes env fn args =
  case M.lookup fn funShapes of
    Just MutableCallShape{mcsOutputEndIx, mcsOutputCurrentIx, mcsCursorArrayTy, mcsInstallAfterCall = True} ->
      copyFromIxs (Just mcsCursorArrayTy) mcsOutputEndIx mcsOutputCurrentIx
    Just MutableCallShape{mcsInstallAfterCall = False} ->
      []
    Nothing ->
      case cursorArrayArgVars of
        -- Fully factored mutable producer/transformer calls have this shape at
        -- the call site even when the function type has been simplified enough
        -- that the shape table misses it:
        -- input ends, output owner/end, output current, input current.
        (_ : (outEndIx, _, ty) : (outCurIx, _, _) : _ : _) ->
          copyFromIxs (Just ty) outEndIx outCurIx
        _ -> []
  where
    argVarAt fallbackTy ix =
      case drop ix args of
        L3.VarE v : _ ->
          case M.lookup v env of
            Just ty | isCursorArrayTy ty -> Just (v, ty)
            _ -> (v,) <$> fallbackTy
        _ -> Nothing

    cursorArrayArgVars =
      [ (ix, v, ty)
      | (ix, L3.VarE v) <- zip [0..] args
      , Just ty <- [M.lookup v env]
      , isCursorArrayTy ty
      ]

    copyFromIxs fallbackTy outEndIx outCurIx =
      case (argVarAt fallbackTy outEndIx, argVarAt fallbackTy outCurIx) of
        (Just (dst, _), Just (src, ty))
          | dst /= src -> [CursorArrayCopy dst src ty]
        _ -> []

mutableCallShape :: L3.FunDef3 -> Maybe MutableCallShape
mutableCallShape FunDef{funTy} =
  case cursorArrayArgs (fst funTy) of
    -- Fully factored mutable producer/transformer calls pass:
    -- input ends, output ends, output current, input current.
    -- Some function types retain additional cursor arrays, so the mutable
    -- producer shape is the leading four cursor-array arguments.
    (_ : (outEndIx, _) : (outCurIx, outCurTy) : _ : _) ->
      Just (MutableCallShape outEndIx outCurIx outCurTy True)
    [(outEndIx, _), (outCurIx, outCurTy)]
      | not (any (hasPacked . L3.stripTyLocs) (fst funTy))
      , L3.stripTyLocs (snd funTy) == L3.ProdTy [] ->
          Just (MutableCallShape outEndIx outCurIx outCurTy False)
    _ -> Nothing

cursorArrayArgs :: [L3.Ty3] -> [(Int, L3.Ty3)]
cursorArrayArgs tys =
  [ (ix, ty) | (ix, ty) <- zip [0..] tys, isCursorArrayTy ty ]

installForCopyCandidate :: CursorArrayCopy -> PassM [Bind3]
installForCopyCandidate (CursorArrayCopy dst src ty) = do
  void <- gensym "install_future_cursor"
  pure [(void, [], L3.ProdTy [], L3.Ext $ L3.MemCpy dst src ty)]

isCursorArrayTy :: L3.Ty3 -> Bool
isCursorArrayTy L3.CursorArrayTy{} = True
isCursorArrayTy _ = False
