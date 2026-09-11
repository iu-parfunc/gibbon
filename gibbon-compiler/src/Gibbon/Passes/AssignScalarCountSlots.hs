{-# LANGUAGE TupleSections #-}

-- | Assign deferred scalar-count slots and bracket producer calls.
module Gibbon.Passes.AssignScalarCountSlots
  ( assignScalarCountSlots
  , scalarCountProducers
  ) where

import Data.Functor.Identity (Identity(..))
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import Gibbon.Common
import Gibbon.DynFlags
import Gibbon.L3.Syntax as L3

{-

Note [Deferred scalar counts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A producer annotated @OPT:StoreScalarCounts@ records, per SoA buffer per chunk,
how many elements it wrote there; a loopified consumer reads those counts as its
loop trip counts.  A count that is too large makes the consumer write past the
end of a chunk, so exactness matters more than speed here.

Instead of bumping a footer once per element, the producer increments a global
counter and the batched total is delivered into the correct footer at the two
points where the target can change or is needed:

  per element  : gib_scalar_count_pending[K]++
  at growth    : gib_scalar_count_on_grow flushes before its cyclic transition
  after a call : gib_scalar_count_finalize flushes what is left

Both hooks are required.  A chunk's fill level is only witnessed as it is
abandoned, and a redirection tag in a scalar buffer is byte-indistinguishable
from data, so it cannot be recovered by scanning afterwards.

This counts the same events as the per-element bump and only batches their
delivery, so the footers are identical.  Deriving the count arithmetically
instead -- @(cursor - base) / width@ -- was rejected: it is exact only where the
byte arithmetic holds, and each place it does not (precondition P1 in
LoopifyTraversals, buffers shared by selective buffer sharing, AoS layouts,
the nursery, RAN/indirection tags) fails silently as a short count.

A slot is @base + position@, where @position@ is the buffer's index in the SoA
cursor array (0 = tag buffer, 1.. = field buffers) and @base@ is assigned here
once per producer over the whole program, so two producers cannot collide.

A producer keeps the per-element bump ('noCountSlot') when it cannot be
bracketed: an unrecognized cursor ABI, mutual recursion between producers, or
any call site 'spliceCalls' does not reach.

Note [Coverage must mirror emission]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'rebaseBumps' moves a producer's accounting onto a slot and 'spliceCalls'
brackets its calls.  If the first happens without the second the slot
accumulates and is never flushed, leaving the footers untouched and a consumer
reading a trip count of zero.  So a producer is deferred only when every call
to it is bracketed: 'countCalls' counts all calls (a total traversal -- no
wildcard, so a new constructor breaks the build), 'countBracketableCalls' walks
the shape 'spliceCalls' does, and unequal means demote to 'noCountSlot'.

Note [Deferred scalar counts is incompatible with the generational GC]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A slot caches the @GibRegionInfo *@ of the region it counts for.  Under
@--gen-gc@ regions are nursery-allocated and the copying collector relocates
them and rewrites @reg_info@ (gibbon-rts/rts-ng/src/gc.rs) with no hook into
the C-side slot table, so the binding goes stale and later counts are written
where nothing reads them.  @gib_scalar_count_on_promote@ covers only the
relocation inside @gib_grow_region_on_heap@; a collection can move a region at
any allocation point.  The combination is therefore rejected outright.

-}

-- | A footer with this slot is not on the deferred path and keeps the
-- per-element bump.
noCountSlot :: Int
noCountSlot = -1

-- | Mirrors GIB_SCALAR_COUNT_MAX_SLOTS in gibbon-rts/rts-c/gibbon_rts.h.
maxCountSlots :: Int
maxCountSlots = 256

-- | Where a producer's output cursor arrays sit in its argument list.
data OutShape = OutShape
  { osLen      :: Int   -- ^ cursor-array length == number of SoA buffers
  , osEndArgIx :: Int   -- ^ argument index of the output end-cursor array
  }
  deriving (Eq, Show)

-- | Functions that maintain scalar counts, i.e. whose body contains a
-- 'ScalarCountBump'.
scalarCountProducers :: FunDefs3 -> S.Set Var
scalarCountProducers = M.keysSet . M.filter (hasBump . funBody)

assignScalarCountSlots :: Prog3 -> PassM Prog3
assignScalarCountSlots prg@Prog{ddefs, fundefs, mainExp} = do
  dflags <- getDynFlags
  let wanted = gopt Opt_DeferScalarCounts dflags || gopt Opt_ScalarCountDiff dflags
  -- See Note [Deferred scalar counts is incompatible with the generational GC].
  if wanted && gopt Opt_GenGc dflags
    then error $
      "assignScalarCountSlots: --defer-scalar-counts is not compatible with --gen-gc.\n" ++
      "A deferred slot caches the GibRegionInfo * of the region it counts for, but under\n" ++
      "the generational GC a region is nursery-allocated and the copying collector\n" ++
      "relocates it and rewrites reg_info (gibbon-rts/rts-ng/src/gc.rs), with no hook\n" ++
      "back into the C-side slot table.  The cached binding then goes stale and every\n" ++
      "count after the collection is written to a footer nothing reads -- silently.\n" ++
      "The per-element bump does not have this problem because it re-resolves the\n" ++
      "footer on every element.\n" ++
      "Drop --defer-scalar-counts (counts stay correct, just slower), or drop --gen-gc."
    else if not wanted
    then pure prg
    else do
      let producers = scalarCountProducers fundefs

      -- Mutual recursion between two producers would put a bracket around a
      -- call that is really a recursive step, rebinding (and so zeroing) a
      -- counter that is still accumulating.
      let calleesOf f = maybe S.empty (callees . funBody) (M.lookup f fundefs)
          mutuallyRecursive =
            S.fromList
              [ f
              | f <- S.toList producers
              , g <- S.toList producers
              , f /= g
              , g `S.member` calleesOf f
              , f `S.member` calleesOf g
              ]

      let abiShapes =
            M.fromList
              [ (f, sh)
              | f <- S.toList producers
              , not (f `S.member` mutuallyRecursive)
              , Just fd <- [M.lookup f fundefs]
              , Just sh <- [outShape (funArgs fd) (fst (funTy fd))]
              ]

      -- See Note [Coverage must mirror emission].  A producer is deferred only
      -- if every call to it is one spliceCalls will bracket; a call it cannot
      -- reach would leave that production's counts unflushed and silently
      -- zero.  A producer's own recursive calls are excluded -- those are
      -- deliberately not bracketed.
      let bodies = [ (Just f, funBody fd) | (f, fd) <- M.toList fundefs ]
                     ++ maybe [] (\(e, _) -> [(Nothing, e)]) mainExp
          coveredIn f =
            and [ countCalls f e == countBracketableCalls f e
                | (owner, e) <- bodies, owner /= Just f ]
          shapes = M.filterWithKey (\f _ -> coveredIn f) abiShapes

      let bases = M.fromList (go 0 (L.sort (M.keys shapes)))
            where
              go _ [] = []
              go next (f:fs) =
                let n = maybe 1 osLen (M.lookup f shapes)
                 in (f, next) : go (next + n) fs
          totalSlots = sum [ osLen sh | sh <- M.elems shapes ]

      if totalSlots > maxCountSlots
        then error $
               "assignScalarCountSlots: this program needs " ++ show totalSlots ++
               " deferred scalar-count slots, but the RTS provides " ++
               show maxCountSlots ++ " (GIB_SCALAR_COUNT_MAX_SLOTS). Raise it, " ++
               "or compile without --defer-scalar-counts."
        else do
          let bracketed = M.keysSet bases
          fundefs' <-
            M.traverseWithKey
              (\f fd -> do
                  -- A producer's own body must not bracket its recursive
                  -- calls: the bind belongs at the OUTERMOST call, once per
                  -- production.
                  bod <- spliceCalls bases shapes (S.delete f bracketed)
                           (rebaseBumps (M.lookup f bases) (funBody fd))
                  pure fd { funBody = bod })
              fundefs
          mainExp' <-
            case mainExp of
              Nothing -> pure Nothing
              Just (e, t) -> do
                e' <- spliceCalls bases shapes bracketed e
                pure (Just (e', t))
          pure prg { ddefs = ddefs, fundefs = fundefs', mainExp = mainExp' }

-- | Rewrite a producer's bump slots from cursor-array positions to absolute
-- slots.  Without a base the function keeps the bump ('noCountSlot').
rebaseBumps :: Maybe Int -> Exp3 -> Exp3
rebaseBumps mbase = go
  where
    go ex =
      case ex of
        LetE (v, locs, ty, rhs) bod -> LetE (v, locs, ty, go rhs) (go bod)
        IfE a b c -> IfE (go a) (go b) (go c)
        CaseE scrt brs -> CaseE (go scrt) [ (dc, vs, go rhs) | (dc, vs, rhs) <- brs ]
        MkProdE ls -> MkProdE (L.map go ls)
        ProjE i e -> ProjE i (go e)
        PrimAppE p args -> PrimAppE p (L.map go args)
        TimeIt e t b -> TimeIt (go e) t b
        WithArenaE v e -> WithArenaE v (go e)
        SpawnE f locs args -> SpawnE f locs (L.map go args)
        AppE f rt locs args -> AppE f rt locs (L.map go args)
        DataConE l dc args -> DataConE l dc (L.map go args)
        MapE (v, t, rhs) bod -> MapE (v, t, go rhs) (go bod)
        FoldE (v1,t1,r1) (v2,t2,r2) bod -> FoldE (v1,t1,go r1) (v2,t2,go r2) (go bod)
        Ext (ScalarCountBump dcon footers) ->
          Ext $ ScalarCountBump dcon
            [ (v, maybe noCountSlot (+ pos) mbase) | (v, pos) <- footers ]
        Ext ext -> Ext (mapExtExps go ext)
        VarE{} -> ex
        LitE{} -> ex
        CharE{} -> ex
        FloatE{} -> ex
        LitSymE{} -> ex
        SyncE -> ex

-- | Wrap every call to a bracketed producer in bind/finalize.  The binders are
-- gensym'd: fixed names are deleted by 'OptimizeL3.removeReDefsExp' when they
-- recur in a scope, which silently unbrackets every call site after the first.
spliceCalls :: M.Map Var Int -> M.Map Var OutShape -> S.Set Var -> Exp3 -> PassM Exp3
spliceCalls bases shapes bracketed = go
  where
    go ex =
      case ex of
        LetE (v, locs, ty, rhs) bod -> do
          rhs' <- go rhs
          bod' <- go bod
          case bracketFor bases shapes bracketed rhs' of
            Nothing -> pure $ LetE (v, locs, ty, rhs') bod'
            Just (base, len, ends) -> do
              bindV <- gensym "scalar_count_bind"
              finV <- gensym "scalar_count_fin"
              -- The region is allocated by the caller, so `ends` already names
              -- live footers here; and the finalize must precede any
              -- ScalarCountCopyAll the propagation pass put in `bod`, which it
              -- does because that pass runs earlier and so its copy is inside
              -- `bod`.
              pure $
                LetE (bindV, [], ProdTy [], Ext $ ScalarCountBind base len ends) $
                  LetE (v, locs, ty, rhs') $
                    LetE (finV, [], ProdTy [], Ext $ ScalarCountFinalize base len ends)
                      bod'
        IfE a b c -> IfE <$> go a <*> go b <*> go c
        CaseE scrt brs ->
          CaseE <$> go scrt
                <*> mapM (\(dc, vs, rhs) -> (dc, vs,) <$> go rhs) brs
        MkProdE ls -> MkProdE <$> mapM go ls
        ProjE i e -> ProjE i <$> go e
        PrimAppE p args -> PrimAppE p <$> mapM go args
        TimeIt e t b -> (\e' -> TimeIt e' t b) <$> go e
        WithArenaE v e -> WithArenaE v <$> go e
        SpawnE f locs args -> SpawnE f locs <$> mapM go args
        AppE f rt locs args -> AppE f rt locs <$> mapM go args
        DataConE l dc args -> DataConE l dc <$> mapM go args
        MapE (v, t, rhs) bod -> MapE <$> ((v, t,) <$> go rhs) <*> go bod
        FoldE (v1,t1,r1) (v2,t2,r2) bod ->
          FoldE <$> ((v1,t1,) <$> go r1) <*> ((v2,t2,) <$> go r2) <*> go bod
        Ext ext -> Ext <$> traverseExtExps go ext
        VarE{} -> pure ex
        LitE{} -> pure ex
        CharE{} -> pure ex
        FloatE{} -> pure ex
        LitSymE{} -> pure ex
        SyncE -> pure ex

bracketFor
  :: M.Map Var Int -> M.Map Var OutShape -> S.Set Var -> Exp3 -> Maybe (Int, Int, Var)
bracketFor bases shapes bracketed rhs =
  case rhs of
    AppE fn _ _ args
      | fn `S.member` bracketed
      , Just base <- M.lookup fn bases
      , Just sh <- M.lookup fn shapes
      , Just ends <- argVarAt (osEndArgIx sh) args -> Just (base, osLen sh, ends)
    _ -> Nothing
  where
    argVarAt ix as =
      case drop ix as of
        VarE v : _ -> Just v
        _ -> Nothing

-- | Every call to @f@, anywhere.  Pairs with 'countBracketableCalls'; see
-- Note [Coverage must mirror emission].
countCalls :: Var -> Exp3 -> Int
countCalls f = go
  where
    go ex =
      case ex of
        AppE g _ _ args -> (if g == f then 1 else 0) + sum (L.map go args)
        SpawnE g _ args -> (if g == f then 1 else 0) + sum (L.map go args)
        LetE (_, _, _, rhs) bod -> go rhs + go bod
        IfE a b c -> go a + go b + go c
        CaseE scrt brs -> go scrt + sum [ go rhs | (_, _, rhs) <- brs ]
        MkProdE ls -> sum (L.map go ls)
        ProjE _ e -> go e
        PrimAppE _ args -> sum (L.map go args)
        TimeIt e _ _ -> go e
        WithArenaE _ e -> go e
        DataConE _ _ args -> sum (L.map go args)
        MapE (_, _, rhs) bod -> go rhs + go bod
        FoldE (_,_,r1) (_,_,r2) bod -> go r1 + go r2 + go bod
        Ext ext -> sum (L.map go (extExps ext))
        VarE{} -> 0
        LitE{} -> 0
        CharE{} -> 0
        FloatE{} -> 0
        LitSymE{} -> 0
        SyncE -> 0

-- | Calls to @f@ that 'spliceCalls' will actually bracket: an 'AppE' sitting
-- directly in a 'LetE' right-hand side, reached along the same traversal.
countBracketableCalls :: Var -> Exp3 -> Int
countBracketableCalls f = go
  where
    isCall (AppE g _ _ _) = g == f
    isCall _ = False

    go ex =
      case ex of
        LetE (_, _, _, rhs) bod ->
          (if isCall rhs then 1 else 0) + go rhs + go bod
        IfE a b c -> go a + go b + go c
        CaseE scrt brs -> go scrt + sum [ go rhs | (_, _, rhs) <- brs ]
        MkProdE ls -> sum (L.map go ls)
        ProjE _ e -> go e
        PrimAppE _ args -> sum (L.map go args)
        TimeIt e _ _ -> go e
        WithArenaE _ e -> go e
        SpawnE _ _ args -> sum (L.map go args)
        AppE _ _ _ args -> sum (L.map go args)
        DataConE _ _ args -> sum (L.map go args)
        MapE (_, _, rhs) bod -> go rhs + go bod
        FoldE (_,_,r1) (_,_,r2) bod -> go r1 + go r2 + go bod
        Ext ext -> sum (L.map go (extExps ext))
        VarE{} -> 0
        LitE{} -> 0
        CharE{} -> 0
        FloatE{} -> 0
        LitSymE{} -> 0
        SyncE -> 0

-- | Recognize where a producer's output end-cursor array sits.  Two shapes
-- occur: a producer that builds from scratch takes just its own output arrays,
-- and one that also walks a packed input takes the input's as well.  Anything
-- else -- including a single-buffer (tag-only) layout, which is not SoA -- is
-- left on the bump rather than guessed at.
outShape :: [Var] -> [Ty3] -> Maybe OutShape
outShape args tys =
  case cursorArrays of
    -- [outEnd, outCur]
    [(endIx, n1), (_, n2)]
      | n1 == n2 && n1 > 1 -> Just (OutShape n1 endIx)
    -- [inEnd, outEnd, outCur, inCur]
    [_, (endIx, n2), (_, n3), _]
      | n2 == n3 && n2 > 1 -> Just (OutShape n2 endIx)
    _ -> Nothing
  where
    cursorArrays = [ (ix, n) | (ix, (_, CursorArrayTy n)) <- zip [0..] (zip args tys) ]

hasBump :: Exp3 -> Bool
hasBump = go
  where
    go ex =
      case ex of
        Ext ScalarCountBump{} -> True
        LetE (_, _, _, rhs) bod -> go rhs || go bod
        IfE a b c -> any go [a, b, c]
        CaseE scrt brs -> go scrt || any (\(_, _, rhs) -> go rhs) brs
        MkProdE ls -> any go ls
        ProjE _ e -> go e
        PrimAppE _ args -> any go args
        TimeIt e _ _ -> go e
        WithArenaE _ e -> go e
        SpawnE _ _ args -> any go args
        AppE _ _ _ args -> any go args
        DataConE _ _ args -> any go args
        MapE (_, _, rhs) bod -> go rhs || go bod
        FoldE (_,_,r1) (_,_,r2) bod -> go r1 || go r2 || go bod
        Ext ext -> any go (extExps ext)
        VarE{} -> False
        LitE{} -> False
        CharE{} -> False
        FloatE{} -> False
        LitSymE{} -> False
        SyncE -> False

callees :: Exp3 -> S.Set Var
callees = go
  where
    go ex =
      case ex of
        AppE f _ _ args -> S.insert f (S.unions (L.map go args))
        SpawnE f _ args -> S.insert f (S.unions (L.map go args))
        LetE (_, _, _, rhs) bod -> go rhs `S.union` go bod
        IfE a b c -> S.unions [go a, go b, go c]
        CaseE scrt brs -> S.unions (go scrt : [ go rhs | (_, _, rhs) <- brs ])
        MkProdE ls -> S.unions (L.map go ls)
        ProjE _ e -> go e
        PrimAppE _ args -> S.unions (L.map go args)
        TimeIt e _ _ -> go e
        WithArenaE _ e -> go e
        DataConE _ _ args -> S.unions (L.map go args)
        MapE (_, _, rhs) bod -> go rhs `S.union` go bod
        FoldE (_,_,r1) (_,_,r2) bod -> S.unions [go r1, go r2, go bod]
        Ext ext -> S.unions (L.map go (extExps ext))
        VarE{} -> S.empty
        LitE{} -> S.empty
        CharE{} -> S.empty
        FloatE{} -> S.empty
        LitSymE{} -> S.empty
        SyncE -> S.empty

-- | The expression children of an extension node.
--
-- Deliberately a TOTAL case with no wildcard: a traversal here that silently
-- skips a form is how a producer call escapes bracketing, and how a
-- 'ScalarCountBump' keeps an un-rebased slot and starts incrementing another
-- producer's counter.  A new constructor must break this build.
extExps :: E3Ext () Ty3 -> [Exp3]
extExps ext =
  case ext of
    WriteScalar _ _ e -> [e]
    WriteTagPacked _ e -> [e]
    WriteCursorSelectiveIndirection _ _ _ e -> [e]
    WriteTaggedCursor _ e -> [e]
    WriteCursorMutable _ e -> [e]
    WriteList _ e _ -> [e]
    WriteVector _ e _ -> [e]
    AddCursor _ e -> [e]
    BumpCursorMutable _ e -> [e]
    AddrOfCursor e -> [e]
    RetE es -> es
    LetAvail _ e -> [e]
    ForE _ e1 e2 -> [e1, e2]
    WhileCursor _ e -> [e]
    WhileCursorEnd _ _ e -> [e]
    VecBroadcast _ _ e -> [e]
    VecAdd _ _ a b -> [a, b]
    VecSub _ _ a b -> [a, b]
    VecMul _ _ a b -> [a, b]
    VecDiv _ _ a b -> [a, b]
    VecMod _ _ a b -> [a, b]
    VecCmp _ _ _ a b -> [a, b]
    VecSelect _ _ a b c -> [a, b, c]
    VecStore _ _ _ e -> [e]
    Assert e -> [e]
    ReadScalar{} -> []
    ReadTag{} -> []
    WriteTag{} -> []
    TagCursor{} -> []
    WriteCursorIndirection{} -> []
    UnwrapSelectiveIndirections{} -> []
    MemCpy{} -> []
    ReadTaggedCursor{} -> []
    ReadCursor{} -> []
    GrowRegion{} -> []
    ReadList{} -> []
    ReadVector{} -> []
    MakeCursorArray{} -> []
    IndexCursorArray{} -> []
    DerefMutCursor{} -> []
    CastPtr{} -> []
    SubPtr{} -> []
    NewBuffer{} -> []
    ScopedBuffer{} -> []
    NewParBuffer{} -> []
    ScopedParBuffer{} -> []
    EndOfBuffer{} -> []
    MMapFileSize{} -> []
    SizeOfPacked{} -> []
    SizeOfScalar{} -> []
    BoundsCheck{} -> []
    BoundsCheckVector{} -> []
    IndirectionBarrier{} -> []
    BumpArenaRefCount{} -> []
    NullCursor -> []
    InitCursor{} -> []
    GetCilkWorkerNum -> []
    AllocateTagHere{} -> []
    AllocateScalarsHere{} -> []
    StartTagAllocation{} -> []
    EndTagAllocation{} -> []
    StartScalarsAllocation{} -> []
    EndScalarsAllocation{} -> []
    ScalarCountBump{} -> []
    ScalarCountBind{} -> []
    ScalarCountFinalize{} -> []
    ScalarCountSet{} -> []
    ScalarCountCopyAll{} -> []
    ReadScalarCount{} -> []
    ReadScalarCountFirstFooter{} -> []
    ReadScalarCountNextFooter{} -> []
    VecLoad{} -> []
    SSPush{} -> []
    SSPop{} -> []

-- | Rebuild an extension node with its expression children mapped.  Kept
-- beside 'extExps' so the two cannot drift.
mapExtExps :: (Exp3 -> Exp3) -> E3Ext () Ty3 -> E3Ext () Ty3
mapExtExps f ext = runIdentity (traverseExtExps (Identity . f) ext)

traverseExtExps
  :: Applicative m => (Exp3 -> m Exp3) -> E3Ext () Ty3 -> m (E3Ext () Ty3)
traverseExtExps f ext =
  case ext of
    WriteScalar s v e -> WriteScalar s v <$> f e
    WriteTagPacked v e -> WriteTagPacked v <$> f e
    WriteCursorSelectiveIndirection a b c e -> WriteCursorSelectiveIndirection a b c <$> f e
    WriteTaggedCursor v e -> WriteTaggedCursor v <$> f e
    WriteCursorMutable v e -> WriteCursorMutable v <$> f e
    WriteList v e t -> (\e' -> WriteList v e' t) <$> f e
    WriteVector v e t -> (\e' -> WriteVector v e' t) <$> f e
    AddCursor v e -> AddCursor v <$> f e
    BumpCursorMutable v e -> BumpCursorMutable v <$> f e
    AddrOfCursor e -> AddrOfCursor <$> f e
    RetE es -> RetE <$> traverse f es
    LetAvail vs e -> LetAvail vs <$> f e
    ForE v e1 e2 -> ForE v <$> f e1 <*> f e2
    WhileCursor v e -> WhileCursor v <$> f e
    WhileCursorEnd v w e -> WhileCursorEnd v w <$> f e
    VecBroadcast s n e -> VecBroadcast s n <$> f e
    VecAdd s n a b -> VecAdd s n <$> f a <*> f b
    VecSub s n a b -> VecSub s n <$> f a <*> f b
    VecMul s n a b -> VecMul s n <$> f a <*> f b
    VecDiv s n a b -> VecDiv s n <$> f a <*> f b
    VecMod s n a b -> VecMod s n <$> f a <*> f b
    VecCmp s n o a b -> VecCmp s n o <$> f a <*> f b
    VecSelect s n a b c -> VecSelect s n <$> f a <*> f b <*> f c
    VecStore s n v e -> VecStore s n v <$> f e
    Assert e -> Assert <$> f e
    ReadScalar{} -> pure ext
    ReadTag{} -> pure ext
    WriteTag{} -> pure ext
    TagCursor{} -> pure ext
    WriteCursorIndirection{} -> pure ext
    UnwrapSelectiveIndirections{} -> pure ext
    MemCpy{} -> pure ext
    ReadTaggedCursor{} -> pure ext
    ReadCursor{} -> pure ext
    GrowRegion{} -> pure ext
    ReadList{} -> pure ext
    ReadVector{} -> pure ext
    MakeCursorArray{} -> pure ext
    IndexCursorArray{} -> pure ext
    DerefMutCursor{} -> pure ext
    CastPtr{} -> pure ext
    SubPtr{} -> pure ext
    NewBuffer{} -> pure ext
    ScopedBuffer{} -> pure ext
    NewParBuffer{} -> pure ext
    ScopedParBuffer{} -> pure ext
    EndOfBuffer{} -> pure ext
    MMapFileSize{} -> pure ext
    SizeOfPacked{} -> pure ext
    SizeOfScalar{} -> pure ext
    BoundsCheck{} -> pure ext
    BoundsCheckVector{} -> pure ext
    IndirectionBarrier{} -> pure ext
    BumpArenaRefCount{} -> pure ext
    NullCursor -> pure ext
    InitCursor{} -> pure ext
    GetCilkWorkerNum -> pure ext
    AllocateTagHere{} -> pure ext
    AllocateScalarsHere{} -> pure ext
    StartTagAllocation{} -> pure ext
    EndTagAllocation{} -> pure ext
    StartScalarsAllocation{} -> pure ext
    EndScalarsAllocation{} -> pure ext
    ScalarCountBump{} -> pure ext
    ScalarCountBind{} -> pure ext
    ScalarCountFinalize{} -> pure ext
    ScalarCountSet{} -> pure ext
    ScalarCountCopyAll{} -> pure ext
    ReadScalarCount{} -> pure ext
    ReadScalarCountFirstFooter{} -> pure ext
    ReadScalarCountNextFooter{} -> pure ext
    VecLoad{} -> pure ext
    SSPush{} -> pure ext
    SSPop{} -> pure ext
