-- | Fuse loopified scalar-buffer loops after selective buffer sharing.
--
-- `LoopifyTraversals` intentionally emits one chunk loop per scalar buffer.
-- That gives `SelectiveBufferSharing` a simple, local shape: any pure-copy
-- buffer can be replaced by one selective-indirection wrapper and its whole
-- loop can disappear.  This pass runs after selective sharing and fuses only
-- the remaining loopified scalar loops that still sit next to each other and
-- carry the same constructor label in their generated loop name.
--
-- The pass is conservative.  If the generated loop body no longer has the
-- expected chunk-loop shape, it leaves the loops alone.
--
-- Note [What makes fusing two chunk loops sound]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 'fuseGroup' below keeps ONE representative loop's while condition, for-bound
-- and chunk branch, and runs every participating loop's body underneath them.
-- The representative's per-chunk trip count therefore drives every buffer in
-- the group.  The invariant that has to hold:
--
--   A fused loop may use one representative trip count only if every
--   participating buffer has the same logical element count in that physical
--   chunk.
--
-- Matching datatype, scalar type, total input length or cursor-array length is
-- NOT sufficient, and neither is "the loops were next to each other".
--
-- The equivalence relation this pass actually uses is: same constructor key
-- parsed out of the generated loop variable's name, and adjacency.  Same
-- constructor is the right relation, and it is sound for two reasons that both
-- have to stay true:
--
--   (1) Every occurrence of constructor K writes exactly one element to each of
--       K's scalar buffers, so K's buffers agree on the logical count for any
--       set of nodes.
--   (2) Chunk boundaries stay aligned across peer buffers.  'BoundsCheckVector'
--       lowers to one disjunction over all the checked buffers followed by a
--       'gib_grow_region' for EVERY one of them
--       (@Gibbon.Passes.Codegen@, @BoundsCheckVector@ case), so when any buffer
--       would overflow they all take a redirection together.  Two buffers of
--       the same constructor therefore split the same nodes into the same
--       chunks even when their element widths differ, which is why fusing an
--       Int8 buffer with an Int32 buffer of the same constructor is fine.
--
-- Different constructors have unrelated per-chunk counts and must never be
-- fused.  That makes the /injectivity/ of the name key a correctness
-- requirement, not a cosmetic one -- see
-- Note [The loop-name constructor key must be injective] in
-- "Gibbon.Passes.LoopifyTraversals", which records the miscompilation that a
-- non-injective key produced.
--
-- The dcon (tag) stream loop is deliberately outside this relation: its footer
-- count is the total number of tags in the chunk, not any one constructor's
-- count.  It is excluded structurally rather than by a check -- the loopifier
-- names it without a @_dcon_<key>_loop@ segment, so 'scalarLoopDCon' returns
-- 'Nothing' for it and it can never join a group.
module Gibbon.Passes.LoopifiedTraversalFusion
  ( fuseLoopifiedTraversals
  ) where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

import Gibbon.Common
import Gibbon.DynFlags
import Gibbon.Language
import qualified Gibbon.L3.Syntax as L3

type Bind3 = (Var, [()], L3.Ty3, L3.Exp3)

data ChunkLoop = ChunkLoop
  { clWhileCond :: Var
  , clPrefixBinds :: [Bind3]
  , clForVar :: Var
  , clForBound :: L3.Exp3
  , clForBody :: L3.Exp3
  , clBranchCond :: L3.Exp3
  , clThenBody :: L3.Exp3
  , clElseBody :: L3.Exp3
  }

fuseLoopifiedTraversals :: L3.Prog3 -> PassM L3.Prog3
fuseLoopifiedTraversals prog@Prog{fundefs} = do
  dflags <- getDynFlags
  let enabled = gopt Opt_EnableLoopFusion dflags
      loopificationOn = gopt Opt_EnableLoopification dflags || gopt Opt_AutoLoopification dflags
  if enabled && not loopificationOn
    then error $
      "fuseLoopifiedTraversals: --opt-loop-fusion is enabled, but neither " ++
      "--opt-loopification nor --auto-loopification is.\n" ++
      "Loop fusion only ever fuses functions loopification already rewrote, " ++
      "so it has nothing to do without it.\n" ++
      "Add --opt-loopification (with --store-scalar-field-counts) or " ++
      "--auto-loopification to the compile command."
    else do
      fds' <-
        if enabled
        then mapM fuseFun (M.elems fundefs)
        else pure (M.elems fundefs)
      pure $ prog { fundefs = M.fromList [ (funName f, f) | f <- fds' ] }

fuseFun :: L3.FunDef3 -> PassM L3.FunDef3
fuseFun fn@FunDef{funMeta, funBody}
  | Loopified `notElem` funOpt funMeta = pure fn
  | otherwise = do
      body' <- fuseBody funBody
      pure $ fn { funBody = body' }

fuseBody :: L3.Exp3 -> PassM L3.Exp3
fuseBody ex = do
  let (binds, tailExp) = unLets3 ex
  binds' <- fuseBinds binds
  pure $ L3.mkLets binds' tailExp

fuseBinds :: [Bind3] -> PassM [Bind3]
fuseBinds [] = pure []
fuseBinds (b:bs) =
  case scalarLoopDCon b of
    Nothing -> (b :) <$> fuseBinds bs
    Just dconKey -> do
      let (grp, rest) = span ((== Just dconKey) . scalarLoopDCon) (b:bs)
      fusedOrOriginal <-
        if length grp < 2
        then pure grp
        else fuseGroup dconKey grp
      (fusedOrOriginal ++) <$> fuseBinds rest

fuseGroup :: String -> [Bind3] -> PassM [Bind3]
fuseGroup dconKey binds =
  case traverse parseChunkLoop binds of
    Nothing -> pure binds
    Just [] -> pure binds
    Just [_] -> pure binds
    Just loops@(rep:_) -> do
      fusedVar <- gensym $ toVar ("loopified_fused_" ++ dconKey)
      innerVar <- gensym $ toVar ("loopified_fused_" ++ dconKey ++ "_inner")
      branchVar <- gensym $ toVar ("loopified_fused_" ++ dconKey ++ "_branch")
      fusedForBody <- mkUnitSeq "loopified_fused_body" $
        [ substE (L3.VarE (clForVar loop)) (L3.VarE (clForVar rep)) (clForBody loop)
        | loop <- loops
        ]
      fusedThen <- mkUnitSeq "loopified_fused_then" (map clThenBody loops)
      fusedElse <- mkUnitSeq "loopified_fused_else" (map clElseBody loops)
      let fusedChunk =
            L3.mkLets
              ( concatMap clPrefixBinds loops
                ++ [ (innerVar, [], L3.ProdTy [], L3.Ext $ L3.ForE (clForVar rep) (clForBound rep) fusedForBody)
                   , (branchVar, [], L3.ProdTy [], L3.IfE (clBranchCond rep) fusedThen fusedElse)
                   ]
              )
              (L3.MkProdE [])
      pure [(fusedVar, [], L3.ProdTy [], L3.Ext $ L3.WhileCursor (clWhileCond rep) fusedChunk)]

mkUnitSeq :: String -> [L3.Exp3] -> PassM L3.Exp3
mkUnitSeq prefix bodies = do
  binds <-
    mapM
      (\body -> do
          v <- gensym (toVar prefix)
          pure (v, [], L3.ProdTy [], body))
      bodies
  pure $ L3.mkLets binds (L3.MkProdE [])

parseChunkLoop :: Bind3 -> Maybe ChunkLoop
parseChunkLoop (_, _, _, L3.Ext (L3.WhileCursor cond body)) = do
  let (binds, tailExp) = unLets3 body
  case tailExp of
    L3.MkProdE [] -> pure ()
    _ -> Nothing
  let (prefix, rest1) = break isForBind binds
  (forBind, rest2) <-
    case rest1 of
      x:xs -> Just (x, xs)
      [] -> Nothing
  (forVar, forBound, forBody) <- getFor forBind
  let (between, rest3) = break isIfBind rest2
  (branchBind, rest4) <-
    case rest3 of
      x:xs -> Just (x, xs)
      [] -> Nothing
  if null rest4
    then pure ()
    else Nothing
  (branchCond, thenBody, elseBody) <- getIf branchBind
  pure
    ChunkLoop
      { clWhileCond = cond
      , clPrefixBinds = prefix ++ between
      , clForVar = forVar
      , clForBound = forBound
      , clForBody = forBody
      , clBranchCond = branchCond
      , clThenBody = thenBody
      , clElseBody = elseBody
      }
parseChunkLoop _ = Nothing

isForBind :: Bind3 -> Bool
isForBind (_, _, _, L3.Ext L3.ForE{}) = True
isForBind _ = False

getFor :: Bind3 -> Maybe (Var, L3.Exp3, L3.Exp3)
getFor (_, _, _, L3.Ext (L3.ForE v bound body)) = Just (v, bound, body)
getFor _ = Nothing

isIfBind :: Bind3 -> Bool
isIfBind (_, _, _, L3.IfE{}) = True
isIfBind _ = False

getIf :: Bind3 -> Maybe (L3.Exp3, L3.Exp3, L3.Exp3)
getIf (_, _, _, L3.IfE cond thn els) = Just (cond, thn, els)
getIf _ = Nothing

scalarLoopDCon :: Bind3 -> Maybe String
scalarLoopDCon (v, _, _, L3.Ext L3.WhileCursor{}) =
  parseDConLoopName (fromVar v)
scalarLoopDCon _ = Nothing

-- | Recover the constructor key from a generated scalar chunk-loop name of the
-- form @<seed>_buf<N>_dcon_<key>_loop@.
--
-- @<key>@ is produced by 'Gibbon.Passes.LoopifyTraversals.sanitizeLoopName' and
-- is injective in the constructor name, so equal keys here mean equal
-- constructors -- which is exactly the precondition
-- Note [What makes fusing two chunk loops sound] needs.  The earliest
-- @_dcon_@ in the name is the separator: an escaped @_dcon_@ inside a key is
-- spelled @__dcon__@ and so cannot appear earlier than the real one.
parseDConLoopName :: String -> Maybe String
parseDConLoopName s = do
  rest <- firstJust [ L.stripPrefix "_dcon_" suffix | suffix <- L.tails s ]
  stripSuffix "_loop" rest

stripSuffix :: String -> String -> Maybe String
stripSuffix suffix s =
  let n = length suffix
   in if suffix `L.isSuffixOf` s
      then Just (take (length s - n) s)
      else Nothing

firstJust :: [Maybe a] -> Maybe a
firstJust = safeHead . mapMaybe id
  where
    safeHead [] = Nothing
    safeHead (x:_) = Just x

unLets3 :: L3.Exp3 -> ([Bind3], L3.Exp3)
unLets3 ex =
  case ex of
    L3.LetE b bod ->
      let (bs, tailExp) = unLets3 bod
       in (b : bs, tailExp)
    _ -> ([], ex)
