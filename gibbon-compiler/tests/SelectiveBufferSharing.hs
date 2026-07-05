{-# LANGUAGE TemplateHaskell #-}

module SelectiveBufferSharing
  ( selectiveBufferSharingTests
  ) where

import qualified Data.Map as M

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Gibbon.Common
import Gibbon.DynFlags
import Gibbon.Language
import qualified Gibbon.L3.Syntax as L3
import Gibbon.Passes.SelectiveBufferSharing

runnerEnabled :: L3.Prog3 -> L3.Prog3
runnerEnabled prg =
  fst $
    runPassM
      (defaultConfig {dynflags = gopt_set Opt_EnableSelectiveBufferSharing (dynflags defaultConfig)})
      0
      (selectiveBufferSharing prg)

runnerDisabled :: L3.Prog3 -> L3.Prog3
runnerDisabled prg =
  fst $ defaultPackedRunPassM $ selectiveBufferSharing prg

loopifiedProg :: L3.Prog3
loopifiedProg =
  Prog
    M.empty
    (M.fromList [("loopifiedMap", loopifiedFun)])
    Nothing

loopifiedFun :: L3.FunDef3
loopifiedFun =
  FunDef
    "loopifiedMap"
    []
    ([], L3.ProdTy [])
    loopifiedBody
    (FunMeta TailRec NoInline False [CanVectorize])

loopifiedProducerFun :: L3.FunDef3
loopifiedProducerFun =
  FunDef
    "producer"
    ["inEnds", "outEnds", "outCurs", "inCurs"]
    ( replicate 4 (L3.CursorArrayTy 3)
    , L3.ProdTy []
    )
    loopifiedBody
    (FunMeta TailRec NoInline False [CanVectorize])

consumerFun :: L3.FunDef3
consumerFun =
  FunDef
    "consumer"
    ["ends", "curs"]
    ([L3.CursorArrayTy 3, L3.CursorArrayTy 3], L3.IntTy)
    (L3.LitE 1)
    (FunMeta TailRec NoInline False [])

callSiteProg :: L3.Prog3
callSiteProg =
  Prog
    M.empty
    (M.fromList [("producer", loopifiedProducerFun), ("consumer", consumerFun)])
    (Just (callSiteMain, L3.IntTy))

callSiteMain :: L3.Exp3
callSiteMain =
  -- This mimics the benchmark pipeline shape:
  -- a loopified map writes selective wrappers into its output SoA cursor
  -- arrays, and main passes a copied output start cursor array to a fold via
  -- an inline let-expression. The unwrap should be hoisted to that call site,
  -- not inserted inside the fold body where recursive calls would repeat it.
  L3.mkLets
    [ ("produce", [], L3.ProdTy [], L3.TimeIt (L3.AppE "producer" UnknownTailType [] [L3.VarE "inEnds", L3.VarE "outEnds", L3.VarE "outCurs", L3.VarE "inCurs"]) (L3.ProdTy []) False)
    , ("consume", [], L3.IntTy, L3.TimeIt (L3.AppE "consumer" UnknownTailType [] [L3.VarE "outEnds", inlineCopiedCursorArg "copyCurs" "outCurs"]) L3.IntTy False)
    ]
    (L3.VarE "consume")

inlineCopiedCursorArg :: Var -> Var -> L3.Exp3
inlineCopiedCursorArg dst src =
  L3.mkLets
    [ (dst, [], L3.CursorArrayTy 3, L3.Ext $ L3.InitCursor (L3.CursorArrayTy 3))
    , (toVar (fromVar dst ++ "_memcpy"), [], L3.ProdTy [], L3.Ext $ L3.MemCpy dst src (L3.CursorArrayTy 3))
    ]
    (L3.VarE dst)

loopifiedBody :: L3.Exp3
loopifiedBody =
  L3.mkLets
    (concatMap preludeFor [0, 1, 2] ++ [dconLoop, copyLoop, mutateLoop])
    (L3.MkProdE [])

preludeFor :: Int -> [(Var, [()], L3.Ty3, L3.Exp3)]
preludeFor ix =
  let pfx = "loop_probe_buf" ++ show ix
   in [ (toVar (pfx ++ "_input_end"), [], L3.CursorTy, L3.Ext $ L3.IndexCursorArray "inEnds" ix)
      , (toVar (pfx ++ "_in_loc"), [], L3.MutCursorTy, L3.Ext $ L3.AddrOfCursor (L3.Ext $ L3.IndexCursorArray "inCurs" ix))
      , (toVar (pfx ++ "_out_loc"), [], L3.MutCursorTy, L3.Ext $ L3.AddrOfCursor (L3.Ext $ L3.IndexCursorArray "outCurs" ix))
      ]

dconLoop :: (Var, [()], L3.Ty3, L3.Exp3)
dconLoop =
  ( "loop_probe_buf0_loop"
  , []
  , L3.ProdTy []
  , L3.Ext $ L3.WhileCursor "loop_probe_buf0_count_footer_loc" $
      L3.mkLets
        [("loop_probe_buf0_inner_loop", [], L3.ProdTy [], L3.Ext $ L3.ForE "i" (L3.LitE 8) dconForBody)]
        (L3.MkProdE [])
  )

dconForBody :: L3.Exp3
dconForBody =
  L3.mkLets
    [("loop_probe_buf0_write_tag", [], L3.CursorTy, L3.Ext $ L3.WriteTagPacked "out_dcon" (L3.LitE 1))]
    (L3.MkProdE [])

copyLoop :: (Var, [()], L3.Ty3, L3.Exp3)
copyLoop =
  ( "loop_probe_buf1_loop"
  , []
  , L3.ProdTy []
  , L3.Ext $ L3.WhileCursor "loop_probe_buf1_count_footer_loc" $
      L3.mkLets
        [("loop_probe_buf1_inner_loop", [], L3.ProdTy [], L3.Ext $ L3.ForE "i" (L3.LitE 8) copyForBody)]
        (L3.MkProdE [])
  )

copyForBody :: L3.Exp3
copyForBody =
  L3.mkLets
    [("loop_probe_buf1_inner_body", [], L3.ProdTy [], copyScalarBody 1)]
    (L3.MkProdE [])

copyScalarBody :: Int -> L3.Exp3
copyScalarBody ix =
  let pfx = "loop_probe_buf" ++ show ix
   in L3.mkLets
        [ (toVar (pfx ++ "_read_pair"), [], L3.ProdTy [L3.IntTy, L3.CursorTy], L3.Ext $ L3.ReadScalar L3.IntS (toVar (pfx ++ "_read_cur")))
        , (toVar (pfx ++ "_read_val"), [], L3.IntTy, L3.ProjE 0 (L3.VarE (toVar (pfx ++ "_read_pair"))))
        , (toVar (pfx ++ "_write_val"), [], L3.CursorTy, L3.Ext $ L3.WriteScalar L3.IntS (toVar (pfx ++ "_write_cur")) (L3.VarE (toVar (pfx ++ "_read_val"))))
        ]
        (L3.MkProdE [])

mutateLoop :: (Var, [()], L3.Ty3, L3.Exp3)
mutateLoop =
  ( "loop_probe_buf2_loop"
  , []
  , L3.ProdTy []
  , L3.Ext $ L3.WhileCursor "loop_probe_buf2_count_footer_loc" $
      L3.mkLets
        [("loop_probe_buf2_inner_loop", [], L3.ProdTy [], L3.Ext $ L3.ForE "i" (L3.LitE 8) mutateForBody)]
        (L3.MkProdE [])
  )

mutateForBody :: L3.Exp3
mutateForBody =
  L3.mkLets
    [("loop_probe_buf2_inner_body", [], L3.ProdTy [], mutateScalarBody)]
    (L3.MkProdE [])

mutateScalarBody :: L3.Exp3
mutateScalarBody =
  L3.mkLets
    [ ("loop_probe_buf2_read_pair", [], L3.ProdTy [L3.IntTy, L3.CursorTy], L3.Ext $ L3.ReadScalar L3.IntS "loop_probe_buf2_read_cur")
    , ("loop_probe_buf2_read_val", [], L3.IntTy, L3.ProjE 0 (L3.VarE "loop_probe_buf2_read_pair"))
    , ("loop_probe_buf2_plus1", [], L3.IntTy, L3.PrimAppE AddP [L3.VarE "loop_probe_buf2_read_val", L3.LitE 1])
    , ("loop_probe_buf2_write_val", [], L3.CursorTy, L3.Ext $ L3.WriteScalar L3.IntS "loop_probe_buf2_write_cur" (L3.VarE "loop_probe_buf2_plus1"))
    ]
    (L3.MkProdE [])

getFunBody :: Var -> L3.Prog3 -> L3.Exp3
getFunBody fn prg =
  case M.lookup fn (fundefs prg) of
    Just FunDef{funBody} -> funBody
    Nothing -> error $ "Missing function in test program: " ++ sdoc fn

countIndirections :: L3.Exp3 -> Int
countIndirections = countExt p
  where
    p L3.WriteCursorSelectiveIndirection{} = True
    p L3.WriteCursorIndirection{} = True
    p _ = False

countSelectiveUnwraps :: L3.Exp3 -> Int
countSelectiveUnwraps = countExt p
  where
    p L3.UnwrapSelectiveIndirections{} = True
    p _ = False

countTimedSelectiveUnwraps :: L3.Exp3 -> Int
countTimedSelectiveUnwraps ex =
  case ex of
    L3.LetE (_, _, _, rhs) bod -> countTimedSelectiveUnwraps rhs + countTimedSelectiveUnwraps bod
    L3.IfE a b c -> sum (map countTimedSelectiveUnwraps [a, b, c])
    L3.CaseE scrt brs -> countTimedSelectiveUnwraps scrt + sum (map (countTimedSelectiveUnwraps . (\(_, _, rhs) -> rhs)) brs)
    L3.AppE _ _ _ args -> sum (map countTimedSelectiveUnwraps args)
    L3.PrimAppE _ args -> sum (map countTimedSelectiveUnwraps args)
    L3.MkProdE ls -> sum (map countTimedSelectiveUnwraps ls)
    L3.ProjE _ e -> countTimedSelectiveUnwraps e
    L3.DataConE _ _ args -> sum (map countTimedSelectiveUnwraps args)
    L3.TimeIt e _ _ -> countSelectiveUnwraps e + countTimedSelectiveUnwraps e
    L3.WithArenaE _ e -> countTimedSelectiveUnwraps e
    L3.SpawnE _ _ args -> sum (map countTimedSelectiveUnwraps args)
    L3.MapE (_, _, e1) e2 -> countTimedSelectiveUnwraps e1 + countTimedSelectiveUnwraps e2
    L3.FoldE (_, _, e1) (_, _, e2) e3 -> sum (map countTimedSelectiveUnwraps [e1, e2, e3])
    L3.Ext ext -> goExt ext
    _ -> 0
  where
    goExt ext =
      case ext of
        L3.ForE _ bound bod -> countTimedSelectiveUnwraps bound + countTimedSelectiveUnwraps bod
        L3.WhileCursor _ bod -> countTimedSelectiveUnwraps bod
        L3.WriteScalar _ _ rhs -> countTimedSelectiveUnwraps rhs
        L3.WriteTagPacked _ rhs -> countTimedSelectiveUnwraps rhs
        L3.WriteTaggedCursor _ rhs -> countTimedSelectiveUnwraps rhs
        L3.WriteCursorMutable _ rhs -> countTimedSelectiveUnwraps rhs
        L3.WriteList _ rhs _ -> countTimedSelectiveUnwraps rhs
        L3.WriteVector _ rhs _ -> countTimedSelectiveUnwraps rhs
        L3.AddCursor _ rhs -> countTimedSelectiveUnwraps rhs
        L3.BumpCursorMutable _ rhs -> countTimedSelectiveUnwraps rhs
        L3.AddrOfCursor rhs -> countTimedSelectiveUnwraps rhs
        L3.LetAvail _ bod -> countTimedSelectiveUnwraps bod
        L3.Assert rhs -> countTimedSelectiveUnwraps rhs
        _ -> 0

countWhileCursors :: L3.Exp3 -> Int
countWhileCursors = countExt p
  where
    p L3.WhileCursor{} = True
    p _ = False

countExt :: (L3.E3Ext () L3.Ty3 -> Bool) -> L3.Exp3 -> Int
countExt p ex =
  case ex of
    L3.LetE (_, _, _, rhs) bod -> countExt p rhs + countExt p bod
    L3.IfE a b c -> sum (map (countExt p) [a, b, c])
    L3.CaseE scrt brs -> countExt p scrt + sum (map (countExt p . (\(_, _, rhs) -> rhs)) brs)
    L3.AppE _ _ _ args -> sum (map (countExt p) args)
    L3.PrimAppE _ args -> sum (map (countExt p) args)
    L3.MkProdE ls -> sum (map (countExt p) ls)
    L3.ProjE _ e -> countExt p e
    L3.DataConE _ _ args -> sum (map (countExt p) args)
    L3.TimeIt e _ _ -> countExt p e
    L3.WithArenaE _ e -> countExt p e
    L3.SpawnE _ _ args -> sum (map (countExt p) args)
    L3.MapE (_, _, e1) e2 -> countExt p e1 + countExt p e2
    L3.FoldE (_, _, e1) (_, _, e2) e3 -> sum (map (countExt p) [e1, e2, e3])
    L3.Ext ext
      | p ext -> 1 + goExt ext
      | otherwise -> goExt ext
    _ -> 0
  where
    goExt ext =
      case ext of
        L3.ForE _ bound bod -> countExt p bound + countExt p bod
        L3.WhileCursor _ bod -> countExt p bod
        L3.WriteScalar _ _ rhs -> countExt p rhs
        L3.WriteTagPacked _ rhs -> countExt p rhs
        L3.WriteTaggedCursor _ rhs -> countExt p rhs
        L3.WriteCursorMutable _ rhs -> countExt p rhs
        L3.WriteList _ rhs _ -> countExt p rhs
        L3.WriteVector _ rhs _ -> countExt p rhs
        L3.AddCursor _ rhs -> countExt p rhs
        L3.BumpCursorMutable _ rhs -> countExt p rhs
        L3.AddrOfCursor rhs -> countExt p rhs
        L3.LetAvail _ bod -> countExt p bod
        L3.Assert rhs -> countExt p rhs
        _ -> 0

case_disabled_by_default :: Assertion
case_disabled_by_default =
  let body = getFunBody "loopifiedMap" (runnerDisabled loopifiedProg)
   in do
        0 @=? countIndirections body
        3 @=? countWhileCursors body

case_shares_dcon_and_copy_buffers :: Assertion
case_shares_dcon_and_copy_buffers =
  let body = getFunBody "loopifiedMap" (runnerEnabled loopifiedProg)
   in do
        2 @=? countIndirections body
        1 @=? countWhileCursors body

case_does_not_add_entry_unwrap_for_soa_input :: Assertion
case_does_not_add_entry_unwrap_for_soa_input =
  let prg = Prog M.empty (M.fromList [("consumer", consumerFun)]) Nothing
      body = getFunBody "consumer" (runnerEnabled prg)
   in 0 @=? countSelectiveUnwraps body

case_adds_call_site_unwrap_for_selective_output :: Assertion
case_adds_call_site_unwrap_for_selective_output =
  let prg = runnerEnabled callSiteProg
      body = getFunBody "consumer" prg
      mainUnwraps =
        case mainExp prg of
          Just (main, _) -> countSelectiveUnwraps main
          Nothing -> error "expected call-site test main expression"
      timedUnwraps =
        case mainExp prg of
          Just (main, _) -> countTimedSelectiveUnwraps main
          Nothing -> error "expected call-site test main expression"
   in do
        0 @=? countSelectiveUnwraps body
        1 @=? mainUnwraps
        0 @=? timedUnwraps

selectiveBufferSharingTests :: TestTree
selectiveBufferSharingTests = $(testGroupGenerator)
