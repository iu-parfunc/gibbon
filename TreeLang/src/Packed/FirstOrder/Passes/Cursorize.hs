{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Inserting cursors and lowering to the target language.
--   This shares a lot with the effect-inference pass.

module Packed.FirstOrder.Passes.Cursorize
    (cursorize, lower) where

import Packed.FirstOrder.Common hiding (FunDef)
import qualified Packed.FirstOrder.L1_Source as L1
import           Packed.FirstOrder.LTraverse as L2
import qualified Packed.FirstOrder.Target as T
import Data.List as L hiding (tail)
import Data.Set as S
import Data.Map as M
import Text.PrettyPrint.GenericPretty
-- import Debug.Trace

-- | Chatter level for this module:
lvl :: Int
lvl = 5

-- =============================================================================

-- | Map every lexical variable in scope to an abstract location.
type Env = M.Map Var Loc

-- | This inserts cursors and REMOVES effect signatures.  It returns
--   the new type as well as how many extra params were added to input
--   and return types.
cursorizeTy :: ArrowTy Ty -> (ArrowTy Ty, Int, Int)
cursorizeTy (ArrowTy inT ef ouT) =
  (ArrowTy (appendArgs newIns  (replacePacked cursorTy inT))
          S.empty
          (appendArgs newOuts (replacePacked voidT ouT))
  , numIn, numOut )
 where
  appendArgs [] t = t
  appendArgs ls t = ProdTy $ ls ++ [t]
  numIn   = S.size ef
  numOut  = length outVs
  newOuts = replicate numIn  cursorTy
  newIns  = replicate numOut cursorTy
  outVs   = allLocVars ouT
  voidT   = ProdTy []          
  replacePacked (t2::Ty) (t::Ty) =
    case t of
      IntTy -> IntTy
      SymTy -> SymTy
      (ProdTy x)    -> ProdTy $ L.map (replacePacked t2) x
      (SymDictTy x) -> SymDictTy $ (replacePacked t2) x
      PackedTy{}    -> t2

                       
-- Use a hack rather than extending the IR at this point:
cursorTy :: Ty
cursorTy = PackedTy "CURSOR_TY" ""

-- | A compiler pass that inserts cursor-passing for reading and
-- writing packed values.
cursorize :: Prog -> SyM Prog  -- [T.FunDecl]
cursorize prg@Prog{fundefs} = -- ddefs, fundefs
    dbgTrace lvl ("Starting cursorize on "++show(doc fundefs)) $ do 
    -- Prog emptyDD <$> mapM fd fundefs <*> pure Nothing

    fds' <- mapM fd $ M.elems fundefs
    return prg{ fundefs = M.fromList $ L.map (\f -> (funname f,f)) fds' }
 where
  fd :: FunDef -> SyM FunDef
  fd (f@FunDef{funname,funty,funarg,funbod}) = dbgTrace lvl ("Processing fundef: "++show(doc f)) $ do      
      let (newTy@(ArrowTy inT _ _outT),newIn,_newOut) = cursorizeTy funty
      fresh <- gensym "tupin"
      let newArg = if newIn == 0 then funarg else fresh
          bod    = if newIn == 0 then funbod
                   else L1.subst funarg (L1.ProjE newIn (L1.VarE fresh)) funbod
          env = M.singleton newArg argLoc
          argLoc  = argtyToLoc (mangle newArg) inT
      (exp',_) <- exp env bod
      return $ FunDef funname newTy newArg exp'
   
  exp :: Env -> L1.Exp -> SyM (L1.Exp, Loc)
  exp env e = 
    dbgTrace lvl ("\n[addCursors] Processing exp: "++show (doc e)++"\n  with env: "++show env) $
    case e of
     L1.VarE v  -> return (__, env # v)
     L1.LitE  _ -> return (__, Bottom)
     _ -> return (e,Top)
     _ -> error $ "ERROR: cursorize: unfinished, needs to handle:\n "++sdoc e

-- =============================================================================

-- | Convert into the target language.  This does not make much of a
-- change, but it checks the changes that have already occurred.
--
-- The only substantitive conversion here is of tupled arguments to
-- multiple argument functions.
lower :: L2.Prog -> SyM T.Prog
lower prg@L2.Prog{fundefs,ddefs,mainExp} = do
  mn <- case mainExp of
          Nothing -> return Nothing
          Just x  -> Just <$> tail x
  T.Prog <$> (mapM fund (M.elems fundefs)) <*> pure mn
 where
  fund :: L2.FunDef -> SyM T.FunDecl
  fund FunDef{funname,funty=(L2.ArrowTy inty _ outty),funarg,funbod} = do
      tl <- tail funbod
      return $ T.FunDecl { T.funName = funname
                         , T.funArgs = [(funarg, typ' inty)]
                         , T.funRetTy = typ' outty
                         , T.funBody = tl } 

  tail :: L1.Exp -> SyM T.Tail
  tail ex = 
   case ex of
--    L1.LetE (v,t,rhs) bod -> T.LetE (v,t,tail rhs) (tail bod)
    L1.VarE v          -> pure$ T.RetValsT [T.VarTriv v]
    
    L1.IfE a b c       -> do b' <- tail b
                             c' <- tail c
                             return $ T.Switch (triv "if test" a)
                                      (T.IntAlts [(0, b')])
                                      (Just c')

    L1.AppE v e        -> return $ T.TailCall v [triv "operand" e]

    L1.LetE (v,t,L1.PrimAppE p ls) bod ->
        T.LetPrimCallT [(v,typ t)]
             (prim p)
             (L.map (triv "prim rand") ls) <$>
             (tail bod)

    L1.LetE (v,t,L1.AppE f arg) bod -> do
        sym <- gensym "tmpstrct"
        T.LetCallT [(v,typ t)] f
             [(triv "app rand") arg]
             (typ t) -- Redundant?
             sym <$>
             (tail bod)

    L1.CaseE e ls ->
        return $ T.Switch{} -- (tail e) (M.map (\(vs,er) -> (vs,tail er)) ls)

    _ -> error$ "lower: unexpected expression in tail position:\n  "++sdoc ex
             
{-    
    L1.LitE _          -> ex
    
    L1.PrimAppE p ls   -> L1.PrimAppE p $ L.map tail ls
    
    L1.ProjE i e       -> L1.ProjE i (tail e)
    L1.CaseE e ls      -> L1.CaseE (tail e) (M.map (\(vs,er) -> (vs,tail er)) ls)
    L1.MkProdE ls      -> L1.MkProdE $ L.map tail ls
    L1.MkPackedE k ls  -> L1.MkPackedE k $ L.map tail ls
    L1.TimeIt e        -> L1.TimeIt $ tail e
    
-}

  triv :: String -> L1.Exp -> T.Triv
  triv = error "FINISHME lower/triv"
  
  typ :: L1.Ty -> T.Ty
  typ = error "FINISHME lower/typ"

  typ' :: L2.Ty -> T.Ty
  typ' = error "FINISHME lower/typ'"

  prim :: L1.Prim -> T.Prim
  prim p =
    case p of
      L1.AddP -> T.AddP
      L1.SubP -> T.SubP
      L1.MulP -> T.MulP
      L1.EqP  -> __ -- T.EqP
      L1.DictInsertP -> T.DictInsertP
      L1.DictLookupP -> T.DictLookupP
      (L1.ErrorP s)  -> __ -- T.ErrorP s
