{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Inserting cursors and lowering to the target language.
--   This shares a lot with the effect-inference pass.

module Packed.FirstOrder.Passes.Cursorize
    (cursorize, lower) where

import Packed.FirstOrder.Common hiding (FunDef)
import qualified Packed.FirstOrder.L1_Source as L1
import           Packed.FirstOrder.LTraverse as L2
import qualified Packed.FirstOrder.Target as L3
import Data.List as L
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
cursorize :: Prog -> SyM Prog  -- [L3.FunDecl]
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
lower :: L2.Prog -> SyM L3.Prog
lower L2.Prog{fundefs,ddefs,mainExp} = 
  case __ of 
   _ -> error "FINISHME"
 where
  exp :: L1.Exp -> L3.Tail
  exp = undefined

