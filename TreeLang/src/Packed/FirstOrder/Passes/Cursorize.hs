{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Inserting cursors and lowering to the target language.
--   This shares a lot with the effect-inference pass.

module Packed.FirstOrder.Passes.Cursorize
    (cursorize, lower) where

import Control.DeepSeq
import Packed.FirstOrder.Common hiding (FunDef)
import qualified Packed.FirstOrder.L1_Source as L1
import           Packed.FirstOrder.LTraverse as L2
import qualified Packed.FirstOrder.Target as T
import Data.List as L hiding (tail)
import Data.Set as S
import Data.Map as M
import Text.PrettyPrint.GenericPretty
import Debug.Trace

-- | Chatter level for this module:
lvl :: Int
lvl = 5

-- =============================================================================

-- | Map every lexical variable in scope to an abstract location.
--   Some variables are cursors, and they are runtime witnesses to
--   particular location variables.
type Env = M.Map Var Loc

-- type Env = M.Map Var LocValue

-- data LocValue = InCursor  LocVar
--               | OutCursor LocVar
--               | NotCursor Loc
--   deriving (Read,Show,Eq,Ord, Generic, NFData)
-- instance Out LocValue
                
-- | This inserts cursors and REMOVES effect signatures.  It returns
--   the new type as well as how many extra params were added to input
--   and return types.
cursorizeTy :: ArrowTy Ty -> (ArrowTy Ty, [LocVar], [LocVar])
cursorizeTy (ArrowTy inT ef ouT) = (newArr, newIn, newOut)
 where
  newArr = ArrowTy newInTy S.empty newOutTy
  newInTy  = prependArgs (L.map mkCursorTy newIn)
                         (mapPacked (\_ l -> mkCursorTy l) inT)
  -- Let's turn output values into updated-output-cursors:a
  newOutTy = prependArgs (L.map mkCursorTy newOut)
                         (mapPacked (\_ l -> mkCursorTy (toEndVar l)) ouT)
                         -- Or they could be void...

  -- Every packed input means another output (new return value for the
  -- moved cursor), and conversely, every output cursor must have had
  -- an original position (new input param):
  newOut   = [ toEndVar v  -- This determines the ORDER of added inputs.
             | Traverse v <- S.toList ef ]
  newIn    = allLocVars ouT -- These stay in their original order (preorder) 

-- Injected cursor args go first in input and output:
prependArgs [] t = t
prependArgs ls t = ProdTy $ ls ++ [t]

             
mkArrowTy :: Ty -> Ty -> ArrowTy Ty
mkArrowTy x y = ArrowTy x S.empty y

-- | Replace all packed types with something else.
replacePacked :: Ty -> Ty -> Ty
replacePacked (t2::Ty) (t::Ty) =
  case t of
    IntTy  -> IntTy
    BoolTy -> BoolTy
    SymTy  -> SymTy
    (ProdTy x)    -> ProdTy $ L.map (replacePacked t2) x
    (SymDictTy x) -> SymDictTy $ (replacePacked t2) x
    PackedTy{}    -> t2

mapPacked :: (Var -> LocVar -> Ty) -> Ty -> Ty
mapPacked fn t =
  case t of
    IntTy  -> IntTy
    BoolTy -> BoolTy
    SymTy  -> SymTy
    (ProdTy x)    -> ProdTy $ L.map (mapPacked fn) x
    (SymDictTy x) -> SymDictTy $ mapPacked fn x
    PackedTy k l  -> fn k l

-- voidT   = ProdTy []          

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
  fd (f@FunDef{funname,funty,funarg,funbod}) = 
      let (newTy@(ArrowTy inT _ _outT),newIn,newOut) = cursorizeTy funty in
      dbgTrace lvl ("Processing fundef: "++show(doc f)++"\n  new type: "++sdoc newTy) $
   do      
      fresh <- gensym "tupin"
      let argLoc  = argtyToLoc (mangle newArg) inT
          (newArg,bod,env) =
              if newIn == [] -- No injected cursor params..
              then (funarg, funbod, M.singleton newArg argLoc)
              else ( fresh
                     -- We could introduce a let binding, but here we
                     -- just substitute instead:
                   , L1.subst funarg (L1.ProjE (length newIn) (L1.VarE fresh))
                              funbod
                   , M.singleton fresh
                     (TupLoc $ L.map Fixed newIn ++ [argLoc]))
      exp' <- tail newOut env bod
      return $ FunDef funname newTy newArg exp'

  -- When processing something in tail position, we take a DEMAND for
  -- a certain list of location witnesses.  We produce a tuple containing
  -- those witnesses prepended to the original return value.
  tail :: [LocVar] -> Env -> L1.Exp -> SyM L1.Exp
  tail demanded env e = 
   dbgTrace lvl ("\n[cursorize] Processing tail: "++show (doc e)++"\n  with env: "++show env) $
   let cursorRets = L.map (meetDemand env) demanded in
   case e of
     -- Trivial return cases need to just pluck from the environment:
     L1.LitE _ -> return $ mkProd cursorRets e
     L1.VarE _ -> return $ mkProd cursorRets e
     
     -- Here we route through extra arguments.
     L1.LetE (v,tv,rhs) bod ->
       do (new,rhs',rty,rloc) <- exp env rhs
          -- ty will always be a known number of cursors (0 or more)
          -- prepended to the value that was already 
          tmp <- gensym "tmp"
          let ix = length new
          return $ L1.LetE ("tmp",rty,rhs') $
                   L1.LetE (v,tv, L1.ProjE ix (L1.VarE "tmp")) $
                   finishEXP
                 
     _ -> undefined
             
  -- This looks like a flattening pass.  Because of the need to route
  -- new, additional outputs from subroutine calls, we use tuples and
  -- let bindings heavily, including changing the types of
  -- conditionals to return tuples.
  --      
  -- We return a list corresponding to the cursor values ADDED to the
  -- return type, containing their locations.
  exp :: Env -> L1.Exp -> SyM ([Loc], L1.Exp, L1.Ty, Loc)
  exp env e = 
    dbgTrace lvl ("\n[cursorize] Processing exp: "++show (doc e)++"\n  with env: "++show env) $
    case e of
{-
     L1.VarE v  -> let NotCursor x = env # v in
                   return (__, x)
     L1.LitE  _ -> return (__, Bottom)

     -- Here's where the magic happens, we must populate new cursor arguments:
     L1.AppE f e ->
       maybeLet e $ \ (extra,rands) ->
        finishEXP
                   
     L1.IfE a b c -> do
       (a',aloc) <- exp env a
       -- If we need to route traversal results out of the branches,
       -- we need to change the type of these branches.
       return (finishEXP, finishLOC)
-}
     _ -> return ([], finishEXP, finishTYP, finishLOC)
     _ -> error $ "ERROR: cursorize: unfinished, needs to handle:\n "++sdoc e

-- | Dig through an environment to find
meetDemand :: Env -> LocVar -> L1.Exp
meetDemand env vr =
  trace ("Attempting to meet demand for loc "++show vr++" in env "++sdoc env) $
  go (M.toList env)
  where
   go [] = error$ "meetDemand: internal error, got to end of"++
                    " environment without finding loc: "++show vr
                    

mkProd [] e = e
mkProd ls e = L1.MkProdE $ ls++[e]
                                                        

finishEXP :: L1.Exp
finishEXP = (L1.VarE "FINISHME")

finishLOC :: Loc
finishLOC = Fresh "FINISHME"

finishTYP :: L1.Ty
finishTYP = L1.Packed "FINISHME" 
            
maybeLet :: forall t. t
maybeLet = undefined

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
        T.LetCallT [(v,typ t)] f
             [(triv "app rand") arg]
             <$>
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

-- ================================================================================

