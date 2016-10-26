{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}

-- | An intermediate language with an effect system that captures traversals.

module Packed.FirstOrder.LTraverse where

import qualified Packed.FirstOrder.Common as C
import Packed.FirstOrder.Common hiding (FunDef)
import qualified Packed.FirstOrder.L1_Source as L1
import Data.List as L
import Data.Set as S
import Data.Map as M
import Text.PrettyPrint.GenericPretty

--------------------------------------------------------------------------------
    
-- Unchanged from L1, or we could go A-normal:
-- data Exp =

-- | Abstract location variables.
type LocVar = Var

-- | Abstract locations:
data Loc = Fixed Var -- ^ A rigid location, such as for an input or output field.
         | Fresh Var -- ^ Fresh location-variables as created by
                     -- calling functions that are polymorphic in
                     -- their output location.
         | TupLoc [Loc] -- ^ The locations for each part of a tuple.
         | Top    -- ^ Contradiction.  Locations couldn't unify.
         | Bottom -- ^ "don't know" or "don't care".  This is the
                  -- location for non-packed data.
  deriving (Read,Show,Eq,Ord, Generic)

instance Out Loc

-- | This should be a semi-join lattice.
join :: Loc -> Loc -> (Loc,[Constraint])
join Bottom Bottom = (Bottom,[])
join Top _         = (Top,[])
join _   Top       = (Top,[])
join (Fresh v) (Fresh w) = (Fresh v, [Eql v w])
join (Fresh v) (Fixed w) = (Fixed w, [Eql v w])
join (Fixed v) (Fresh w) = (Fixed v, [Eql v w])
join (Fixed v) (Fixed w) | v == w    = (Fixed v, [])
                         | otherwise = (Top, [])
join (TupLoc l1) (TupLoc l2) =
    let (locs,cs) = unzip $ zipWith join l1 l2 in
    (TupLoc locs, concat cs)
join l1 l2 = error$ "join: locations have inconsistent shapes: "++show(doc (l1,l2))
                           
joins :: [Loc]-> (Loc,[Constraint])
joins [] = (Bottom,[])
joins (a:b) = let (l,c) = joins b 
                  (l2,c2) = join a l
              in (l2,c++c2)

-- | We need equality for join and disequality for distinct fields'
--   and arguments' locations.
data Constraint = Eql Var Var
                | Neq Var Var
                     
-- -- | When processing an expression, our output goes to a certain set
-- -- of abstract locations.  The shape of the context is based on the
-- -- shape of the Type.
-- data Context = EmptyCtxt
-- --             | InFst Context | InSnd  Context
--              | TupCtxt [Context]
--              | OutCtxt LocVar
    
-- Our type for functions grows to include effects.
data ArrowTy = ArrowTy Ty (Set Effect) Ty
  deriving (Read,Show,Eq,Ord, Generic)

data Effect = Traverse LocVar
  deriving (Read,Show,Eq,Ord, Generic)

instance Out Ty
instance Out ArrowTy
instance Out Effect
instance Out a => Out (Set a) where
  docPrec n x = docPrec n (S.toList x)
  doc x = doc (S.toList x)
instance Out FunDef
instance Out Prog
    
type OldFuns = FunDefs L1.Ty L1.Exp
type NewFuns = M.Map Var FunDef
    
type FunEnv = M.Map Var ArrowTy

-- | L1 Types extended with abstract Locations
data Ty = IntTy | SymTy | ProdTy [Ty] | SymDictTy Ty  
        | PackedTy { con :: Constr, loc :: LocVar }
  deriving (Show, Read, Ord, Eq, Generic)
           
-- | Here we only change the types of FUNCTIONS:
data Prog = Prog { ddefs    :: DDefs L1.Ty
                 , fundefs  :: NewFuns
                 , mainExp  :: Maybe L1.Exp
                 }
  deriving (Show, Read, Ord, Eq, Generic)

-- | Arrow type caries the function's effectS:
data FunDef = FunDef Var ArrowTy Var L1.Exp
  deriving (Show, Read, Ord, Eq, Generic)
--------------------------------------------------------------------------------
    

-- | We initially populate all functions with empty effect signatures.
--   We also associate fresh location variables with packed types.
initialEnv :: OldFuns -> FunEnv
initialEnv mp = M.map (\x -> fst $ runSyM 0 (go x))  mp
  where
    go :: C.FunDef L1.Ty L1.Exp -> SyM ArrowTy
    go (C.FunDef _ (_,argty) ret _)  =
        do argTy <- annotateTy argty
           retTy <- annotateTy ret
           return $ ArrowTy argTy S.empty retTy
                                    
-- (L1.FunDef name retty args bod)

-- | Annotate a naked type with location
annotateTy :: L1.Ty -> SyM Ty
annotateTy t =
  case t of
    L1.Packed k -> PackedTy k <$> genLetter                   
    L1.IntTy    -> return IntTy
    L1.SymTy    -> return SymTy
    L1.ProdTy l -> ProdTy <$> mapM annotateTy l
    L1.SymDictTy v -> SymDictTy <$> annotateTy v

inferProg :: L1.Prog -> Prog
inferProg (L1.Prog dd fds mainE) =
  Prog dd
       (M.intersectionWith (\ (C.FunDef nm (arg,_) _ bod) arrTy ->
                             FunDef nm arrTy arg bod)
          fds finalFunTys)
       mainE
 where
   finalFunTys :: FunEnv
   finalFunTys = fst $ runSyM 0 $
                 fixpoint fds (initialEnv fds)
   
   fixpoint :: OldFuns -> FunEnv -> SyM FunEnv
   fixpoint funs env =
    do effs' <- M.fromList <$>
                mapM (\(k,v) -> (k,) <$> inferEffects env v)
                     (M.toList funs)
       let env' = M.intersectionWith
                  (\ neweffs (ArrowTy as _ b) -> ArrowTy as neweffs b)
                  effs' env
       if env == env'
        then return env
        else fixpoint funs env'

-- | Apply a variable substitution to a type.
substTy :: Map LocVar LocVar -> Ty -> Ty
substTy mp t = go t
  where
    go t = 
     case t of
      IntTy -> IntTy
      SymTy -> SymTy
      SymDictTy te -> SymDictTy (go te)
      ProdTy    ts -> ProdTy    (L.map go ts)
      PackedTy k l -> PackedTy k (mp M.! l)
      
             
-- | Take a polymorphic ArrowTy, instantiate its location variables
-- and traversal effects with the given locations.
instantiateEffs :: ArrowTy -> Loc -> SyM (Set Effect, Ty)
instantiateEffs (ArrowTy inT effs outT) loc =
     -- applySubst subst outT
    return (S.map (\(Traverse v) -> Traverse (subst M.! v)) effs,
            substTy subst outT)
  where
   subst = unify loc inT
    
   -- Unify the location of the argument, with the loc variables in the type:
   unify :: Loc -> Ty -> M.Map LocVar LocVar
   unify Bottom _                 = M.empty
   unify (Fixed l) (PackedTy _ v) = M.singleton v l
   unify (Fresh l) (PackedTy _ v) = M.singleton v l
   unify (TupLoc l1) (ProdTy l2)  = M.unions (zipWith unify l1 l2)
   unify Top       (PackedTy _ v) = M.empty -- M.singleton v l
   unify loc ty = error$ "instantiateEffs: argument type "++show(doc ty)
                      ++"does not have matching structure to location: "++show(doc loc)
                                    
-- | Map every lexical variable in scope to an abstract location.
type Env = M.Map Var Loc

-- | Convert the type of a function argument to an abstract location
-- for that function argument.
tyToLoc :: Var -> L1.Ty -> Loc
tyToLoc v (L1.Packed _) = Fixed v
 -- ^ Here we set the type based on the variable binding name, not the
 -- quantified loc variable in the type signature.
tyToLoc v (L1.ProdTy ls) = TupLoc [tyToLoc (subloc v i) t | (t,i) <- zip ls [1..]]
 -- ^ Here we generate fixed locations that are *subparts* of the function argument.
tyToLoc _ L1.SymTy        = Bottom
tyToLoc _ L1.IntTy        = Bottom
tyToLoc v (L1.SymDictTy _t) = -- ^ This may contain packed objects, but it is not contiguous.
    Fixed v
    -- if hasPacked t then Top else Bottom


hasPacked :: L1.Ty -> Bool
hasPacked t = case t of
                L1.Packed _  -> True
                L1.ProdTy ls -> any hasPacked ls
                L1.SymTy     -> False
                L1.IntTy     -> False
                L1.SymDictTy t -> hasPacked t
                             
-- A bit name mangling:

-- | First, lift program variables so they don't interfere with ones
-- we introduce.  Also, remove internal underscores.
mangle :: Var -> Var
mangle v = "_" ++ L.filter (/='_') v

-- | Refer to a portion of the data associated with a var.
subloc :: Var -> Int -> Var
subloc v n = v ++"_"++show n

-- | Strip off any subloc modifiers
-- root :: Var -> Var

freshLoc :: String -> SyM Loc
freshLoc m = Fresh <$> gensym m

getLocVar :: Loc -> Maybe Var
getLocVar (Fresh v) = Just v
getLocVar (Fixed v) = Just v
getLocVar Top = Nothing
getLocVar l = error $"getLocVar: expected a single packed value location, got: "
                    ++show(doc l)
             
inferEffects :: FunEnv -> C.FunDef L1.Ty L1.Exp -> SyM (Set Effect)
inferEffects fenv (C.FunDef _name (arg,argty) retty bod) =
    fst <$> exp outloc0 env0 bod
  where
  env0    = M.singleton arg (tyToLoc (mangle arg) argty)
  outloc0 = tyToLoc "out" retty

  -- We have one location for the destination, and another for each lexical binding.
  exp :: Loc -> Env -> L1.Exp -> SyM (Set Effect, Loc)
  exp out env e = 
    case e of
     -- QUESTION: does a variable reference count as traversing to the end?
     -- If so, the identity function has the traverse effect.
     -- I'd prefer that the identity function get type (Tree_p -> Tree_p).
     L1.VarE v  -> return (S.empty, env M.! v)
     L1.LitE  _ -> return (S.empty, Bottom)
     L1.CaseE e1 mp ->
      do (eff1,loc1) <- exp out env e1
         (bools,effs,locs) <- unzip3 <$>
                              mapM (rhs out loc1 env) (M.elems mp)
         -- Critical policy point!  We only get to the end if ALL
         -- branches get to the end.
         let end = if all id bools
                   then case getLocVar loc1 of
                          Just v  -> S.singleton (Traverse v)
                          Nothing -> S.empty
                   else S.empty
         let (locFin,constraints) = joins locs
         return (S.union (S.union eff1 end)
                         (L.foldl1 S.intersection effs),
                 locFin)

     -- Construct output packed data.  We will always "scroll to the end" of 
     -- output values, so they are not interesting for this effect analysis.
     L1.MkPackedE k ls -> trivs ls $
        -- And because it's freshly allocated, it has unconstrained location:
        do l <- freshLoc $ "mk"++k
           return (S.empty,l)

     -- We need to reach a fixed point where we jointly infer effects
     -- for all functions.
     L1.AppE rat (L1.VarE rand) ->
         -- | Just loc <- M.lookup rand env ->
       do let loc   = env M.! rand
          let arrTy = fenv M.! rat
          (effs,loc') <- instantiateEffs arrTy loc
 
          -- Assume rands are trivial.
          -- The traversal effects are inherited based on the type of rator.
          error "finishAPPE"

{-
     L1.AppE rat rand -> triv rand $ undefined

     -- If rands are already trivial 
     L1.PrimAppE _ rands -> trivs rands $          
         return S.empty
                          
     -- Here we UNION the end-points that are reached in the RHS and the BOD:
     L1.LetE (v,t,rhs) bod -> -- FIXME: change to let.
         let env' = extendEnv [v] env
             out' = undefined -- ?? create based on type.
         in
         S.union <$> exp out env' bod
                 <*> exp out' env rhs

     -- If any sub-expression reaches a destination, we can reach the destination:
     L1.MkProdE ls -> S.unions <$> mapM (exp out env) ls
     L1.ProjE _ e -> exp out env e

--     L1.MkPacked k ls ->
                     
  rhs :: Loc -> Env -> ([Var], L1.Exp) -> SyM (Set Effect)
  rhs out env ([], erhs) = addOuts out <$> exp out env erhs
  rhs out env (patVs, erhs) =
      -- Subtlety: if the rhs expression consumes the RIGHTMOST
      -- pattern variable, then the later code transformations will
      -- have to ensure that it consumes everything.
   do let env' = extendEnv patVs env
      eff <- exp out env' erhs
      let isLocal (Traverse v) = L.elem v patVs -- FIXME... need LocVar
          stripped  = S.filter isLocal eff
      return $ if S.member (Traverse (L.last patVs)) eff
               then addOuts out stripped
               else stripped
-}

  rhs = undefined

-- Simple invariant assertions:
           
triv :: L1.Exp -> a -> a
triv = undefined

trivs :: [L1.Exp] -> a -> a
trivs = undefined

-- | Build up a set of effects corresponding to an output context.
addOuts :: Loc -> Set Effect -> Set Effect
addOuts Bottom       s = s
addOuts Top          _ = error "What to do here?"
-- addOuts (Fixed v)    = S.insert (Traverse v) 
-- addOuts (TupCtxt [y]) = addOuts y
-- addOuts (TupCtxt (x:rs)) = addOuts x . addOuts (TupCtxt rs)
           
-- TODO: need abstract locations for these:
extendEnv :: [Var] -> Env -> Env
extendEnv = undefined
            
           
-- data Ty
--     = IntTy 
--     | TagTy -- ^ A single byte / Word8
--     | SymTy -- ^ Symbols used in writing compiler passes.
--             --   It's an alias for Int, an index into a symbol table.
--     | ProdTy [Ty]
--     | SymDictTy T1
--      -- ^ We allow built-in dictionaries from symbols to a value type.
--     | Packed Constr [T1]            
--  deriving (Show, Read, Ord, Eq, Generic)


-- Examples and Tests:
--------------------------------------------------------------------------------

exadd1 :: Prog
exadd1 = inferProg L1.add1Prog
