{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}

-- | An intermediate language with an effect system that captures traversals.

module Packed.FirstOrder.LTraverse where

import Control.Monad (when)
import qualified Packed.FirstOrder.Common as C
import Packed.FirstOrder.Common hiding (FunDef)
import qualified Packed.FirstOrder.L1_Source as L1
import Data.List as L
import Data.Set as S
import Data.Map as M
import Text.PrettyPrint.GenericPretty
import Debug.Trace
import GHC.Stack (errorWithStackTrace)

traceIt :: Show a => String -> a -> a
traceIt msg x = trace (msg++": "++show x) x
    
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
  deriving (Read,Show,Eq,Ord, Generic)
instance Out Constraint

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

-- | We initially populate all functions with MAXIMUM effect signatures.
--   Subsequently, these monotonically SHRINK until a fixpoint.
--   We also associate fresh location variables with packed types.
initialEnv :: OldFuns -> FunEnv
initialEnv mp = M.map (\x -> fst $ runSyM 0 (go x))  mp
  where
    go :: C.FunDef L1.Ty L1.Exp -> SyM ArrowTy
    go (C.FunDef _ (_,argty) ret _)  =
        do argTy <- annotateTy argty
           retTy <- annotateTy ret
           let maxEffects = S.map Traverse
                            (S.union (getTyLocs argTy) (getTyLocs retTy))
           return $ ArrowTy argTy maxEffects retTy
                                    
-- | Retrieve all LocVars mentioned in a type
getTyLocs :: Ty -> Set LocVar
getTyLocs t =
    case t of
      IntTy  -> S.empty
      SymTy  -> S.empty
      ProdTy ls -> S.unions (L.map getTyLocs ls)
      PackedTy _ lv -> S.singleton lv
      -- This is a tricky case:
      SymDictTy elt -> getTyLocs elt
      
                  
-- | Annotate a naked type with fresh location variables.
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
      PackedTy k l -> case M.lookup l mp of
                        Just v  -> PackedTy k v
                        Nothing -> PackedTy k l
                            -- errorWithStackTrace $ "substTy: failed to find "++show l++
                            --   "\n  in map: "++show mp++", when processing type "++show t

-- | Apply a substitution to an effect set.                                   
substEffs :: Map LocVar LocVar -> Set Effect -> Set Effect
substEffs mp ef =
    trace ("\n  Substituting in effects "++show(mp,ef)) $ 
    S.map (\(Traverse v) ->
               case M.lookup v mp of
                 Just v2 -> Traverse v2
                 Nothing -> Traverse v) ef

allLocVars :: Ty -> [LocVar]
allLocVars t =
    case t of
      SymTy     -> []
      IntTy     -> []
      PackedTy _ v -> [v]
      ProdTy ls  -> L.concatMap allLocVars ls
      SymDictTy elt -> allLocVars elt    
               
freshenVar :: LocVar -> SyM LocVar
freshenVar = gensym
               
freshenArrowSchema :: ArrowTy -> SyM ArrowTy
freshenArrowSchema (ArrowTy inT effs outT) = do
    let lvs = allLocVars inT ++ allLocVars outT
    lvs' <- mapM freshenVar lvs
    let subst = M.fromList (zip lvs lvs')
    return $ ArrowTy (substTy subst inT)
                     (substEffs subst effs)
                     (substTy subst outT)
               
-- | Take a polymorphic ArrowTy, instantiate its location variables
--   and traversal effects with the given (input) locations.
--   Return the location that the result of the application will occupy.
instantiateApp :: ArrowTy -> Loc -> SyM (Set Effect, Loc)
instantiateApp arrty0 loc = do
    (ArrowTy inT effs outT) <- freshenArrowSchema arrty0
    let subst = zipTL inT loc 
    trace ("\n  instantiateApp: Came up with subst: "++show subst) $
     return (substEffs subst effs,
            traceIt "   instantiate result loc" $ rettyToLoc (substTy subst outT))
  where
   -- Question: when computing the return location, which variables are Fresh?
   -- Conversely, when would we need to use Fixed?
   rettyToLoc :: Ty -> Loc
   rettyToLoc t =
     case t of
       IntTy -> Bottom
       SymTy -> Bottom
       SymDictTy _  -> Top
       ProdTy ls    -> TupLoc $ L.map rettyToLoc ls
       PackedTy _ l -> Fresh l
    
-- | Unify type and locaion , creating a mapping between variables in
-- the former to the latter.
zipTL :: Ty -> Loc -> M.Map LocVar LocVar
zipTL _ Bottom                 = M.empty
zipTL (PackedTy _ v) (Fixed l) = M.singleton v l
zipTL (PackedTy _ v) (Fresh l) = M.singleton v l
zipTL (ProdTy l1) (TupLoc l2)  = M.unions (zipWith zipTL l1 l2)

-- Here is a tricky one. 
zipTL (PackedTy l v) Top =
    error $ "zipTL: don't yet know what to do with Packed/Top case: "++
          show (PackedTy l v)
    -- M.empty -- M.singleton v l
zipTL ty loc = error$ "zipTL: argument type "++show(doc ty)
                   ++"does not have matching structure to location: "++show(doc loc)

-- | Unify location and type, creating a mapping between variables in
-- the former to the latter.
zipLT :: Loc -> Ty -> M.Map LocVar LocVar
zipLT Bottom _                 = M.empty
zipLT (Fixed l) (PackedTy _ v) = M.singleton l v
zipLT (Fresh l) (PackedTy _ v) = M.singleton l v
zipLT (TupLoc l1) (ProdTy l2)  = M.unions (zipWith zipLT l1 l2)
-- Here is a tricky one. 
zipLT Top       (PackedTy l v) =
    error $ "zipLT: don't yet know what to do with Top/Packed case: "++
          show (PackedTy l v)
    -- M.empty -- M.singleton v l
zipLT loc ty = error$ "zipLT: argument type "++show(doc ty)
                   ++"does not have matching structure to location: "++show(doc loc)



                     
-- | Map every lexical variable in scope to an abstract location.
type Env = M.Map Var Loc

-- | Convert the type of a function argument to an abstract location
-- for that function argument.
argtyToLoc :: Var -> L1.Ty -> Loc
argtyToLoc v (L1.Packed _) = Fixed v
 -- ^ Here we set the type based on the variable binding name, not the
 -- quantified loc variable in the type signature.
argtyToLoc v (L1.ProdTy ls) = TupLoc [argtyToLoc (subloc v i) t | (t,i) <- zip ls [1..]]
 -- ^ Here we generate fixed locations that are *subparts* of the function argument.
argtyToLoc _ L1.SymTy        = Bottom
argtyToLoc _ L1.IntTy        = Bottom
argtyToLoc v (L1.SymDictTy _t) = -- ^ This may contain packed objects, but it is not contiguous.
    Fixed v
    -- if hasPacked t then Top else Bottom

-- | Do values of this type contain packed data?
hasPacked :: L1.Ty -> Bool
hasPacked t = case t of
                L1.Packed _  -> True
                L1.ProdTy ls -> any hasPacked ls
                L1.SymTy     -> False
                L1.IntTy     -> False
                L1.SymDictTy t -> hasPacked t
                             
-- A bit of name mangling:
------------------------------------------------------------
-- | First, lift program variables so they don't interfere with ones
-- we introduce.  Also, remove internal underscores.
mangle :: Var -> Var
mangle v = v
-- mangle v = "_" ++ L.filter (/='_') v

-- | Refer to a portion of the data associated with a var.
subloc :: Var -> Int -> Var
subloc v n = v ++"_"++show n

-- Strip off any subloc modifiers
-- root :: Var -> Var
------------------------------------------------------------


freshLoc :: String -> SyM Loc
freshLoc m = Fresh <$> gensym m

-- | Take a location which is expected to be a single variable, and
-- retrieve that variable.
getLocVar :: Loc -> Maybe Var
getLocVar (Fresh v) = Just v
getLocVar (Fixed v) = Just v
getLocVar Top = Nothing
getLocVar l = error $"getLocVar: expected a single packed value location, got: "
                    ++show(doc l)
             
inferEffects :: FunEnv -> C.FunDef L1.Ty L1.Exp -> SyM (Set Effect)
inferEffects fenv (C.FunDef name (arg,argty) retty bod) =
    -- For this pass we don't need to know the output location:
    do (effs1,_loc) <- exp outloc0 env0 bod
       -- Finally, restate the effects in terms of the type schema for the fun:
       let allEffs = substEffs (zipLT argLoc inTy) effs1
           externalLocs = S.fromList $ allLocVars inTy ++ allLocVars outTy
       return $ S.filter (\(Traverse v) -> S.member v externalLocs) allEffs

  where
  (ArrowTy inTy _ outTy) = fenv # name
  env0    = M.singleton arg argLoc
  argLoc  = argtyToLoc (mangle arg) argty
  outloc0 = argtyToLoc "out" retty

  unused_outloc = error "This param needs to be removed"
            
  -- We have one location for the destination, and another for each lexical binding.
  exp :: Loc -> Env -> L1.Exp -> SyM (Set Effect, Loc)
  exp out env e =
    trace ("\nProcessing exp: "++show e++"\n  with env: "++show env) $
    case e of
     -- QUESTION: does a variable reference count as traversing to the end?
     -- If so, the identity function has the traverse effect.
     -- I'd prefer that the identity function get type (Tree_p -> Tree_p).
     L1.VarE v  -> return (S.empty, env # v)
     L1.LitE  _ -> return (S.empty, Bottom)
     L1.CaseE e1 mp ->
      do (eff1,loc1) <- exp out env e1
         (bools,effs,locs) <- unzip3 <$>
                              mapM (caserhs out loc1 env) (M.elems mp)
         -- Critical policy point!  We only get to the end if ALL
         -- branches get to the end.
         let end = if all id bools
                   then case getLocVar loc1 of
                          Just v  -> S.singleton (Traverse v)
                          Nothing -> S.empty
                   else S.empty
         let (locFin,cnstrts) = joins locs

         when (not (L.null cnstrts)) $
           error $"FINISHME: process these constraints: "++show cnstrts
                                
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
       do let loc   = env # rand
          let arrTy = fenv # rat
          instantiateApp arrTy loc

     -- Here we UNION the end-points that are reached in the RHS and the BOD:
     L1.LetE (v,t,rhs) bod -> -- FIXME: change to let.
      do (reff,rloc) <- exp unused_outloc env rhs
         let env' = M.insert v rloc env 
         (beff,bloc) <- exp out env' bod         
         return (S.union beff reff, bloc)

     L1.AppE rat rand -> triv rand $ undefined

     -- If rands are already trivial 
     L1.PrimAppE _ rands -> trivs rands $          
         return (S.empty, undefined)
                          
     -- If any sub-expression reaches a destination, we can reach the destination:
     L1.MkProdE ls -> do (effs,locs) <- unzip <$> mapM (exp out env) ls
                         error "FINISH mkprode"
     L1.ProjE _ e -> exp out env e

--     L1.MkPacked k ls ->

  -- Returns true if this particular case reaches the end of the scrutinee.
  caserhs :: Loc -> Loc -> Env -> ([Var], L1.Exp) -> SyM (Bool, Set Effect, Loc)
  caserhs out _scrut env ([], erhs) = do
     (effs,loc) <- exp out env erhs
     return $ ( True
              , addOuts out effs
              , loc)
{-                         
  caserhs out env (patVs, erhs) =
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

  caserhs _ _ _ _ = error "put rhs function back..."

-- Simple invariant assertions:
           
triv :: L1.Exp -> a -> a
triv e = case e of
           L1.VarE _ -> id
           L1.LitE _ -> id
           _         -> error$ "triv: expected trivial argument, got: "++show e

trivs :: [L1.Exp] -> a -> a
trivs [] = id
trivs (a:b) = triv a . trivs b

-- | Build up a set of effects corresponding to an output context.
addOuts :: Loc -> Set Effect -> Set Effect
addOuts Bottom       s = s
addOuts Top          _ = error "What to do here?"
-- addOuts (Fixed v)    = S.insert (Traverse v) 
-- addOuts (TupCtxt [y]) = addOuts y
-- addOuts (TupCtxt (x:rs)) = addOuts x . addOuts (TupCtxt rs)
           
-- TODO: need abstract locations for these:
extendEnv :: [Var] -> Env -> Env
extendEnv = error "finishme extendEnv"

            
(#) :: (Ord a1, Show a1) => Map a1 a -> a1 -> a
m # k = case M.lookup k m of
          Just x  -> x
          Nothing -> errorWithStackTrace $ "Map lookup failed on key: "++show k

-- Examples and Tests:
--------------------------------------------------------------------------------

exadd1 :: Prog
exadd1 = inferProg L1.add1Prog

