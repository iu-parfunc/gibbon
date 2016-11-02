{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

-- | An intermediate language with an effect system that captures traversals.

module Packed.FirstOrder.LTraverse
    ( Prog(..), Ty(..), FunEnv, FunDef(..), Effect(..), ArrowTy(..)
    , inferEffects, inferFunDef
    -- * Utilities for dealing with the extended types:
    , cursorTy, mkCursorTy, isCursorTy, cursorTyLoc
    , tyWithFreshLocs
    -- * Lattices of abstract locations:
    , Loc(..), LocVar, toEndVar, isEndVar, fromEndVar
    , join, joins
    , allLocVars, argtyToLoc, mangle, subloc
    , extendEnv, getLocVar
    -- * Constraints
    , Constraint(..)
    )
    where
import Control.Monad (when)
import Control.DeepSeq
import qualified Packed.FirstOrder.Common as C
import Packed.FirstOrder.Common hiding (FunDef)
import qualified Packed.FirstOrder.L1_Source as L1
-- import qualified Packed.FirstOrder.Target as T
-- import Packed.FirstOrder.L1_Source (Exp(..))
import Data.List as L
import Data.Set as S
import Data.Map as M
import Text.PrettyPrint.GenericPretty
-- import Debug.Trace
-- import GHC.Stack (errorWithStackTrace)
    
--------------------------------------------------------------------------------

-- | Chatter level for this module:
lvl :: Int
lvl = 5

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
  deriving (Read,Show,Eq,Ord, Generic, NFData)
instance Out Loc

toEndVar :: LocVar -> LocVar
toEndVar v =
  if isEndVar v
  then error$ "toEndVar: cannot add an end marker to something already at end: "++show v
  else end_prefix ++ v

fromEndVar :: LocVar -> Maybe LocVar
fromEndVar v | isEndVar v = Just (drop (length end_prefix) v)
          | otherwise = Nothing

isEndVar :: LocVar -> Bool
isEndVar = isPrefixOf end_prefix

end_prefix :: String
end_prefix = "end_" -- Hacky way to encode end-of-region variables.
        
    
-- | This should be a semi-join lattice.
join :: Loc -> Loc -> (Loc,[Constraint])
join Bottom y      = (y,[])
join x Bottom      = (x,[])
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
--                | EqlOffset Var (Int,Var)
  deriving (Read,Show,Eq,Ord, Generic, NFData)
instance Out Constraint

-- Our type for functions grows to include effects.
data ArrowTy t = ArrowTy { arrIn :: t, arrEffs:: (Set Effect), arrOut:: t }
  deriving (Read,Show,Eq,Ord, Generic, NFData)

data Effect = Traverse LocVar
  deriving (Read,Show,Eq,Ord, Generic, NFData)

instance Out Ty
instance Out t => Out (ArrowTy t)
instance Out Effect
instance Out a => Out (Set a) where
  docPrec n x = docPrec n (S.toList x)
  doc x = doc (S.toList x)
instance Out FunDef
instance Out Prog
    
type OldFuns = FunDefs L1.Ty L1.Exp
type NewFuns = M.Map Var FunDef
    
type FunEnv = M.Map Var (ArrowTy Ty)

-- | L1 Types extended with abstract Locations
data Ty = IntTy | SymTy | BoolTy | ProdTy [Ty] | SymDictTy Ty  
        | PackedTy { con :: Constr, loc :: LocVar }
  deriving (Show, Read, Ord, Eq, Generic, NFData)
           
-- | Here we only change the types of FUNCTIONS:
data Prog = Prog { ddefs    :: DDefs L1.Ty
                 , fundefs  :: NewFuns
                 , mainExp  :: Maybe L1.Exp
                 }
  deriving (Show, Read, Ord, Eq, Generic, NFData)

-- | A function definition with the function's effects.
data FunDef = FunDef { funname :: Var
                     , funty   :: (ArrowTy Ty)
                     , funarg   :: Var
                     , funbod  :: L1.Exp }
  deriving (Show, Read, Ord, Eq, Generic, NFData)
--------------------------------------------------------------------------------

-- | We initially populate all functions with MAXIMUM effect signatures.
--   Subsequently, these monotonically SHRINK until a fixpoint.
--   We also associate fresh location variables with packed types.
initialEnv :: OldFuns -> FunEnv
initialEnv mp = M.map (\x -> fst $ runSyM 0 (go x))  mp
  where
    go :: C.FunDef L1.Ty L1.Exp -> SyM (ArrowTy Ty)
    go (C.FunDef _ (_,argty) ret _)  =
        do argTy <- tyWithFreshLocs argty
           retTy <- tyWithFreshLocs ret
           let maxEffects = S.map Traverse
                            (S.union (getTyLocs argTy) (getTyLocs retTy))
           return $ ArrowTy argTy maxEffects retTy
                                    
-- | Retrieve all LocVars mentioned in a type
getTyLocs :: Ty -> Set LocVar
getTyLocs t =
    case t of
      IntTy  -> S.empty
      SymTy  -> S.empty
      BoolTy -> S.empty
      ProdTy ls -> S.unions (L.map getTyLocs ls)
      PackedTy _ lv -> S.singleton lv
      -- This is a tricky case:
      SymDictTy elt -> getTyLocs elt
      
                  
-- | Annotate a naked type with fresh location variables.
tyWithFreshLocs :: L1.Ty -> SyM Ty
tyWithFreshLocs t =
  case t of
    L1.Packed k -> PackedTy k <$> genLetter                   
    L1.IntTy    -> return IntTy
    L1.SymTy    -> return SymTy
    L1.BoolTy   -> return BoolTy
    L1.ProdTy l -> ProdTy <$> mapM tyWithFreshLocs l
    L1.SymDictTy v -> SymDictTy <$> tyWithFreshLocs v

inferEffects :: L1.Prog -> SyM Prog
inferEffects (L1.Prog dd fds mainE) = do
  finalFunTys <- fixpoint 1 fds (initialEnv fds)
  return $ Prog dd
           (M.intersectionWith (\ (C.FunDef nm (arg,_) _ bod) arrTy ->
                                  FunDef nm arrTy arg bod)
            fds finalFunTys)
           mainE
 where
   
   fixpoint :: Int -> OldFuns -> FunEnv -> SyM FunEnv
   fixpoint iter funs env =
    do effs' <- M.fromList <$>
                mapM (\(k,v) -> (k,) <$> inferFunDef (dd,env) v)
                     (M.toList funs)
       let env' = M.intersectionWith
                  (\ neweffs (ArrowTy as _ b) -> ArrowTy as neweffs b)
                  effs' env
       if env == env'
        then dbgTrace 4 ("\n<== Fixpoint completed after iteration "++show iter++" ==>") $
             return env
        else fixpoint (iter+1) funs env'

-- | Apply a variable substitution to a type.
substTy :: Map LocVar LocVar -> Ty -> Ty
substTy mp t = go t
  where
    go t = 
     case t of
      IntTy  -> IntTy
      SymTy  -> SymTy
      BoolTy -> BoolTy
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
    dbgTrace 5 ("\n  Substituting in effects "++show(mp,ef)) $ 
    S.map (\(Traverse v) ->
               case M.lookup v mp of
                 Just v2 -> Traverse v2
                 Nothing -> Traverse v) ef

allLocVars :: Ty -> [LocVar]
allLocVars t =
    case t of
      SymTy     -> []
      BoolTy    -> []
      IntTy     -> []
      PackedTy _ v -> [v]
      ProdTy ls  -> L.concatMap allLocVars ls
      SymDictTy elt -> allLocVars elt    
               
freshenVar :: LocVar -> SyM LocVar
freshenVar = gensym
               
freshenArrowSchema :: ArrowTy Ty -> SyM (ArrowTy Ty)
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
instantiateApp :: ArrowTy Ty -> Loc -> SyM (Set Effect, Loc)
instantiateApp arrty0 loc = do
    (ArrowTy inT effs outT) <- freshenArrowSchema arrty0
    let subst = zipTL inT loc 
    dbgTrace lvl ("\n  instantiateApp: Came up with subst: "++show subst) $
     return (substEffs subst effs,
            dbgTraceIt lvl "   instantiate result loc" $ rettyToLoc (substTy subst outT))
  where
   -- Question: when computing the return location, which variables are Fresh?
   -- Conversely, when would we need to use Fixed?
   rettyToLoc :: Ty -> Loc
   rettyToLoc t =
     case t of
       IntTy  -> Bottom
       SymTy  -> Bottom
       BoolTy -> Bottom
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


-- Cursor types encoded into the current language
--------------------------------------------------------------------------------

-- Use a hack rather than extending the IR at this point:
cursorTy :: Ty
cursorTy = PackedTy "CURSOR_TY" ""

mkCursorTy :: LocVar -> Ty
mkCursorTy = PackedTy "CURSOR_TY" 

isCursorTy :: Ty -> Bool
isCursorTy (PackedTy "CURSOR_TY" _) = True
isCursorTy _ = False

cursorTyLoc :: Ty -> LocVar
cursorTyLoc (PackedTy "CURSOR_TY" l) = l
cursorTyLoc t = error $ "cursorTyLoc: should only be called on a cursor type, not "++show t
               
--------------------------------------------------------------------------------
                     
-- | Map every lexical variable in scope to an abstract location.
type Env = M.Map Var Loc

-- | Convert the type of a function argument to an abstract location
-- for that function argument.
argtyToLoc :: Var -> Ty -> Loc
argtyToLoc v ty =
 case ty of   
  PackedTy{}
    | isCursorTy ty -> Fixed $ cursorTyLoc ty
    | otherwise -> Fixed v
    -- ^ Here we set the type based on the variable binding name, not the
    -- quantified loc variable in the type signature.
  (ProdTy ls)   -> TupLoc [argtyToLoc (subloc v i) t | (t,i) <- zip ls [1..]]
   -- ^ Here we generate fixed locations that are *subparts* of the function argument.
  SymTy         -> Bottom
  IntTy         -> Bottom
  BoolTy        -> Bottom
  SymDictTy _t  -> -- ^ This may contain packed objects, but it is not contiguous.
    Fixed v
    -- if hasPacked t then Top else Bottom

-- | Do values of this type contain packed data?
hasPacked :: L1.Ty -> Bool
hasPacked t = case t of
                L1.Packed _  -> True
                L1.ProdTy ls -> any hasPacked ls
                L1.SymTy     -> False
                L1.BoolTy    -> False
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
             
inferFunDef :: (DDefs L1.Ty,FunEnv) -> C.FunDef L1.Ty L1.Exp -> SyM (Set Effect)
inferFunDef (ddefs,fenv) (C.FunDef name (arg,argty) _retty bod) =
    -- For this pass we don't need to know the output location:
    do argty' <- tyWithFreshLocs argty -- Temp.
       let ArrowTy inTy _ outTy = fenv # name
           env0    = M.singleton arg argLoc
           argLoc  = argtyToLoc (mangle arg) argty'

       (effs1,_loc) <- exp env0 bod

       -- Finally, restate the effects in terms of the type schema for the fun:
       let allEffs = substEffs (zipLT argLoc inTy) effs1
           externalLocs = S.fromList $ allLocVars inTy ++ allLocVars outTy
       return $ S.filter (\(Traverse v) -> S.member v externalLocs) allEffs

  where
  -- We have one location for the destination, and another for each lexical binding.
  exp :: Env -> L1.Exp -> SyM (Set Effect, Loc)
  exp env e =
    dbgTrace lvl ("\nProcessing exp: "++show e++"\n  with env: "++show env) $
    case e of
     -- QUESTION: does a variable reference count as traversing to the end?
     -- If so, the identity function has the traverse effect.
     -- I'd prefer that the identity function get type (Tree_p -> Tree_p).
     L1.VarE v  -> return (S.empty, env # v)
     L1.LitE  _ -> return (S.empty, Bottom)
     L1.CaseE e1 mp ->
      do (eff1,loc1) <- exp env e1
         (bools,effs,locs) <- unzip3 <$>
                              mapM (caserhs env) (M.toList mp)
         -- Critical policy point!  We only get to the end if ALL
         -- branches get to the end.
         let end = if all id bools
                   then case getLocVar loc1 of
                          Just v  -> S.singleton (Traverse v)
                          Nothing -> S.empty
                   else S.empty
         let (locFin,cnstrts) = joins locs

         when (not (L.null cnstrts)) $
           dbgTrace 1 ("Warning: FINISHME: process these constraints: "++show cnstrts) (return ())
                                
         return $ dbgTrace lvl ("\n==>Results on subcases: "++show (doc(bools,effs,locs))) $
                (S.union (S.union eff1 end)
                         (L.foldl1 S.intersection effs),
                 locFin)

     L1.IfE a b c -> 
         do (eff1,_loc1) <- exp env a
            (eff2,loc2) <- exp env b
            (eff3,loc3) <- exp env c             
            return (S.union eff1 (S.intersection eff2 eff3),
                    fst (join loc2 loc3))

     L1.TimeIt e -> exp env e

     -- Construct output packed data.  We will always "scroll to the end" of 
     -- output values, so they are not interesting for this effect analysis.
     L1.MkPackedE k ls -> L1.assertTrivs ls $
        -- And because it's freshly allocated, it has unconstrained location:
        do l <- freshLoc $ "mk"++k
           return (S.empty,l)

     -- Here we UNION the end-points that are reached in the RHS and the BOD:
     L1.LetE (v,_t,rhs) bod -> -- FIXME: change to let.
      do (reff,rloc) <- exp env rhs
         let env' = M.insert v rloc env 
         (beff,bloc) <- exp env' bod         
         return (S.union beff reff, bloc)

     -- We need to reach a fixed point where we jointly infer effects
     -- for all functions.
     L1.AppE rat rand ->
      case rand of
        L1.VarE vr -> 
          do let loc   = env # vr
             let arrTy = fenv # rat
             instantiateApp arrTy loc
        _ -> error$ "FINISHME: handle this rand: "++show rand

     -- If rands are already trivial, no traversal effects can occur here.
     L1.PrimAppE _ rands -> L1.assertTrivs rands $ 
         return (S.empty, Bottom) -- All primitives operate on non-packed data.
                          
     -- If any sub-expression reaches a destination, we can reach the destination:
     L1.MkProdE ls -> do (_effs,_locs) <- unzip <$> mapM (exp env) ls
                         error "FINISH mkprode"
     L1.ProjE _ e -> exp env e

--     L1.MkPacked k ls ->

  -- Returns true if this particular case reaches the end of the scrutinee.
  caserhs :: Env -> (Var,([Var],L1.Exp)) -> SyM (Bool, Set Effect, Loc)
  caserhs env (_dcon,([],erhs)) = do
     (effs,loc) <- exp env erhs
     return $ ( True, effs, loc)

  caserhs env (dcon,(patVs,erhs)) =
   -- Subtlety: if the rhs expression consumes the RIGHTMOST
   -- pattern variable, then the later code transformations MUST
   -- ensure that it consumes everything.
   do let tys    = lookupDataCon ddefs dcon
          zipped = fragileZip patVs tys
          freeRHS = L1.freeVars erhs
      env' <- extendEnv zipped env
          -- WARNING: we may need to generate "nested inside of" relation
          -- between the patVs and the scrutinee.      
      (eff,rloc) <- exp env' erhs
      let winner =           
           dbgTrace lvl ("\nInside caserhs, for "++show (dcon,patVs,tys)
                        ++ "\n  freevars "++show freeRHS
                        ++",\n  env "++show env'++",\n  eff "++show eff) $
           -- We've gotten "to the end" of a nullary constructor just by matching it:
           (L.null patVs) ||
           -- If there is NO packed child data, then our object has static size:
           (L.all (not . hasPacked) tys) ||
              let (lastV,lastTy) = last zipped
                  isUsed = S.member lastV freeRHS
              in
              case lastTy of
                -- If the last field is packed, then we better have
                -- traversed it in the RHS:
                L1.Packed{}    -> S.member (Traverse lastV) eff
                -- ANY usage of a fixed-sized last field requires
                -- traversal of packed data in the middle fields:
                L1.IntTy  -> isUsed
                L1.SymTy  -> isUsed
                L1.BoolTy -> isUsed
                L1.SymDictTy{} -> error "no SymDictTy allowed inside Packed"
                L1.ProdTy{}    -> error "no ProdTy allowed inside Packed"

          -- Also, in any binding form we are obligated to not return
          -- our local bindings in traversal side effects:                   
          isLocal (Traverse v) = L.elem v patVs -- FIXME... need LocVar
          stripped  = S.filter isLocal eff
      return ( winner, stripped, rloc )



-- | We extend the environment when going under lexical binders, which
-- always have fixed abstract locations associated with them.
extendEnv :: [(Var,L1.Ty)] -> Env -> SyM Env
extendEnv []    e     = return e
extendEnv ((v,t):r) e =
    do t' <- tyWithFreshLocs t -- Temp, just to call argtyToLoc.
       extendEnv r (M.insert v (argtyToLoc (mangle v) t') e)


-- Examples and Tests:
--------------------------------------------------------------------------------

_exadd1 :: Prog
_exadd1 = fst $ runSyM 0 $ inferEffects L1.add1Prog


--------------------------------------------------------------------------------


{-
cursorizeTy :: ArrowTy Ty -> ArrowTy T.Ty
cursorizeTy (ArrowTy inT ef ouT) =
  ArrowTy (appendArgs newIns  (replacePacked T.CursorTy inT))
          ef
          (appendArgs newOuts (replacePacked voidT ouT))
 where
  appendArgs [] t = t
  appendArgs ls t = T.ProdTy $ ls ++ [t]
  newOuts = replicate (S.size ef) T.CursorTy 
  newIns  = replicate (length outVs) T.CursorTy 
  outVs   = allLocVars ouT
  voidT   = T.ProdTy []          
  replacePacked (t2::T.Ty) (t::Ty) =
    case t of
      IntTy -> T.IntTy
      SymTy -> T.SymTy
      (ProdTy x)    -> T.ProdTy $ L.map (replacePacked t2) x
      (SymDictTy x) -> T.SymDictTy $ (replacePacked t2) x
      PackedTy{}    -> t2
-}
