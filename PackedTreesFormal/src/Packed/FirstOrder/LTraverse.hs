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
    
-- | When processing an expression, our output goes to a certain set
-- of abstract locations.  The shape of the context is based on the
-- shape of the Type.
data Context = EmptyCtxt
--             | InFst Context | InSnd  Context
             | TupCtxt [Context]
             | OutCtxt LocVar
    
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

-- | Take a polymorphic ArrowTy and instantiate its location variables
-- and traversal effects with concrete values for the 
instantiateEffs :: ArrowTy -> LocVar -> ()
instantiateEffs arr l = undefined $ go undefined undefined
  where
   go (OutCtxt l) (PackedTy _ v) = M.singleton v l
   go EmptyCtxt _                = M.empty
   go (TupCtxt l1)  (ProdTy l2)  = M.unions (zipWith go l1 l2)


-- | Map every lexical variable in scope to an abstract location.
type Env = M.Map Var Loc

-- | Convert the type of a function argument to an abstract location
-- for than function argument.
tyToLoc :: Var -> L1.Ty -> Loc
tyToLoc v (L1.Packed _) = Fixed v
 -- ^ Here we set the type based on the variable binding name, not the
 -- quantified loc variable in the type signature.
tyToLoc v (L1.ProdTy ls) = TupLoc [tyToLoc (subloc v i) t | (t,i) <- zip ls [1..]]
 -- ^ Here we generate fixed locations that are *subparts* of the function argument.
tyToLoc v L1.SymTy        = Bottom
tyToLoc v L1.IntTy        = Bottom
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

                                  
inferEffects :: FunEnv -> C.FunDef L1.Ty L1.Exp -> SyM (Set Effect)
inferEffects fenv (C.FunDef _name (arg,argty) retty bod) =
    exp outloc0 env0 bod
  where
  env0    = M.singleton arg (tyToLoc (mangle arg) argty)
  outloc0 = tyToLoc "out" retty

  -- We have one location for the destination, and another for each lexical binding.
  exp :: Loc -> Env -> L1.Exp -> SyM (Set Effect)
  exp out env e = 
    case e of 
     L1.VarE  _ -> pure S.empty
     L1.LitE  _ -> pure S.empty
     L1.CaseE e1 mp ->
      do eff1 <- exp out env e1
         effs <- mapM (rhs out env) (M.elems mp)
         -- Critical policy point!  We only get to the end if all
         -- branches get to the end.
         return $ S.union eff1 undefined

     L1.MkPackedE k ls -> undefined
          
     -- We need to reach a fixed point where we infer effects for all
     -- functions at once:
     L1.AppE rat (L1.VarE rand) | Just loc <- M.lookup rand env -> 
         let arrTy = fenv M.! rat
             _ = instantiateEffs arrTy rand
         in
         -- Assume rands are trivial.
         -- The traversal effects are inherited based on the type of rator.
         undefined

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
