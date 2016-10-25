{-# LANGUAGE DeriveGeneric #-}

-- | An intermediate language with an effect system that captures traversals.

module Packed.FirstOrder.LTraverse where

import Packed.FirstOrder.Common 
import qualified Packed.FirstOrder.L1_Source as L1
import Data.List as L
import Data.Set as S
import Data.Map as M
import GHC.Generics (Generic)
    
--------------------------------------------------------------------------------
    
-- Unchanged from L1, or we could go A-normal:
-- data Exp =

-- | Abstract location variables.
type LocVar = Var

-- data L1 = Varref Var | Lit Int
--         | App Var L1 -- Only apply top-level / first-order functions
--         | PrimApp Prim [L1]
--         | Letrec (Var,T1,L1) L1
--           -- ^ One binding at a time, but could bind a tuple for
--           -- mutual recursion.
--         | Fst L1 | Snd L1 | MkProd L1 L1
--         | CasePacked L1 (M.Map Constr ([Var], L1))
--         | MkPacked Constr [L1]
--   deriving (Read,Show,Eq,Ord, Generic)


-- Our type for functions grows to include effects.
data ArrowTy = ArrowTy [Ty] (Set Effect) Ty
  deriving (Read,Show,Eq,Ord, Generic)

data Effect = Traverse LocVar
  deriving (Read,Show,Eq,Ord, Generic)


-- | When processing an expression, our output goes to a certain set
-- of abstract locations.  The shape of the context is based on the
-- shape of the Type.
data Context = EmptyCtxt
--             | InFst Context | InSnd  Context
             | TupCtxt Context Context
             | OutCtxt LocVar

           
type Env = M.Map Var LocVar

type Funs = FunDefs L1.Ty L1.Exp
type FunEnv = M.Map Var ArrowTy

-- | L1 Types extended with abstract Locations
data Ty = IntTy | SymTy | ProdTy [Ty] | SymDictTy Ty  
        | Packed { con :: Constr, loc :: LocVar }
  deriving (Show, Read, Ord, Eq, Generic)

data Prog = Prog

--------------------------------------------------------------------------------
    

-- | We initially populate all functions with empty effect signatures.
--   We also associate fresh location variables with packed types.
initialEnv :: Funs -> FunEnv
initialEnv mp = M.map (\x -> fst $ runSyM 0 (go x))  mp
  where
    go :: FunDef L1.Ty L1.Exp -> SyM ArrowTy
    go (FunDef _ ret args _)  =
        do argTys <- mapM annotateTy (L.map snd args)
           retTy  <- annotateTy ret
           return $ ArrowTy argTys S.empty retTy
                                    
-- (L1.FunDef name retty args bod)

-- | Annotate a naked type with location
annotateTy :: L1.Ty -> SyM Ty
annotateTy t =
  case t of
    L1.Packed k -> Packed k <$> genLetter                   
    L1.IntTy    -> return IntTy
    L1.SymTy    -> return SymTy
    L1.ProdTy l -> ProdTy <$> mapM annotateTy l
    L1.SymDictTy v -> SymDictTy <$> annotateTy v

inferProg :: L1.Prog -> Prog
inferProg = undefined
    
inferEffects :: FunEnv -> FunDef L1.Ty L1.Exp -> Set Effect
inferEffects fenv (FunDef name retty args bod) = exp outloc env0 bod
  where
  env0 = undefined
  outloc = undefined

-- triv = 
           
  -- The environment gives us abstract locations for variables.
  exp :: Context -> Env -> L1.Exp -> Set Effect
  exp out env e = 
    case e of 
     L1.VarE  _ -> S.empty
     L1.LitE  _ -> S.empty
     L1.CaseE e1 mp ->
         let eff1 = exp out env e1
             effs = L.map (rhs out env) (M.elems mp)
         in
         -- Critical policy point!  We only get to the end if all
         -- branches get to the end.
         S.union eff1 
          undefined

     -- We need to reach a fixed point where we infer effects for all
     -- functions at once:
     L1.AppE rat rand -> triv rand $ 
         -- Assume rands are trivial.
         -- The traversal effects are inherited based on the type of rator.
         undefined

     -- If rands are already trivial 
     L1.PrimAppE _ rands -> trivs rands $          
         S.empty
                          
     -- Here we UNION the end-points that are reached in the RHS and the BOD:
     L1.LetE (v,t,rhs) bod -> -- FIXME: change to let.
         let env' = extendEnv [v] env
             out' = undefined -- ?? create based on type.
         in
         S.union (exp out env' bod)
                 (exp out' env rhs)

     -- If any sub-expression reaches a destination, we can reach the destination:
     L1.MkTupE ls -> S.unions (L.map (exp out env) ls)
     L1.ProjE _ e -> exp out env e

--     L1.MkPacked k ls ->
                     
  rhs :: Context -> Env -> ([Var], L1.Exp) -> Set Effect
  rhs out env ([], erhs) = addOuts out (exp out env erhs)
  rhs out env (patVs, erhs) =
      -- Subtlety: if the rhs expression consumes the RIGHTMOST
      -- pattern variable, then the later code transformations will
      -- have to ensure that it consumes everything.
      let env' = extendEnv patVs env
          eff  = exp out env' erhs
          isLocal (Traverse v) = L.elem v patVs -- FIXME... need LocVar
          stripped  = S.filter isLocal eff
      in
      if S.member (Traverse (L.last patVs)) eff
      then addOuts out stripped
      else stripped

-- Simple invariant assertions:
           
triv :: L1.Exp -> a -> a
triv = undefined

trivs :: [L1.Exp] -> a -> a
trivs = undefined

addOuts :: Context -> Set Effect -> Set Effect
addOuts EmptyCtxt     = id
addOuts (OutCtxt v)   = S.insert (Traverse v) 
addOuts (TupCtxt x y) = addOuts x . addOuts y 

           
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
