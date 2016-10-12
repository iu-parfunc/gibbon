{-# LANGUAGE DeriveGeneric #-}
-- | An intermediate language with cursors but not explicit memory
-- representations.  

module Packed.L2_Intermediate where

import Control.Monad.Writer hiding (Sum)
import Data.Map as M
import GHC.Generics
import Packed.Common
import Text.PrettyPrint.GenericPretty

    
-- | A monadic intermediate language.  This hides the details of
-- packed-adt representation, but it exposes a "cursor" argument to
-- every tree constructor, and it makes constructing tree values an IO
-- action.
data L2 = Varref Var | Lit Int | Void
        | App L2 L2
        | Lam (Var,T2) L2
        | CaseEither L2 L2 L2
        | CasePacked L2 [([Var], L2)]
        | Add L2 L2 -- One primitive.
        | Letrec (Var,T2,L2) L2
        | InL L2 | InR L2 | MkProd L2 L2

        -- NEW: Monadic operations:
-- TEMP: doing an impure/call-by-value target language first:
--        | Bind L2 L2
--        | Return L2
        | NewBuf             -- ^ Allocate a new buffer (could take size)
        | MkPacked CursorVar Constr  [L2]
        -- ^ CHANGED: We have a required cursor parameter to every constructor:
        | Copy { src :: Var, dst:: Var }
           -- ^ A recursive, polymorphic copy operation on any Packed type.

  deriving (Read,Show,Ord,Eq, Generic)

instance Out T2
instance Out L2
instance Out P2
           
-- A smart constructor:
bind :: TEnv -> L2 -> ((Var, T2) -> SyM L2) -> SyM L2
bind tenv a1 fn = do tmp <- gensym "t"
                     let ty = tyc tenv a1
                     bod <- fn (tmp,ty)
                     return $ Letrec (tmp,ty,a1) bod                                     
-- Monadic version:
-- bind tenv a1 fn = do tmp <- gensym "t"
--                      let ty = tyc tenv a1
--                      return $ a1 `Bind`
--                               Lam (tmp,ty) (fn (tmp,ty))

type TEnv = Map Var T2

data T2 = TInt | TArr T2 T2 | TyVar Var | TVoid
        | Prod T2 T2 | Sum T2 T2
        | Packed Var [T2] 
        -- NEW:
        | TCursor 
        | TIO T2 -- ^ an IO action.
  deriving (Read,Show,Ord,Eq,Generic)

-- | Typecheck and return the type of the input expression:
tyc :: TEnv -> L2 -> T2
tyc = go
  where
  go tenv e = 
   case e of
    (Varref x)  -> tenv ! x
    (Lit _)     -> TInt
    (Copy _ _)  -> TCursor
    (Add _ _)   -> TInt
    (App x1 x2) -> let TArr arg b = go tenv x1
                       tyRand     = go tenv x2
                   in runUnify $ do _ <- unify tyRand arg
                                    return b

    (Lam (v,t) bod) -> TArr t (go (M.insert v t tenv) bod)
    NewBuf -> TCursor -- Not monadic yet!  Should probably be a unit function then.
    (InL x) -> undefined
    (InR x) -> undefined

    (CaseEither x1 x2 x3) -> undefined
    (CasePacked x1 x2) -> undefined
    (Letrec x1 x2) -> undefined
    (MkProd x1 x2) -> undefined

    (MkPacked x1 x2 x3) -> undefined

type Constraint = (Var,T2)

-- | Complete a unification session by solving all constraints.
--   Apply those equations to the returned type.
runUnify :: Writer [Constraint] T2 -> T2
runUnify = undefined
            
unify :: T2 -> T2 -> Writer [Constraint] T2
unify = go
  where
  -- Strang GHC error (FlexibleContexts) if this type sig is ommitted:
  go :: T2 -> T2 -> Writer [Constraint] T2
  go t1 t2 =
   let err = error $ "Types do not unify:\n  "++show t1++"\n  "++show t2 in
   case (t1,t2) of
    (_,_) | t1 == t2                   -> return t1
    ((TyVar x1),x2) -> do tell [(x1,x2)]; return x2
    (x1, TyVar x2)  -> do tell [(x2,x1)]; return x1
    ((TArr x11 x12),(TArr x21 x22)) -> TArr <$> go x11 x21 <*> go x12 x22
    ((Prod x11 x12),(Prod x21 x22)) -> Prod <$> go x11 x21 <*> go x12 x22
    ((Sum x11 x12),(Sum x21 x22))   -> Sum  <$> go x11 x21 <*> go x12 x22
    ((TIO x1),(TIO x2))             -> TIO  <$> go x1 x2
    ((Packed k1 ls1),(Packed k2 ls2)) ->
      if k1 /= k2
      then err
      else Packed k1 <$> mapM (uncurry go) (zip ls1 ls2)
    _ -> err
        
                                           
-- | Complete programs include datatype definitions:
data P2 = P2 { defs :: DDefs T2
             , mainProg :: L2
             , mainTy   :: T2 }
  deriving (Read,Show,Eq,Ord,Generic)

--------------------------------------------------------------------------------

interp :: P2 -> Value L2
interp = undefined
