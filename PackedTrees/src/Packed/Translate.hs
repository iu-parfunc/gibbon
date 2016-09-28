{-# LANGUAGE RecordWildCards #-}

-- | A compiler pass that translates between the source and target
-- language, implementing the cursor-passing implementation of Packed
-- tree ADTs.

module Packed.Translate where

import Packed.Common
import           Packed.L1_Source       as S
import qualified Packed.L2_Intermediate as T
import Packed.L2_Intermediate (L2,T2,P2)
import Data.Map as M
    
-- | Used during compilation to describe the outstanding return context.
data Context = Empty
             | InRight Context | InLeft Context
             | InFst   Context | InSnd  Context
             | CtxtCursor 

-- | A package of cursors corresponding to a return type.
data Cursors = Cursor Var
             | CProd Cursors Cursors
             | CSum  Cursors Cursors
             | CNone 

type Env = Map Var Cursors


hasPacked :: T1 -> Bool
hasPacked = go
 where
  go x = case x of
           TInt           -> False
           (TyVar a)      -> False
           (Packed a1 a2) -> True
           (TArr a1 a2) -> go a1 || go a2
           (Prod a1 a2) -> go a1 || go a2
           (Sum a1 a2)  -> go a1 || go a2

insertCursors :: P1 -> P2
insertCursors P1{..} = undefined (go mainTy M.empty mainProg)
 where
   go :: T1 -> Env -> L1 -> T.L2
   go ty env (App a b) =
       if undefined
          then T.App (go ty env a) (go ty env b)
          -- Inject the cursor argument
          else if undefined -- No return context, make a fresh buffer.
                  then T.NewBuf `T.Bind`
                       T.Lam ("c",undefined) 
                             (T.App (go undefined env a) 
                                    (T.App (T.Varref "v") (go undefined env b)))
                  else T.App (go undefined env a) 
                             (T.App undefined (go undefined env b))

   go ty env (Varref x) 
     | hasPacked ty = undefined
     | otherwise = undefined
   go ty env (Lit x) = undefined
   go ty env (Lam x1 x2) = undefined
   go ty env (CaseEither x1 x2 x3) = undefined
   go ty env (CasePacked x1 x2) = undefined
   go ty env (Add x1 x2) = undefined
   go ty env (Letrec x1 x2) = undefined
   go ty env (InL x) = undefined
   go ty env (InR x) = undefined
   go ty env (MkProd x1 x2) = undefined
   go ty env (MkPacked x1 x2) = undefined

-- | Translate a type to route through cursor parameters.
doTy :: T1 -> IO T2 -- (T2,[()])
doTy  = pos
 where
  -- In a positive position, cursors are passed as arguments.
  -- When returned, they must be added as output params.
  pos t = case t of
            TInt -> undefined
            (TArr x y) -> T.TArr <$> pos x <*> neg y
            (TyVar x)    -> undefined
            (Prod x1 x2) -> undefined
            (Sum x1 x2)    -> undefined
            (Packed k ls) -> T.Packed k <$> mapM pos ls
  neg t = case t of 
            TInt -> undefined
            (TArr x1 x2) -> undefined
            (TyVar x)    -> undefined
            (Prod x1 x2) -> undefined
            (Sum x1 x2)    -> undefined
            (Packed x1 x2) -> undefined

-- Examples:
--------------------------------------------------------------------------------

-- | A basic identity function
ex1 :: P1
ex1 = P1 { defs = []
         , mainTy = TInt
         , mainProg = Letrec ("f", TArr TInt TInt, Lam ("x",TInt) (Varref "x"))
                                       (App (Varref "f") (Lit 33)) }

-- | Next, an identity function on a packed type.
--   Because packed types are by value (ish), this is a copy.   
ex2 :: P1
ex2 = P1 { defs = [DDef "T" [("K1",[])]]
         , mainTy = TInt
         , mainProg = Letrec ("f", TArr TInt TInt, Lam ("x",TInt) (Varref "x"))
                             (App (Varref "f") (MkPacked "K1" [])) }
