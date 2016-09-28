{-# LANGUAGE RecordWildCards #-}

-- | A compiler pass that translates between the source and target
-- language, implementing the cursor-passing implementation of Packed
-- tree ADTs.

module Packed.Translate where

import           Control.Monad.State
import           Data.Map as M hiding (map)
import           Packed.Common
import           Packed.L1_Source       as S
import           Packed.L2_Intermediate (L2,T2,P2)
import qualified Packed.L2_Intermediate as T


-- | Used during compilation to describe the outstanding return context.
data Context = Empty
             | InRight Context | InLeft Context
             | InFst   Context | InSnd  Context
             | CtxtCursor Var

-- | A package of cursors corresponding to a return type.
data Cursors = Cursor Var
             | CProd Cursors Cursors
             | CSum  Cursors Cursors
             | CNone 

type CEnv = Map Var Cursors


hasPacked :: T1 -> Bool
hasPacked = go
 where
  go x = case x of
           TInt           -> False
           (TyVar _)      -> False
           (Packed _ _) -> True
           (TArr a1 a2) -> go a1 || go a2
           (Prod a1 a2) -> go a1 || go a2
           (Sum a1 a2)  -> go a1 || go a2

insertCursors :: P1 -> P2
insertCursors P1{..} = undefined (go CNone mainTy M.empty mainProg)
 where
   go :: Cursors -> T1 -> CEnv -> L1 -> SyM T.L2
   go ctxt ty env (App a b) =
     let argty = S.tyc undefined b in
       if undefined
          then T.App <$> go ctxt ty env a <*> go ctxt ty env b
          -- Inject the cursor argument
          else if undefined -- No return context, make a fresh buffer.
                  then T.bind undefined T.NewBuf $ 
                       \ (cur,_ty) ->
                        (T.App <$> (go ctxt undefined env a) 
                               <*> (T.App (T.Varref cur) <$> (go ctxt undefined env b)))
                  else T.App <$> (go ctxt undefined env a)
                             <*> (T.App undefined <$> (go ctxt undefined env b))

   go _ctxt ty _env (Varref x) 
     | hasPacked ty = undefined
     | otherwise    = undefined
   go _ctxt _ty _env (Lit x) = undefined
   go _ctxt _ty _env (Lam x1 x2) = undefined
   go _ctxt _ty _env (CaseEither x1 x2 x3) = undefined
   go _ctxt _ty _env (CasePacked x1 x2) = undefined
   go _ctxt _ty _env (Add x1 x2) = undefined
   go _ctxt _ty _env (Letrec x1 x2) = undefined
   go _ctxt _ty _env (InL x) = undefined
   go _ctxt _ty _env (InR x) = undefined
   go _ctxt _ty _env (MkProd x1 x2) = undefined
   
   -- Here we must fetch the cursor from the context, and we also most
   -- transform the arguments into functions of a cursor argument.
   go ctxt ty env (MkPacked k ls) = 
     let Cursor c = ctxt in 
     T.MkPacked c k <$> (mapM (go2 (Cursor c) ty env) ls)

   -- | This function takes a term of type T and returns one of type
   -- (Cursor -> T).  It should hold that (hasPacked T == True).
   go2 :: Cursors -> T1 -> CEnv -> L1 -> SyM T.L2
   go2 c ty env (Varref v) = return $ T.Copy v undefined


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

-- | Simplest program: a literal.
ex0 :: P1
ex0 = P1 { defs = []
         , mainTy = TInt
         , mainProg = Lit 33 }

-- | Next: a packed literal.
ex0b :: P1
ex0b = P1 { defs = [DDef "T" [] [("K1",[])] ]
          , mainTy = Packed "T" []
          , mainProg = (MkPacked "K1" []) }


-- | A basic identity function
ex1 :: P1
ex1 = P1 { defs = []
         , mainTy = TInt
         , mainProg = Letrec ("f", TArr TInt TInt, Lam ("x",TInt) (Varref "x"))
                                       (App (Varref "f") (Lit 33)) }

-- | Next, an identity function on a packed type.  Because packed
--   types are by value (modulo optimizations), this is a copy.
ex2 :: P1
ex2 = P1 { defs = [DDef "T" [] [("K1",[])] ]
         , mainTy = TInt
         , mainProg = Letrec ("f", TArr TInt TInt, Lam ("x",TInt) (Varref "x"))
                             (App (Varref "f") (MkPacked "K1" [])) }

