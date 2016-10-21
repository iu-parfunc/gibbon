{-# LANGUAGE RecordWildCards #-}

-- | A compiler pass that translates between the L1 and L2 languages.
-- This implements the cursor-passing implementation of Packed tree
-- ADTs.

module Packed.HigherOrder.Translate where

import           Control.Monad.Identity
import           Control.Monad.State
import           Data.Map as M hiding (map)
import           Packed.HigherOrder.Common
import           Packed.HigherOrder.L1_Source       as S
import           Packed.HigherOrder.L2_Intermediate (L2,T2,P2)
import qualified Packed.HigherOrder.L2_Intermediate as T


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

-- | Compiler pass that lowers L1 to L2 by inserting cursor arguments.
insertCursors :: P1 -> P2
insertCursors P1{..} = T.P2 (fmap (fmap (runIdentity . doTy)) defs)
                            (fst $ runSyM 0 $ go CNone mainTy mainProg)
                            (runIdentity $ doTy mainTy)
 where
   -- This tracks the current return type and the cursors
   -- corresponding to that return type.
   go :: Cursors -> T1 -> L1 -> SyM T.L2
   go ctxt ty (App a b) =
     let argty = S.tyc (error "finishme52") b in
       if (error "finishme53")
          then T.App <$> go ctxt ty a <*> go ctxt ty b
          -- Inject the cursor argument
          else if (error "finishme56") -- No return context, make a fresh buffer.
                  then T.bind (error "finishme57") T.NewBuf $ 
                       \ (cur,_ty) ->
                        (T.App <$> (go ctxt (error "finishme59") a) 
                               <*> (T.App (T.Varref cur) <$>
                                         (go ctxt (error "finishme61") b)))
                  else T.App <$> (go ctxt (error "finishme62") a)
                             <*> (T.App (error "finishme63")
                                       <$> (go ctxt (error "finishme64") b))

   go _ctxt ty (Varref x) 
     | hasPacked ty = (error "finishme67")
     | otherwise    = (error "finishme68")
   go _ctxt _ty (Lit x) = (error "finishme69")
   go _ctxt _ty (Lam x1 x2) = (error "finishme70")
   go _ctxt _ty (CaseEither x1 x2 x3) = (error "finishme71")
   go _ctxt _ty (CasePacked x1 x2) = (error "finishme72")
   go _ctxt _ty (Add x1 x2) = (error "finishme73")
   go ctxt ty (Letrec (v,ty2,rhs) bod) = 
     do rhs' <- go CNone ty2 rhs
        bod' <- go ctxt ty bod
        return $ T.Letrec (v, runIdentity (doTy ty2), rhs') bod'

   go _ctxt _ty (InL x) = (error "finishme75")
   go _ctxt _ty (InR x) = (error "finishme76")
   go ctxt (Prod t1 t2) (MkProd x1 x2) =
     case ctxt of
       CProd c1 c2 -> T.MkProd <$> go c1 t1 x1    <*> go c2 t2 x2
       CNone       -> T.MkProd <$> go CNone t1 x1 <*> go CNone t2 x2 
       _           -> error "impossible"

   -- Here we must fetch the cursor from the context, and we also most
   -- transform the arguments to feed into the *same* cursor.
   go ctxt ty (MkPacked k ls) = 
     let bod c = T.MkPacked c k <$> (mapM (go2 c ty) ls)
     in case ctxt of
          Cursor c -> bod c
          CNone    -> do c <- gensym "c"
                         T.bind (error "finishme86") T.NewBuf $ \(c,_) -> bod c

   -- | This function takes a term of type T and returns one of type T
   -- that feeds a specific cursor.  This is called in the context of
   -- ARGUMENTS to a packed constructor.
   go2 :: Var -> T1 -> L1 -> SyM T.L2
   go2 c ty l1 =      
     case l1 of 
       (Varref v) -> return $ T.Copy v c
       (MkPacked k ls) -> T.MkPacked c k <$> mapM (go2 c undefined) ls
       (Lit x) -> (error "finishme95")
       (App x1 x2) -> (error "finishme96")
       (Lam x1 x2) -> (error "finishme97")
       (CaseEither x1 x2 x3) -> (error "finishme98")
       (CasePacked x1 x2) -> (error "finishme99")
       (Add x1 x2) -> (error "finishme100")
       (Letrec x1 x2) -> (error "finishme101")
       (InL x) -> (error "finishme102")
       (InR x) -> (error "finishme103")
       (MkProd x1 x2) -> (error "finishme104")




-- | Translate a type to route cursor parameters through.
doTy :: T1 -> Identity T2
doTy  = pos
 where
  -- In a positive position, cursors are passed as arguments.
  -- When returned, they must be added as output params.
  pos t = case t of
            TInt          -> return T.TInt 
            (TArr x y)    -> T.TArr <$> pos x <*> neg y
            (TyVar x)     -> return $ T.TyVar x
            (Prod x1 x2)  -> T.Prod <$> pos x1 <*> pos x2
            (Sum x1 x2)   -> T.Sum  <$> pos x1 <*> pos x2
            (Packed k ls) -> T.Packed k <$> mapM pos ls
  -- This assumes mutable cursors and thus a "returned" packed value
  -- is just void.
  neg t = case t of 
            TInt           -> return T.TInt
            (TArr x1 x2)   -> T.TArr <$> pos x1 <*> neg x2
            (TyVar x)      -> return $ T.TyVar x
            (Prod x1 x2)   -> T.Prod <$> neg x1 <*> neg x2
            (Sum x1 x2)    -> T.Sum  <$> neg x1 <*> neg x2
            (Packed x1 x2) -> return T.TVoid

-- Examples:
--------------------------------------------------------------------------------

-- | Simplest program: a literal.
ex0 :: P1
ex0 = P1 { defs = emptyDD
         , mainTy = TInt
         , mainProg = Lit 33 }

-- | Next: a packed literal.
ex0b :: P1
ex0b = P1 { defs = fromListDD [DDef "T" [] [("K1",[])] ]
          , mainTy = Packed "T" []
          , mainProg = (MkPacked "K1" []) }

t0b :: P2
t0b = insertCursors ex0b

-- | And with a nested constructor.
ex0c :: P1
ex0c = P1 { defs = fromListDD [DDef "Nat" [] [ ("Suc",[Packed "Nat" []])
                                             , ("Zer",[])] ]
          , mainTy = Packed "Nat" []
          , mainProg = (MkPacked "Suc" [MkPacked "Zer" []]) }

t0c :: P2
t0c = insertCursors ex0c

-- | And inside a product.      
ex0d :: P1
ex0d = P1 { defs = fromListDD [DDef "Nat" [] [ ("Suc",[Packed "Nat" []])
                                             , ("Zer",[])] ]
          , mainTy = (Prod (Packed "Nat" []) (Packed "Nat" []))
          , mainProg = MkProd (MkPacked "Suc" [MkPacked "Zer" []])
                              (MkPacked "Zer" []) }

-- | An example with sharing.
ex0e :: P1
ex0e = P1 { defs = fromListDD [DDef "Nat" [] [ ("Suc",[Packed "Nat" []])
                                             , ("Zer",[])] ]
          , mainTy = (Prod (Packed "Nat" []) (Packed "Nat" []))
          , mainProg = Letrec ("shrd",Packed "Nat" [],
                               (MkPacked "Suc" [MkPacked "Zer" []])) $
                       MkProd (Varref "shrd") (Varref "shrd") }
       
       
----------------------------------------
        
-- | A basic identity function
ex1 :: P1
ex1 = P1 { defs = emptyDD
         , mainTy = TInt
         , mainProg = Letrec ("f", TArr TInt TInt, Lam ("x",TInt) (Varref "x"))
                                       (App (Varref "f") (Lit 33)) }

-- | Next, an identity function on a packed type.  Because packed
--   types are by value (modulo optimizations), this is a copy.
ex2 :: P1
ex2 = P1 { defs = fromListDD [DDef "T" [] [("K1",[])] ]
         , mainTy = TInt
         , mainProg = Letrec ("f", TArr TInt TInt, Lam ("x",TInt) (Varref "x"))
                             (App (Varref "f") (MkPacked "K1" [])) }

 
