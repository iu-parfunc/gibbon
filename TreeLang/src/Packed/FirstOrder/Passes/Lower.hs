{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}


-------------------------------------------------------------------------------

-- | Lowering L1 to the target language.
module Packed.FirstOrder.Passes.Lower
  ( lower
  ) where

-------------------------------------------------------------------------------

import Control.Monad
import Packed.FirstOrder.Common hiding (FunDef)
import qualified Packed.FirstOrder.L1_Source as L1
import qualified Packed.FirstOrder.LTraverse as L2
import           Packed.FirstOrder.LTraverse ( FunDef(..), Prog(..) )
import qualified Packed.FirstOrder.Target as T
import qualified Packed.FirstOrder.Passes.Cursorize as C
import Data.Maybe
import Data.List as L hiding (tail)
import Data.Map as M

import Prelude hiding (tail)

-------------------------------------------------------------------------------

-- | Convert into the target language.  This does not make much of a
-- change, but it checks the changes that have already occurred.
--
-- The only substantitive conversion here is of tupled arguments to
-- multiple argument functions.
lower :: Bool -> L2.Prog -> SyM T.Prog
lower pkd L2.Prog{fundefs,ddefs,mainExp} = do
  mn <- case mainExp of
          Nothing -> return Nothing
          Just x  -> (Just . T.PrintExp) <$> tail x
  T.Prog <$> mapM fund (M.elems fundefs) <*> pure mn
 where
  fund :: L2.FunDef -> SyM T.FunDecl
  fund L2.FunDef{funname,funty=(L2.ArrowTy inty _ outty),funarg,funbod} = do
      tl <- tail funbod
      return $ T.FunDecl { T.funName = funname
                         , T.funArgs = [(funarg, typ inty)]
                         , T.funRetTy = typ outty
                         , T.funBody = tl }

  tail :: L1.Exp -> SyM T.Tail
  tail ex =
   case ex of
    -- Packed codegen
    --------------------------------------------------------------------------------
    L1.CaseE e ls | pkd -> do
      __finish_packed_caseE

    -- These are in a funny normal form atfer cursor insertion.  They take one cursor arg.
    -- They basically are a WriteTag.
    L1.LetE (cursOut, _, L1.MkPackedE k ls) bod | pkd -> do
      let [cursIn] = ls
      T.LetPrimCallT [(cursOut,T.CursorTy)] T.WriteTag
                     [triv "WriteTag cursor" cursIn, T.IntTriv (getTagOfDataCon ddefs k)] <$>
        -- Here we lamely chase down all the tuple references and make them variables:
--        let bod' = __ $ substE (Proj ix (VarE tupname))
--            ix = 0 in
        tail bod
     
    -- Not-packed, pointer-based codegen
    --------------------------------------------------------------------------------
    -- If we get here that means we're NOT packing trees on this run:
    -- Thus this operates on BOXED data:
    L1.CaseE e [(c, bndrs, rhs)] | not pkd -> do
      -- a product, directly assign the fields
      let tys = L.map typ (lookupDataCon ddefs c)

      -- TODO(osa): enable this
      -- ASSERT(length tys == length bndrs)

      let T.VarTriv e_var = triv "case scrutinee" e
      rhs' <- tail rhs
      return (T.LetUnpackT (zip bndrs tys) e_var rhs')

    L1.CaseE _ _ -> error "Case on sum types not implemented yet."

    -- Accordingly, constructor allocation becomes an allocation.
    L1.LetE (v, _, L1.MkPackedE k ls) bod | not pkd -> L1.assertTrivs ls $ do
      -- is this a product?
      let all_cons = dataCons (lookupDDef ddefs k)
          is_prod  = length all_cons == 1
          tag      = fromJust (L.findIndex ((==) k . fst) all_cons)
          fields0  = L.map (triv "MkPackedE args") ls
          fields
            | is_prod   = T.TagTriv (fromIntegral tag) : fields0
            | otherwise = fields0

      bod' <- tail bod

      let tys = L.map typ (lookupDataCon ddefs k)
      return (T.LetAllocT v (zip tys (L.map (triv "MkPacked args") ls)) bod')

    --------------------------------------------------------------------------------


    e | L1.isTriv e -> pure$ T.RetValsT [triv "<internal error1>" e]

    -- L1.LetE (v,t, L1.MkProdE ls) bod -> do
    --   let rhss = L.map triv ls
    --   vsts <- unzipTup v t
    --   let go _ [] = tail bod
    --       go ix ((v1,t1):rst) = T.LetTrivT (v1,t1, )

    -- We could eliminate these ahead of time:
    L1.LetE (v,t,rhs) bod | L1.isTriv rhs -> T.LetTrivT (v,typ t, triv "<internal error2>" rhs) <$> tail bod

    -- TWO OPTIONS HERE: we could push equality prims into the target lang.
    -- Or we could map directly onto the IfEqT form:
    -- L1.IfE (L1.PrimAppE L1.EqP __ ) b c -> __

    L1.IfE a b c       -> do b' <- tail b
                             c' <- tail c
                             return $ T.Switch (triv "if test" a)
                                      -- If we are treating the boolean as a tag, then tag "0" is false
                                      (T.IntAlts [(0, c')])
                                      -- And tag "1" is true:
                                      (Just b')

    L1.AppE v e        -> return $ T.TailCall v [triv "operand" e]

    --------------------------------Start PrimApps----------------------------------
    -- (1) Primapps that become Tails:

    -- FIXME: No reason errors can't stay primitive at Target:
    L1.PrimAppE (L1.ErrorP str _ty) [] ->
      pure $ T.ErrT str
    L1.LetE (_,_,L1.PrimAppE (L1.ErrorP str _) []) _ ->
      pure $ T.ErrT str

    -- Whatever... a little just-in-time flattening.  Should obsolete this:
    L1.PrimAppE p ls -> do
      tmp <- gensym "flt"
      tail (L1.LetE (tmp, L1.primRetTy p, L1.PrimAppE p ls) (L1.VarE tmp))

    ---------------------           
    -- (2) Next FAKE Primapps.  These could be added to L1 if we wanted to pollute it.
    L1.LetE (v,_,C.WriteInt c e) bod ->
      T.LetPrimCallT [(v,T.CursorTy)] T.WriteInt [T.VarTriv c, triv "WriteTag arg" e] <$>
         tail bod

    ---------------------
    -- (3) Proper primapps.
    L1.LetE (v,t,L1.PrimAppE p ls) bod ->
        -- No tuple-valued prims here:
        T.LetPrimCallT [(v,typ t)]
             (prim p)
             (L.map (triv "prim rand") ls) <$>
             (tail bod)
    --------------------------------End PrimApps----------------------------------

             
    L1.LetE (v,t,L1.AppE f arg) bod -> do
        -- FIXME, tuples should be unzipped here:
        T.LetCallT [(v,typ t)] f
             [(triv "app rand") arg]
             <$>
             (tail bod)

    L1.LetE (v, t, L1.IfE a b c) bod -> do
      vsts <- unzipTup v t
      b' <- tail b
      c' <- tail c
      T.LetIfT vsts (triv "if test" a, b', c')
           <$> tail bod


    L1.TimeIt e ty ->
        do tmp <- gensym "timed"
           -- Hack: no good way to express EndTimer in the source lang:
           e' <- tail (L1.LetE (tmp, ty, e) (L1.VarE tmp))
           tm <- gensym "tmr"
           -- We splice in the end-timer post-facto:
           let endT = T.EndTimerT tm
           return $ T.StartTimerT tm $
            case e' of
             T.LetCallT   bnd rat rnds bod -> T.LetCallT   bnd rat rnds (endT bod)
             T.LetPrimCallT bnd p rnds bod -> T.LetPrimCallT bnd p rnds (endT bod)
             T.LetTrivT  bnd           bod -> T.LetTrivT           bnd  (endT bod)
             T.LetIfT bnd (tst,con,els) bod ->
                 T.LetIfT bnd (tst, endT con, endT els) (endT bod)
             _ -> error $ "lower: expected let binding back from recursive call:\n  "++sdoc e'

    _ -> error$ "lower: unexpected expression in tail position:\n  "++sdoc ex

unzipTup :: Var -> L1.Ty -> SyM [(Var,T.Ty)]
unzipTup v t =
  case t of
    L1.ProdTy ts -> do
      vs <- mapM (\_ -> gensym "uziptmp") ts
      return (zip vs (L.map typ ts))
    _ -> return [(v,typ t)]

triv :: String -> L1.Exp -> T.Triv
triv msg e0 =
  case e0 of
    (L1.VarE x) -> T.VarTriv x
    (L1.LitE x) -> T.IntTriv x
    -- Bools become ints:
    (L1.PrimAppE L1.MkTrue [])  -> T.IntTriv 1
    (L1.PrimAppE L1.MkFalse []) -> T.IntTriv 0
    -- TODO: I think we should allow tuples and projection in trivials:

    -- Heck, let's map Unit onto Int too:
    (L1.MkProdE []) -> T.IntTriv 0
--      (ProjE x1 x2) -> __
--      (MkProdE x) -> __
    _ | L1.isTriv e0 -> error $ "lower/triv: this function is written wrong.  "++
                         "It won't handle the following, which satisfies 'isTriv':\n "++sdoc e0
    _ -> error $ "lower/triv, expected trivial in "++msg++", got "++sdoc e0

typ :: L1.Ty1 a -> T.Ty
typ t =
  case t of
    L1.IntTy  -> T.IntTy
    L1.SymTy  -> T.SymTy
    L1.BoolTy -> T.IntTy
    L1.ListTy{} -> error "lower/typ: FinishMe: List types"
    L1.ProdTy xs -> T.ProdTy $ L.map typ xs
    L1.SymDictTy x -> T.SymDictTy $ typ x
    -- t | isCursorTy t -> T.CursorTy
    L1.PackedTy{} -> T.PtrTy

prim :: L1.Prim -> T.Prim
prim p =
  case p of
    L1.AddP -> T.AddP
    L1.SubP -> T.SubP
    L1.MulP -> T.MulP
    L1.EqSymP -> T.EqP
    L1.EqIntP -> T.EqP
    L1.DictInsertP -> T.DictInsertP
    L1.DictLookupP -> T.DictLookupP
    L1.DictEmptyP -> T.DictEmptyP
    L1.ErrorP{} -> error$ "lower/prim: internal error, should not have got to here: "++show p

    L1.MkTrue  -> error "lower/prim: internal error. MkTrue should not get here."
    L1.MkFalse -> error "lower/prim: internal error. MkFalse should not get here."
