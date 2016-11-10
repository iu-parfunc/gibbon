
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

import Data.Char
import Control.Monad
import Packed.FirstOrder.Common hiding (FunDef)
import qualified Packed.FirstOrder.L1_Source as L1
import           Packed.FirstOrder.L1_Source (Exp(..))
import qualified Packed.FirstOrder.LTraverse as L2
import           Packed.FirstOrder.LTraverse ( FunDef(..), Prog(..) )
import qualified Packed.FirstOrder.Target as T
import qualified Packed.FirstOrder.Passes.Cursorize as C
import Data.Maybe
import Data.List as L hiding (tail)
import Data.Map as M

import qualified Prelude as P
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
          Nothing    -> return Nothing
          Just (x,_) -> (Just . T.PrintExp) <$> tail x
  T.Prog <$> mapM fund (M.elems fundefs) <*> pure mn
 where
  fund :: L2.FunDef -> SyM T.FunDecl
  fund L2.FunDef{funname,funty=(L2.ArrowTy inty _ outty),funarg,funbod} = do                                             
      (args,bod) <- dbgTrace 1 ("Inspecting function with input type: "++show inty) $
                    case inty of
                      -- ASSUMPTION: no nested tuples after unariser:
                      L2.ProdTy ls -> do let tys'  = L.map (fmap (const ())) ls
                                             tys'' = L.map typ ls
                                         (vs,e') <- eliminateProjs funarg tys' funbod
                                         return $
                                          dbgTrace 5 (" [lower] unzipping funarg "++show funarg++" to "++show vs) $
                                          (zip vs tys'', e')
                      _ -> return ([(funarg, typ inty)], funbod)
      tl <- tail bod
      return T.FunDecl{ T.funName = funname
                      , T.funArgs = args
                      , T.funRetTy = typ outty
                      , T.funBody = tl }

  tail :: L1.Exp -> SyM T.Tail
  tail ex =
   dbgTrace 5 ("\n [lower] processing tail:\n  "++sdoc ex) $
   case ex of

    -- HACK! We don't have LetSwitchT yet.  This means potential exponential code duplication:
    -- LetE (_,_, CaseE _ _) _ ->
    --    error "lower: unfinished, we cannot let-bind the result of a switch yet."
    LetE (vr,ty, CaseE scrt ls) bod -> tail $
                                       dbgTrace 1 ("WARNING: Let-bound CasE, code duplication of this body:\n  "
                                                   ++sdoc bod)$
         -- For now just duplicate code:
         CaseE scrt [ (k,vs, mkLet (vr,ty,e) bod)
                    | (k,vs,e) <- ls]

    -- Aaand... if we're going to push Let's under Case's, we have to repeat this bit of flattening:
    LetE (v1, t1, LetE (v2,t2,rhs2) rhs1) bod ->
       tail $ LetE (v2,t2,rhs2) $ LetE (v1,t1,rhs1) bod

    --------------------------------------------------------------------------------
    -- Packed codegen
    --------------------------------------------------------------------------------
    -- These are in a funny normal form atfer cursor insertion.  They take one cursor arg.
    -- They basically are a WriteTag.
    LetE (cursOut, _, MkPackedE k ls) bod | pkd -> do
      case ls of
       [cursIn] -> T.LetPrimCallT [(cursOut,T.CursorTy)] T.WriteTag
                     [ T.TagTriv (getTagOfDataCon ddefs k)
                     , triv "WriteTag cursor" cursIn ] <$>
                    tail bod
       _ -> error$ "Lower: Expected one argument to data-constructor (which becomes WriteTag): "
                   ++sdoc (MkPackedE k ls)

    -- Likewise, Case really means ReadTag.  Argument is a cursor.
    CaseE (VarE scrut) ls | pkd -> do
        let (last:restrev) = reverse ls; rest = reverse restrev
        tagtmp <- gensym "tmpval"
        ctmp   <- gensym "tmpcur"
        -- We only need to thread one value through, the cursor resulting from read.
        let doalt (k,ls,rhs) =
             (getTagOfDataCon ddefs k,) <$>
             case ls of
               []  -> tail rhs -- AUDITME -- is this legit, or should it have one cursor param anyway?
               [c] -> tail (L1.subst c (VarE ctmp) rhs)
        alts <- mapM doalt rest
        (_,last') <- doalt last
        return $
         T.LetPrimCallT [(tagtmp,T.TagTy),(ctmp,T.CursorTy)] T.ReadTag [T.VarTriv scrut] $
          T.Switch (T.VarTriv tagtmp)
                   (T.TagAlts alts)
                   (Just last')

    --------------------------------------------------------------------------------
    -- Not-packed, pointer-based codegen
    --------------------------------------------------------------------------------
    -- If we get here that means we're NOT packing trees on this run:
    -- Thus this operates on BOXED data:
    CaseE e [(c, bndrs, rhs)] | not pkd -> do
      -- a product, directly assign the fields
      let tys = L.map typ (lookupDataCon ddefs c)

      -- TODO(osa): enable this
      -- ASSERT(length tys == length bndrs)

      let T.VarTriv e_var = triv "case scrutinee" e
      rhs' <- tail rhs
      return (T.LetUnpackT (zip bndrs tys) e_var rhs')

    CaseE e (def_alt : alts) | not pkd -> do
      tag <- gensym "tag"
      buf0 <- gensym "buf"
      let e_triv = triv "case scrutinee" e

      let
        bind_tag = T.LetPrimCallT [(tag, T.TagTy), (buf0, T.CursorTy)] T.ReadTag [e_triv]

        mk_alt (con, bndrs, rhs) = do
          rhs_triv <- tail rhs
          ptr_vars <- (buf0 :) <$> replicateM (length bndrs) (gensym "buf")
          let ptr_vars' = zip ptr_vars (P.tail ptr_vars)

          let
            bndr_tys = L.map typ (lookupDataCon ddefs con)
            con_tag  = getTagOfDataCon ddefs con

            rhs = Prelude.foldr
                    (\(bndr, _bndr_ty, (cur_ptr, next_ptr)) body ->
                      -- TODO: bndr type is not used here because Target is
                      -- is assuming it's an int
                      T.LetPrimCallT [(bndr, T.IntTy), (next_ptr, T.CursorTy)] T.ReadInt [T.VarTriv cur_ptr] body)
                    rhs_triv (zip3 bndrs bndr_tys ptr_vars')

          return (con_tag, rhs)

      alts' <- mapM mk_alt alts
      (_, def_alt') <- mk_alt def_alt

      return $ bind_tag $ T.Switch (T.VarTriv tag) (T.TagAlts alts') (Just def_alt')

    -- Accordingly, constructor allocation becomes an allocation.
    LetE (v, _, MkPackedE k ls) bod | not pkd -> L1.assertTrivs ls $ do
      -- is this a product?
      let tycon    = getTyOfDataCon ddefs k
          all_cons = dataCons (lookupDDef ddefs tycon)
          is_prod  = length all_cons == 1
          tag      = fromJust (L.findIndex ((==) k . fst) all_cons)
          fields0  = L.map (triv "MkPackedE args") ls
          fields
            | is_prod   = fields0
            | otherwise = T.TagTriv (fromIntegral tag) : fields0

      bod' <- tail bod

      let tys = L.map typ (lookupDataCon ddefs k)
      return (T.LetAllocT v (zip tys fields) bod')

    --------------------------------------------------------------------------------

    L1.MkProdE ls -> pure$ T.RetValsT (L.map (triv "returned element of tuple") ls)
    e | L1.isTriv e -> pure$ T.RetValsT [triv "<internal error1>" e]

    -- L1.LetE (v,t, L1.MkProdE ls) bod -> do
    --   let rhss = L.map triv ls
    --   vsts <- unzipTup v t
    --   let go _ [] = tail bod
    --       go ix ((v1,t1):rst) = T.LetTrivT (v1,t1, )

    -- We could eliminate these ahead of time (unariser):
    -- FIXME: Remove this when that is done a priori:
    L1.LetE (v, L2.ProdTy tys, MkProdE ls) bod -> do
      (tmps,bod') <- eliminateProjs v tys bod
      -- Bind tmps individually:a
      let go [] acc                 = acc
          go ((pvr,pty,rhs):rs) acc = go rs (LetE (pvr,pty,rhs) acc)
      -- Finally reprocess teh whole thing:
      tail (go (zip3 tmps tys ls) bod')

    -- We could eliminate these ahead of time:
    L1.LetE (v,t,rhs) bod | L1.isTriv rhs ->
      T.LetTrivT (v,typ t, triv "<internal error2>" rhs) <$> tail bod

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
         
    -- Hack: no good way to express EndTimer in the source lang, so we
    -- stick it in just-in-time here.
    LetE (vr, ty, L1.TimeIt rhs _) bod -> do
      tm <- gensym "tmr"
      tail $ StartTimer tm $
              LetE (vr, ty, rhs) $
               EndTimer tm bod
    -- For internal use only:
    StartTimer nm bod -> T.StartTimerT nm <$> tail bod
    EndTimer   nm bod -> T.EndTimerT   nm <$> tail bod
                        
    L1.AppE v e        -> return $ T.TailCall ( v) [triv "operand" e]

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
      T.LetPrimCallT [(v,T.CursorTy)] T.WriteInt [triv "WriteTag arg" e, T.VarTriv c] <$>
         tail bod

    L1.LetE (pr,_,C.ReadInt c) bod -> do
      vtmp <- gensym "tmpval"
      ctmp <- gensym "tmpcur"
      T.LetPrimCallT [(vtmp,T.IntTy),(ctmp,T.CursorTy)] T.ReadInt [T.VarTriv c] <$>
        -- Here we lamely chase down all the tuple references and make them variablesa:
        let bod' = L1.substE (L1.ProjE 0 (L1.VarE pr)) (L1.VarE vtmp) $
                   L1.substE (L1.ProjE 1 (L1.VarE pr)) (L1.VarE ctmp) bod
        in
          dbgTrace 5 (" [lower] ReadInt, after substing references to "++pr++":\n  "++sdoc bod')$
          tail bod'

    L1.LetE (v,_,C.NewBuffer) bod ->
      T.LetPrimCallT [(v,T.CursorTy)] T.NewBuf [] <$>
         tail bod

    L1.LetE (v,_,C.ScopedBuffer) bod ->
      T.LetPrimCallT [(v,T.CursorTy)] T.ScopedBuf [] <$>
         tail bod

    ---------------------
    -- (3) Proper primapps.
    L1.LetE (v,t,L1.PrimAppE p ls) bod ->
        -- No tuple-valued prims here:
        T.LetPrimCallT [(v,typ t)]
             (prim p)
             (L.map (triv $ "prim rand "++show p) ls) <$>
             (tail bod)
    --------------------------------End PrimApps----------------------------------


    L1.LetE (_,_,L1.AppE f _) _ | M.notMember f fundefs ->
      error $ "Application of unbound function: "++show f

    -- Non-tail call:
    L1.LetE (v,t,L1.AppE f arg) bod -> do
        let f' = cleanFunName f
        (vsts,bod') <- case t of
                        L1.ProdTy ls -> do (tmps,e) <- eliminateProjs v ls bod
                                           return (zip tmps (L.map typ ls), e)
                        _ -> return ([(v,typ t)], bod)
        case arg of
          MkProdE es ->
               T.LetCallT vsts f' (L.map (triv "app rands") es) <$> (tail bod')
          _ -> T.LetCallT vsts f' [(triv "app rand") arg]       <$> (tail bod')
                  

    L1.LetE (v, t, L1.IfE a b c) bod -> do
      let a' = triv "if test" a
      b' <- tail b
      c' <- tail c
      case t of
        -- Finilize unarisation:
        L2.ProdTy ls -> do
             (tmps,bod') <- eliminateProjs v ls bod
             T.LetIfT (zip tmps (L.map typ ls)) (a', b', c') <$> tail bod'
        _ -> T.LetIfT [(v, typ t)] (a', b', c') <$> tail bod

{-
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
-}
{-        
        case ty of
          -- This gets tricky:
          L2.ProdTy ls -> error$ "[lower] Unfinished: time statement with tupled return: "++show ex
          _ -> do 
           tm <- gensym "tmr"
           rhs' <- tail rhs
           -- let rhs'' = wrapLast (T.EndTimerT tm) rhs'
           return $
            T.StartTimerT tm $
             chainTail rhs' $ \ _ ->
                 T.EndTimerT tm $
                -- mkLetTail (vr,ty,rhs'') $             
                  tail bod
-}
    _ -> error$ "lower: unexpected expression in tail position:\n  "++sdoc ex

pattern StartTimer t bod = AppE "StartTimer" (MkProdE [VarE t, bod])
pattern EndTimer t   bod = AppE "EndTimer"   (MkProdE [VarE t, bod])

{-
-- | Go under bindings and transform the very last return point.
chainTail :: T.Tail -> (T.Tail -> T.Tail) -> T.Tail 
chainTail tl fn =
  case tl of
    T.LetCallT   bnd rat rnds bod -> T.LetCallT   bnd rat rnds (chainTail bod fn)
    T.LetPrimCallT bnd p rnds bod -> T.LetPrimCallT bnd p rnds (chainTail bod fn)
    T.LetTrivT  bnd           bod -> T.LetTrivT           bnd  (chainTail bod fn)
    T.LetIfT bnd pr bod           -> T.LetIfT           bnd pr (chainTail bod fn)
    T.LetAllocT lhs vals bod      -> T.LetAllocT     lhs vals  (chainTail bod fn)
    -- Question here is whether we plan to go under Ifs and Cases...
    -- T.IfE a b c -> T.IfE a (chainTail b fn) (chainTail c fn)
    oth -> fn oth

-- | Create the right kind of Target let binding based on the form of the RHS:a
mkLetTail :: (Var,L2.Ty, T.Tail) -> T.Tail -> T.Tail
mkLetTail (vr,ty,rhs) =
  case rhs of
    RetValsT [one] -> __
    _ -> __
-}
         
-- | Eliminate projections from a given tuple variable.  INEFFICIENT!
eliminateProjs :: Var -> [L1.Ty] -> Exp -> SyM ([Var],Exp)
eliminateProjs vr tys bod =
 dbgTrace 5 (" [lower] eliminating "++show (length tys)++
             " projections on variable "++show vr++" in expr with types "++show tys++":\n   "++sdoc bod) $
 do
    tmps <- mapM (\_ -> gensym "pvrtmp") [1.. (length tys)]

    let go _ [] acc = acc
        go ix ((pvr,_pty):rs) acc =
           go (ix+1) rs
             (L1.substE (ProjE ix (VarE vr)) (VarE pvr) acc)
    let bod' = go 0 (zip tmps tys) bod
    return (tmps,bod')



mkLet :: (Var, L1.Ty, Exp) -> Exp -> Exp
mkLet (v,t,LetE (v2,t2,rhs2) bod1) bod2 = LetE (v2,t2,rhs2) $ LetE (v,t,bod1) bod2
mkLet (v,t,rhs) bod = LetE (v,t,rhs) bod



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
                         "It won't handle the following, which satisfies 'isTriv':\n "++sdoc e0++
                         "\nMessage: "++msg
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
    L1.DictInsertP ty -> T.DictInsertP $ typ ty
    L1.DictLookupP ty -> T.DictLookupP $ typ ty
    L1.DictEmptyP ty -> T.DictEmptyP $ typ ty
    L1.ErrorP{} -> error$ "lower/prim: internal error, should not have got to here: "++show p

    L1.MkTrue  -> error "lower/prim: internal error. MkTrue should not get here."
    L1.MkFalse -> error "lower/prim: internal error. MkFalse should not get here."

