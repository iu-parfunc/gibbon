
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

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
import           Packed.FirstOrder.L1_Source (Exp(..))
import qualified Packed.FirstOrder.L2_Traverse as L2
import           Packed.FirstOrder.L2_Traverse ( FunDef(..), Prog(..) )
import qualified Packed.FirstOrder.L3_Target as T
import Data.Maybe
import qualified Data.List as L
import Data.List as L hiding (tail)
import Data.Map as M hiding (foldl)
import Data.Int (Int64)
-- import Data.Word

import Prelude hiding (tail)


-- Generating unpack functions from Packed->Pointer representation:
-------------------------------------------------------------------------------

genDcons :: [L1.Ty] -> Var -> [(T.Ty, T.Triv)] -> SyM T.Tail
genDcons (x:xs) tail fields = case x of
  L1.IntTy             ->  do
    val  <- gensym $ toVar "val"
    t    <- gensym $ toVar "tail"
    T.LetPrimCallT [(val, T.IntTy), (t, T.CursorTy)] T.ReadInt [(T.VarTriv tail)]
      <$> genDcons xs t (fields ++ [(T.IntTy, T.VarTriv val)])

  L1.PackedTy tyCons _ _ -> do
    ptr  <- gensym $ toVar "ptr"
    t    <- gensym $ toVar "tail"
    T.LetCallT [(ptr, T.PtrTy), (t, T.CursorTy)] (mkUnpackerName tyCons) [(T.VarTriv tail)]
      <$> genDcons xs t (fields ++ [(T.CursorTy, T.VarTriv ptr)])
  _                    -> undefined

genDcons [] tail fields     = do
  ptr <- gensym $ toVar "ptr"
  return $ T.LetAllocT ptr fields $ T.RetValsT [T.VarTriv ptr, T.VarTriv tail]

genAlts :: [(DataCon,[L1.Ty])] -> Var -> Var -> Int64 -> SyM T.Alts
genAlts ((_, typs):xs) tail tag n = do
  curTail <- genDcons typs tail [(T.TagTyPacked, T.VarTriv tag)]
  alts    <- genAlts xs tail tag (n+1)
  case alts of
    T.IntAlts []   -> return $ T.IntAlts [(n::Int64, curTail)]
    -- T.TagAlts []   -> return $ T.TagAlts [(n::Word8, curTail)]
    T.IntAlts tags -> return $ T.IntAlts ((n::Int64, curTail) : tags)
    -- T.TagAlts tags -> return $ T.TagAlts ((n::Word8, curTail) : tags)
    _              -> error $ "Invalid case statement type."

genAlts [] _ _ _                  = return $ T.IntAlts []

genUnpacker :: DDef L1.Ty -> SyM T.FunDecl
genUnpacker DDef{tyName, dataCons} = do
  p    <- gensym $ toVar "p"
  tag  <- gensym $ toVar "tag"
  tail <- gensym $ toVar "tail"
  alts <- genAlts dataCons tail tag 0
  bod  <- return $ T.LetPrimCallT [(tag, T.TagTyPacked), (tail, T.CursorTy)] T.ReadTag [(T.VarTriv p)] $
            T.Switch (T.VarTriv tag) alts Nothing
  return T.FunDecl{ T.funName  = mkUnpackerName (fromVar tyName),
                    T.funArgs  = [(p, T.CursorTy)],
                    T.funRetTy = T.ProdTy [T.PtrTy, T.CursorTy],
                    T.funBody  = bod }


-- | Modify a Tail to *print* its return value and then

-- Utility functions
openParen :: String -> (T.Tail -> T.Tail)
openParen s  = T.LetPrimCallT [] (T.PrintString ("(" ++ s ++ " ")) []

closeParen :: T.Tail -> T.Tail
closeParen   = T.LetPrimCallT [] (T.PrintString ")") []

printSpace :: T.Tail -> T.Tail
printSpace = T.LetPrimCallT [] (T.PrintString " ") []

sandwich :: (T.Tail -> T.Tail) -> String -> T.Tail -> T.Tail
sandwich mid s end = openParen s $ mid $ closeParen end

-- Generate printing functions
genDconsPrinter :: [L1.Ty] -> Var -> SyM T.Tail
genDconsPrinter (x:xs) tail = case x of
  L1.IntTy             ->  do
    val  <- gensym $ toVar "val"
    t    <- gensym $ toVar "tail"
    T.LetPrimCallT [(val, T.IntTy), (t, T.CursorTy)] T.ReadInt [(T.VarTriv tail)] <$>
      printTy L1.IntTy [T.VarTriv val] <$>
       maybeSpace <$>
        genDconsPrinter xs t

  L1.PackedTy tyCons _ _ -> do
    val  <- gensym $ toVar "val"
    t    <- gensym $ toVar "tail"
    tmp  <- gensym $ toVar "temp"
    T.LetPrimCallT [(val, T.IntTy), (t, T.CursorTy)] T.ReadInt [(T.VarTriv tail)] <$>
      T.LetCallT [(tmp, T.PtrTy)] (mkPrinterName tyCons) [(T.VarTriv val)] <$>
       maybeSpace <$>
         genDconsPrinter xs t

  _ -> error "FINISHME: genDconsPrinter"

 where
  maybeSpace = if L.null xs
               then id
               else printSpace

genDconsPrinter [] tail     = do
  return $ closeParen $ T.RetValsT [(T.VarTriv tail)]

genAltPrinter :: [(DataCon,[L1.Ty])] -> Var -> Int64 -> SyM T.Alts
genAltPrinter ((dcons, typs):xs) tail n = do
  curTail <- (openParen dcons) <$> genDconsPrinter typs tail
  alts    <- genAltPrinter xs tail (n+1)
  case alts of
    T.IntAlts []   -> return $ T.IntAlts [(n::Int64, curTail)]
    -- T.TagAlts []   -> return $ T.TagAlts [(n::Word8, curTail)]
    T.IntAlts tags -> return $ T.IntAlts ((n::Int64, curTail) : tags)
    -- T.TagAlts tags -> return $ T.TagAlts ((n::Word8, curTail) : tags)
    _              -> error $ "Invalid case statement type."
genAltPrinter [] _ _                = return $ T.IntAlts []

genPrinter  :: DDef L1.Ty -> SyM T.FunDecl
genPrinter DDef{tyName, dataCons} = do
  p    <- gensym $ toVar "p"
  tag  <- gensym $ toVar "tag"
  tail <- gensym $ toVar "tail"
  alts <- genAltPrinter dataCons tail 0
  bod  <- return $ T.LetPrimCallT [(tag, T.TagTyPacked), (tail, T.CursorTy)] T.ReadInt [(T.VarTriv p)] $
            T.Switch (T.VarTriv tag) alts Nothing
  return T.FunDecl{ T.funName  = mkPrinterName (fromVar tyName),
                    T.funArgs  = [(p, T.CursorTy)],
                    T.funRetTy = T.PtrTy,
                    T.funBody  = bod }

printTy :: L1.Ty -> [T.Triv] -> (T.Tail -> T.Tail)
printTy L1.IntTy [trv]                = T.LetPrimCallT [] T.PrintInt [trv]
printTy L1.BoolTy [trv]               =
  let prntBool m                      = T.LetPrimCallT [] (T.PrintString m) [] in
    \t -> T.IfT trv (prntBool truePrinted $ t) (prntBool falsePrinted $ t)
printTy (L1.ProdTy xs) [trv]          = \t -> foldl (\y x -> (printTy x [trv] $ y)) t xs
printTy (L1.SymDictTy (x)) [trv]      = sandwich (printTy x [trv]) "Dict"
printTy (L1.PackedTy constr _ _) [trv]  = T.LetCallT [] (mkPrinterName constr) [trv]
printTy (L1.ListTy (x)) [trv]         = sandwich (printTy x [trv]) "List"
printTy _ _                           = error $ "Invalid L1 data type."

addPrintToTail :: L1.Ty -> T.Tail-> SyM T.Tail
addPrintToTail ty tl0 =
    T.withTail (tl0, T.IntTy) $ \ [trv] ->
      printTy ty [trv] $
        -- Always print a trailing newline at the end of execution:
        T.LetPrimCallT [] (T.PrintString "\n") [] $
          T.RetValsT []  -- Void return after printing.

-- | In packed mode we print by unpacking first.
addPrintToTailPacked :: L1.Ty -> T.Tail-> SyM T.Tail
addPrintToTailPacked ty tl0 =
  -- FIXME: Need to handle products of packed!!
  case ty of
    L1.PackedTy tycon _ _ ->
       T.withTail (tl0, T.IntTy) $ \ [trv] ->
          T.LetCallT [(toVar "unpkd", T.PtrTy), (toVar "ignre", T.CursorTy)] (mkUnpackerName tycon) [trv] $
           printTy ty [T.VarTriv (toVar "unpkd")] $
             -- Always print a trailing newline at the end of execution:
             T.LetPrimCallT [] (T.PrintString "\n") [] $
               T.RetValsT []  -- Void return after printing.
    _ -> addPrintToTail ty tl0

-- The compiler pass
-------------------------------------------------------------------------------a

-- | Convert into the target language.  This does not make much of a
-- change, but it checks the changes that have already occurred.
--
-- The only substantitive conversion here is of tupled arguments to
-- multiple argument functions.
--
-- First argument indicates (1) whether we're inpacked mode, and (2)
-- the pre-cursorize type of the mainExp, if there is a mainExp.
lower :: (Bool,Maybe L1.Ty) -> L2.Prog -> SyM T.Prog
lower (pkd,mMainTy) L2.Prog{fundefs,ddefs,mainExp} = do
  mn <- case mainExp of
          Nothing    -> return Nothing
          Just (x,mty) -> let Just origMainTy = mMainTy
                              addPrint = if pkd
                                           then addPrintToTailPacked origMainTy
                                           else addPrintToTail mty
                          in (Just . T.PrintExp) <$>
                              (addPrint =<< tail x)

  funs       <- mapM fund (M.elems fundefs)
  unpackers  <- mapM genUnpacker (M.elems ddefs)
  printers   <- mapM genPrinter (M.elems ddefs)
  T.Prog <$> pure (funs ++ unpackers ++ printers) <*> pure mn

--  T.Prog <$> mapM fund (M.elems fundefs) <*> pure mn

 where
  fund :: L2.FunDef -> SyM T.FunDecl
  fund L2.FunDef{funname,funty=(L2.ArrowTy inty _ outty),funarg,funbod} = do
      (args,bod) <- case inty of
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
  tail ex0 =
   dbgTrace 7 ("\n [lower] processing tail:\n  "++sdoc ex0) $
   case ex0 of

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
        tagtmp <- gensym $ toVar "tmpval"
        ctmp   <- gensym $ toVar "tmpcur"
        -- We only need to thread one value through, the cursor resulting from read.
        let doalt (k,ls,rhs) =
             (getTagOfDataCon ddefs k,) <$>
             case ls of
               []  -> tail rhs -- AUDITME -- is this legit, or should it have one cursor param anyway?
               [c] -> tail (L1.subst c (VarE ctmp) rhs)
        alts <- mapM doalt rest
        (_,last') <- doalt last
        return $
         T.LetPrimCallT [(tagtmp,T.TagTyPacked),(ctmp,T.CursorTy)] T.ReadTag [T.VarTriv scrut] $
          T.Switch (T.VarTriv tagtmp)
                   (T.TagAlts alts)
                   (Just last')

    --------------------------------------------------------------------------------
    -- Not-packed, pointer-based codegen
    --------------------------------------------------------------------------------
    -- In pointer-based representation we don't use `TagTyPacked`, because it is
    -- causing problems.  By default gcc aligns struct fields but we don't
    -- take that padding into account in our codegen.
    --
    -- If we get here that means we're NOT packing trees on this run:
    -- Thus this operates on BOXED data:
    CaseE e [(c, bndrs, rhs)] | not pkd -> do
      -- a product, directly assign the fields
      let tys = L.map typ (lookupDataCon ddefs c)

      -- TODO(osa): enable this
      -- ASSERT(length tys == length bndrs)

      let T.VarTriv e_var = triv "product case scrutinee" e
      tag_bndr  <- gensym $ toVar "tag"

      let bndrs' = tag_bndr : bndrs
          tys'   = T.IntTy  : tys
      rhs' <- tail rhs
      return (T.LetUnpackT (zip bndrs' tys') e_var rhs')

    CaseE e (def_alt : alts) | not pkd -> do
      tag_bndr <- gensym $ toVar "tag"
      tail_bndr <- gensym $ toVar "tail"

      let
        e_triv = triv "sum case scrutinee" e

        mk_alt :: (DataCon, [Var], Exp) -> SyM (Int64, T.Tail)
        mk_alt (con, bndrs, rhs) = do
          let
            con_tag = getTagOfDataCon ddefs con
            bndr_tys = L.map typ (lookupDataCon ddefs con)
          rhs' <- tail rhs
          return ( fromIntegral con_tag, T.LetUnpackT (zip bndrs bndr_tys) tail_bndr rhs' )

      alts'    <- mapM mk_alt alts
      (_, def) <- mk_alt def_alt

      return $
        T.LetPrimCallT
          [(tag_bndr, T.TagTyPacked), (tail_bndr, T.CursorTy)]
          T.ReadInt
          [e_triv]
          (T.Switch (T.VarTriv tag_bndr) (T.IntAlts alts') (Just def))

    -- Accordingly, constructor allocation becomes an allocation.
    LetE (v, _, MkPackedE k ls) bod | not pkd -> L1.assertTrivs ls $ do
      let tycon    = getTyOfDataCon ddefs k
          all_cons = dataCons (lookupDDef ddefs (toVar tycon))
          is_prod  = length all_cons == 1
          tag      = fromJust (L.findIndex ((==) k . fst) all_cons)

          field_tys= L.map typ (lookupDataCon ddefs k)
          fields0  = fragileZip field_tys (L.map (triv "MkPackedE args") ls)
          fields   = (T.IntTy, T.IntTriv (fromIntegral tag)) : fields0
          --  | is_prod   = fields0
          --  | otherwise = (T.IntTy, T.IntTriv (fromIntegral tag)) : fields0

      -- trace ("data con: " ++ show k) (return ())
      -- trace ("is_prod: " ++ show is_prod) (return ())
      -- trace ("fields: " ++ show fields) (return ())

      bod' <- tail bod

      return (T.LetAllocT v fields bod')

    -- This is legitimately flattened, but we need to move it off the spine:
    L1.MkPackedE k _ls -> do
       tmp <- gensym $ toVar "tailift"
       let ty = L1.PackedTy (getTyOfDataCon ddefs k) ()
       tail $ LetE (tmp, ty L1.NoneCur, ex0) (VarE tmp)

    --------------------------------------------------------------------------------

--    L1.LitE n       -> pure$ T.RetValsT [triv "literal in tail" (LitE n)]
    L1.MkProdE ls   -> pure$ T.RetValsT (L.map (triv "returned element of tuple") ls)
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

    LetE (vr, ty, L1.TimeIt rhs _ flg) bod ->
        do rhs' <- tail rhs
           case ty of
             L2.ProdTy ls ->
               do (tmps,bod') <- eliminateProjs vr ls bod
                  T.LetTimedT flg (zip tmps (L.map typ ls)) rhs' <$> tail bod'
             _ -> T.LetTimedT flg   [(vr, typ ty)]          rhs' <$> tail bod


    --------------------------------Start PrimApps----------------------------------
    -- (1) Primapps that become Tails:

    -- FIXME: No reason errors can't stay primitive at Target:
    L1.PrimAppE (L1.ErrorP str _ty) [] ->
      pure $ T.ErrT str
    L1.LetE (_,_,L1.PrimAppE (L1.ErrorP str _) []) _ ->
      pure $ T.ErrT str

    -- Whatever... a little just-in-time flattening.  Should obsolete this:
    L1.PrimAppE p ls -> do
      tmp <- gensym $ toVar "flt"
      tail (L1.LetE (tmp, L2.primRetTy p, L1.PrimAppE p ls) (L1.VarE tmp))

    ---------------------
    -- (2) Next FAKE Primapps.  These could be added to L1 if we wanted to pollute it.
    L1.LetE (v,_,L2.WriteInt c e) bod ->
      T.LetPrimCallT [(v,T.CursorTy)] T.WriteInt [triv "WriteTag arg" e, T.VarTriv c] <$>
         tail bod

    L1.LetE (pr,_,L2.ReadInt c) bod -> do
      vtmp <- gensym $ toVar "tmpval"
      ctmp <- gensym $ toVar "tmpcur"
      T.LetPrimCallT [(vtmp,T.IntTy),(ctmp,T.CursorTy)] T.ReadInt [T.VarTriv c] <$>
        -- Here we lamely chase down all the tuple references and make them variablesa:
        let bod' = L1.substE (L1.ProjE 0 (L1.VarE pr)) (L1.VarE vtmp) $
                   L1.substE (L1.ProjE 1 (L1.VarE pr)) (L1.VarE ctmp) bod
        in
          dbgTrace 5 (" [lower] ReadInt, after substing references to "++(fromVar pr)++":\n  "++sdoc bod')$
          tail bod'

    L1.LetE (v,_,L2.NewBuffer) bod ->
      T.LetPrimCallT [(v,T.CursorTy)] T.NewBuf [] <$>
         tail bod

    L1.LetE (v,_,L2.ScopedBuffer) bod ->
      T.LetPrimCallT [(v,T.CursorTy)] T.ScopedBuf [] <$>
         tail bod

    -- In Target, AddP is overloaded still:
    L1.LetE (v,_, L2.AddCursor c n) bod ->
      T.LetPrimCallT [(v,T.CursorTy)] T.AddP [ triv "addCursor base" (L2.VarE c)
                                             , triv "addCursor offset" (L2.LitE n)] <$>
         tail bod

    L1.LetE (_,_, p) _ | L2.isExtendedPattern p ->
     error $ "Lower: missed an extended L2 pattern on rhs of let: "++ndoc p
    p | L2.isExtendedPattern p ->
     error $ "Lower: missed an extended L2 pattern: "++ndoc p

    ---------------------
    -- (3) Proper primapps.
    L1.LetE (v,t,L1.PrimAppE p ls) bod ->
        -- No tuple-valued prims here:
        T.LetPrimCallT [(v,typ t)]
             (prim p)
             (L.map (triv $ "prim rand "++show p) ls) <$>
             (tail bod)
    --------------------------------End PrimApps----------------------------------

    L1.AppE{} | L2.isExtendedPattern ex0 -> error$ "Lower: Unhandled extended L2 pattern(1): "++ndoc ex0
    L1.AppE v (MkProdE ls) -> return $ T.TailCall ( v) (L.map (triv "operands") ls)
    L1.AppE v e            -> return $ T.TailCall ( v) [triv "operand" e]

    -- Tail calls are just an optimization, if we have a Proj/App it cannot be tail:
    ProjE ix ap@(AppE f e) | not (L2.isExtendedPattern ap) -> do
        tmp <- gensym $ toVar "prjapp"
        let L2.ArrowTy (L2.ProdTy inTs) _ _ = funty (fundefs # f)
        tail $ LetE ( tmp
                    , fmap (const ()) (inTs !! ix)
                    , ProjE ix (AppE f e))
                 (VarE tmp)

    L1.LetE (_,_, ap@(L1.AppE f _)) _
        | L2.isExtendedPattern ap -> error$ "Lower: Unhandled extended L2 pattern(2): "++ndoc ap
        | M.notMember f fundefs -> error $ "Application of unbound function: "++show f

    -- Non-tail call:
    L1.LetE (vr,t, projOf -> (stk, ap@(L1.AppE f arg))) bod
      | L2.isExtendedPattern ap -> error$ "Lower: Unhandled extended L2 pattern(3): "++ndoc ap
      | otherwise -> do
        let L2.ArrowTy _ _ outTy = funty (fundefs # f)
        let f' = cleanFunName f
        (vsts,bod') <- case outTy of
                        L1.ProdTy [] -> error "lower: FINISHME: unit valued function"
                        L1.ProdTy tys ->
                          case stk of
                            [] -> do (tmps,e) <- eliminateProjs vr (L.map (fmap (const ())) tys) bod
                                     return (zip tmps (L.map typ tys), e)
                            -- More than one should not currently be
                            -- possible (no nested tuple returns):
                            [ix] -> do garbages <- sequence [ gensym (toVar "garbage") | _ <- L.tail tys ]
                                       let (lead,trail) = L.splitAt ix garbages
                                       return ( zip (lead++[vr]++trail)
                                                    (L.map typ tys)
                                              , bod)
                        _ -> return ([(vr,typ t)], bod)
        case arg of
          MkProdE es ->
               T.LetCallT vsts f' (L.map (triv "one of app rands") es) <$> (tail bod')
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


    _ -> error$ "lower: unexpected expression in tail position:\n  "++sdoc ex0


-- Helpers
--------------------------------------------------------------------------------

-- | View pattern for matching agaist projections of Foo rather than just Foo.
projOf :: Exp -> ([Int], Exp)
projOf (ProjE ix e) = let (stk,e') = projOf e in
                      (stk++[ix], e')
projOf e = ([],e)



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
             " projections on variable "++show vr++" in expr with types "
                                        ++show tys++":\n   "++sdoc bod) $
 do tmps <- mapM (\_ -> gensym (toVar "pvrtmp")) [1.. (length tys)]
    let go _ [] acc =
            -- If there are ANY references left, we are forced to make the products:
            L1.subst vr (MkProdE (L.map VarE tmps)) acc
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
    (L1.LitE x) -> T.IntTriv (fromIntegral x) -- TODO: back propogate Int64 toL1
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
    L1.SizeParam -> T.SizeParam
    L1.DictInsertP ty -> T.DictInsertP $ typ ty
    L1.DictLookupP ty -> T.DictLookupP $ typ ty
    L1.DictEmptyP ty -> T.DictEmptyP $ typ ty

    L1.ReadPackedFile mf tyc _ -> T.ReadPackedFile mf tyc

    L1.MkNullCursor -> error$ "lower/prim: internal error, should not have got to here: "++show p
    L1.ErrorP{}     -> error$ "lower/prim: internal error, should not have got to here: "++show p
    L1.MkTrue       -> error "lower/prim: internal error. MkTrue should not get here."
    L1.MkFalse      -> error "lower/prim: internal error. MkFalse should not get here."
