{-# LANGUAGE OverloadedStrings #-}

module Packed.FirstOrder.Passes.Cursorize4
  (cursorize) where

import Data.Loc
import Data.List as L
import Data.Map as M
import Data.Set as S
import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint.HughesPJ

import Packed.FirstOrder.Common    hiding (FunDefs, FunDef(..))
import Packed.FirstOrder.L1.Syntax hiding (Prog(..), FunDef(..), FunDefs)
import Packed.FirstOrder.L2.Syntax as L2
import qualified Packed.FirstOrder.L3.Syntax as L3

import qualified Packed.FirstOrder.Chai as Chai
import Debug.Trace

--------------------------------------------------------------------------------

-- | Stores LRM information about variables which are eventually transformed to
-- cursors in this pass
-- For example, a function argument is transformed into an input cursor
type VEnv = Map Var LRM

-- | Used to assign correct types to cursors constructed using other locations/cursors
type LEnv = Map LocVar LRM

-- |
cursorize :: Prog -> SyM L3.Prog
cursorize Prog{ddefs,fundefs,mainExp} = do
  fns' <- mapM (fd . snd) (M.toList fundefs)
  let fundefs' = M.fromList $ L.map (\f -> (L3.funname f, f)) fns'
  -- TODO: cursorize mainExp
  return $ L3.Prog ddefs fundefs' Nothing

  where
        -- TODO: This is risky! might result in a runtime error. use something safe here
        getInLrm  = head . L.filter (\(LRM _ _ m) -> m == Input)
        getOutLrm = head . L.filter (\(LRM _ _ m) -> m == Output)

        -- | Change the input and output types to have explicit cursors, and cursorize the body
        fd :: FunDef -> SyM L3.FunDef
        fd FunDef{funname,funty,funarg,funbod} =
          let inlrm    = getInLrm (locVars funty)
              outlrm   = getOutLrm (locVars funty)
              inloc    = lrmLoc inlrm
              outloc   = lrmLoc outlrm
              initVenv = M.singleton funarg inlrm
              initLenv = M.fromList [(inloc,inlrm),(outloc,outlrm)]
              toEndV   = varAppend "end_"

              -- | Change the function type
              -- f :: Tree_{lin} -> (end_lin, Tree_{lout})
              -- becomes
              -- f :: (lout, lin) -> (end_lin, end_lout)
              funty'   = funty { arrIn  = ProdTy [CursorTy inlrm, CursorTy outlrm]
                               , arrOut = ProdTy [CursorTy (changeLRMLoc (toEndV inloc) inlrm),
                                                  CursorTy (changeLRMLoc (toEndV outloc) outlrm)]
                               }

          in do
          newFunarg <- gensym (varAppend "tup_" $ varAppend outloc inloc)
          -- Here, we make the locVars explicit by creating let bindings for them,
          -- so that other expressions in function body can refer to them
          (L3.FunDef funname funty' newFunarg) <$>
            l<$> (LetE (locToCur outloc,[], CursorTy outlrm,
                        l$ ProjE 0 (l$ VarE newFunarg)) <$>
                  (l<$> (LetE (locToCur inloc,[], CursorTy inlrm,
                               l$ ProjE 1 (l$ VarE newFunarg)) <$>
          -- and finally, we cursorize the function body
                         cursorizeExp ddefs initVenv initLenv funbod)))


cursorizeExp :: DDefs Ty2 -> VEnv -> LEnv -> L Exp2 -> SyM (L L3.Exp3)
cursorizeExp ddfs venv lenv (L p exp) = L p <$>
  case exp of
    VarE v    -> return $ VarE v
    LitE n    -> return $ LitE n
    LitSymE v -> return $ LitSymE v

    -- a case expression is eventually transformed into a ReadTag + switch
    -- we first retrieve the cursor referred to by the scrutinee, and unpack
    -- the first bound variable 1 byte after that cursor
    CaseE (L _ (VarE v)) brs ->
      case M.lookup v venv of
        Just lrm -> do
          {- generate a ReadTag here, instead of doing it in Lower ...

          cursorAfterTag <- gensym (toVar "cursor_after_tag")
          traceShow lrm (return __)
          maintain all the same properties as the cursor for v
          let cursorAfterTagLrm = changeLRMLoc "FIXME" lrm
          (LetE (cursorAfterTag,[],
                 CursorTy cursorAfterTagLrm,
                 l$ Ext $ L3.ReadTag (locToCur loc))) <$>
            (l<$> CaseE (l$ VarE cursorAfterTag)) <$>
                    mapM (unpackDataCon cursorAfterTagLrm) brs

          -}
          CaseE (l$ VarE $ lrmLoc lrm) <$>
            mapM (unpackDataCon lrm) brs
        Nothing -> error $ "cursorizeExp: CaseE encountered a var without a cursor: " ++ sdoc v

    PrimAppE pr args -> PrimAppE pr <$> mapM go args

    AppE f locs _args ->
      case locs of
        [] -> error "cursorizing AppE: hmm.. when would locs be empty?"
        (iploc:oploc:[]) -> do
          return $ AppE f [] (l$ MkProdE [l$ VarE oploc, l$ VarE iploc])
        _ -> error $ "cursorizing AppE: unexpected number of locations: " ++ show locs

    LetE (v,locs,ty,rhs) bod | not (isPacked ty) -> do
      rhs' <- go rhs
      LetE (v,locs,ty,rhs') <$> go bod

    LetE (v,locs,_ty,rhs) bod -> do
      -- would return a (start,end) cursor tuple
      -- we bind v to the end cursor, and start cursor to the location in locs
      rhs' <- go rhs
      fresh <- gensym "packed_tpl"

      -- FIXME: get rid of this
      let tmpLRM = LRM {lrmLoc = "FIXME", lrmReg = VarR "FIXME", lrmMode = Input}

      case locs of
        [] -> do
          (LetE (fresh,[],ProdTy [CursorTy tmpLRM, CursorTy tmpLRM], rhs') <$>
            (l <$> LetE (v,[],CursorTy tmpLRM, l$ ProjE 1 (l$ VarE fresh)) <$>
              go bod))
        _ -> do
          (LetE (fresh,[],ProdTy [CursorTy tmpLRM, CursorTy tmpLRM], rhs') <$>
            (l <$> LetE (head locs,[],CursorTy tmpLRM, l$ ProjE 0 (l$ VarE fresh)) <$>
              (l <$> LetE (v,[],CursorTy tmpLRM, l$ ProjE 1 (l$ VarE fresh)) <$>
               go bod)))

    -- this is the write tag operation
    -- returns a tuple of (start,end) cursors
    DataConE sloc dcon args -> do
      let slrm = lenv ! sloc
          -- Return (start,end).
          -- The final return value lives at the position of the out cursoara:
          go2 d [] = return $ MkProdE [l$ VarE sloc, l$ VarE d]

          go2 _d ((rnd, ty):rst) | isPacked ty = do
            d' <- gensym $ toVar "writepackedcur"
            let (L _ (VarE v)) = rnd
            LetE (d',[], CursorTy (changeLRMLoc "FIXME" slrm), l$ VarE v) <$>
              l <$> (go2 d' rst)

          -- (_ty == IntTy) : Int fields are currently our only "scalar" fields
          go2 d ((rnd,_ty):rst) = do
            d' <- gensym $ toVar "writeintcur"
            rnd' <- go rnd
            LetE (d',[], CursorTy (changeLRMLoc "FIXME" slrm), l$ Ext $ L3.WriteInt d rnd') <$>
              l <$> (go2 d' rst)

      writetag <- gensym "writetag"
      (LetE (writetag,[], CursorTy (changeLRMLoc "FIXME" slrm),
                       l$ Ext $ L3.WriteTag dcon sloc)
        <$> l <$> (go2 writetag (zip args (lookupDataCon ddfs dcon))))

    Ext (LetLocE loc rhs bod) -> do
      let (rhs',lrm) = cursorizeLocExp rhs
          lrm'  = changeLRMLoc loc lrm
          nlenv = M.insert loc lrm' lenv
      LetE (loc,[],CursorTy lrm',rhs') <$>
        cursorizeExp ddfs venv nlenv bod

    Ext (RetE locs v) ->
      case locs of
        [] -> return $ VarE v
        [loc] -> return $ MkProdE [l$ VarE loc, l$ VarE v]
        _     -> error $ "cursorize: RetE with more than 1 locs not allowed! " ++ show locs

    oth -> trace ("TODO:\n" ++ sdoc oth) $ return (VarE "chai")

  where
    go = cursorizeExp ddfs venv lenv

    -- | take a cursor pointing to the start of the tag, and advance it by 1 byte
    -- if the first bound varaible is a scalar (IntTy), read it using the newly returned cursor.
    -- otherwise, just process the body. it'll have the correct instructions to process
    -- these bound variables
    -- Not sure if we should do it here, or in lower ...
    unpackDataCon :: LRM -> (DataCon, [(Var,LocVar)], L Exp2) ->
                     SyM (DataCon, [(Var,LocVar)], L L3.Exp3)
    unpackDataCon scrtLrm (dcon,vlocs,rhs) =
      let (vars,locs) = unzip vlocs
          tys  = lookupDataCon ddfs dcon
      in
      case tys of
        [] -> (dcon, vlocs,) <$> go rhs
        (ty:_) -> do
          let scrtLoc = lrmLoc (scrtLrm)          -- location of the scrutinee
              floc    = head locs                 -- location of the first field

              flrm    = changeLRMLoc floc scrtLrm -- retail all the properties (region & mode)
                                                  -- of the cursor for the scrutinee, but
                                                  -- change its location appropriately
              lenv'   = M.insert floc flrm lenv

          -- TODO: check if we can conditionally add things to the fmap computation
          bod <-
            if ty == IntTy
            then do
              -- the first field is an int, create a let binding for "v" by performing a
              -- readint
              let v = head vars -- name of the first bound variable
              tmp <- gensym (toVar "readint_tpl")
              LetE (floc,[],CursorTy flrm, l$ Ext$ L3.AddCursor 1 scrtLoc) <$>
                -- the tmp cursor doesn't have a correct type. flrm should be modified
                -- with the location of the next field, if it has any
                l<$> (LetE (tmp,[],ProdTy [IntTy, CursorTy (changeLRMLoc "FIXME" flrm)],
                            l$ Ext $ L3.ReadInt scrtLoc) <$>
                       (l<$> LetE (v,[],IntTy, l$ ProjE 0 (l$ VarE tmp)) <$>
                         cursorizeExp ddfs venv lenv' rhs))
            else do
              LetE (floc,[],CursorTy flrm, l$ Ext$ L3.AddCursor 1 scrtLoc) <$>
                cursorizeExp ddfs venv lenv' rhs

          return (dcon,vlocs,l$ bod)

    -- would this always have a valid LRM ? should this be a Maybe ?
    cursorizeLocExp :: LocExp -> (L L3.Exp3, LRM)
    cursorizeLocExp locExp =
      case locExp of
        AfterConstantLE i loc -> (l$ Ext $ L3.AddCursor i loc, lenv ! loc)
        AfterVariableLE v loc -> (l$ VarE (toVar $ "AfterVariableLE" ++ fromVar v ++ fromVar loc), LRM{lrmLoc = "FIXME",lrmReg = VarR "FIXME", lrmMode = Input})
        FromEndLE v -> (l$ VarE v, LRM{lrmLoc = "FIXME",lrmReg = VarR "FIXME", lrmMode = Input})
        oth -> error $ "cursorizeLocExp: todo " ++ sdoc oth

-- | Change lrmLoc to the given location
changeLRMLoc :: LocVar -> LRM -> LRM
changeLRMLoc v lrm = lrm {lrmLoc = v}

-- | Prepend "cur_" to all location variables
locToCur :: LocVar -> Var
locToCur = varAppend ""

-- | Prepend "cur_" to all location variables
locToEndCursor :: LocVar -> Var
locToEndCursor = varAppend "cur_end_"

isPacked :: Ty2 -> Bool
isPacked PackedTy{} = True
isPacked _ = False
