{-# LANGUAGE OverloadedStrings #-}

module Packed.FirstOrder.Passes.Cursorize4
  (cursorize) where

import Data.Loc
import Data.List as L
import Data.Map as M

import Packed.FirstOrder.Common    hiding (FunDefs, FunDef(..))
import Packed.FirstOrder.L1.Syntax hiding (Prog(..), FunDef(..), FunDefs)
import Packed.FirstOrder.L2.Syntax as L2
import qualified Packed.FirstOrder.L3.Syntax as L3

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
cursorize Prog{ddefs,fundefs} = do
  fns' <- mapM (fd . snd) (M.toList fundefs)
  let fundefs' = M.fromList $ L.map (\f -> (L3.funname f, f)) fns'
      ddefs' = M.map L3.eraseLocMarkers ddefs
  -- TODO: cursorize mainExp
  return $ L3.Prog ddefs' fundefs' Nothing

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

              -- | Change the function type
              -- f :: Tree_{lin} -> (end_lin, Tree_{lout})
              -- becomes
              -- f :: (lout, lin) -> (end_lin, end_lout)
              funty'   = L3.ArrowTy { L3.arrIn  = ProdTy [CursorTy, CursorTy]
                                    , L3.arrOut = ProdTy [CursorTy, CursorTy]
                                    }

          in do
          newFunarg <- gensym (varAppend "tup_" $ varAppend outloc inloc)
          -- Here, we make the locVars explicit by creating let bindings for them,
          -- so that other expressions in function body can refer to them
          (L3.FunDef funname funty' newFunarg) <$>
            l<$> (LetE (outloc,[], CursorTy,
                        l$ ProjE 0 (l$ VarE newFunarg)) <$>
                  (l<$> (LetE (inloc,[], CursorTy,
                               l$ ProjE 1 (l$ VarE newFunarg)) <$>
          -- and finally, we cursorize the function body
                         cursorizeExp ddefs initVenv funbod)))


cursorizeExp :: DDefs Ty2 -> VEnv -> L Exp2 -> SyM (L L3.Exp3)
cursorizeExp ddfs venv (L p exp) = L p <$>
  case exp of
    VarE v    -> case M.lookup v venv of
                   Just lrm -> traceShow (v,lrm) (return $ VarE (lrmLoc lrm))
                   _        -> return $ VarE v

    LitE n    -> return $ LitE n
    LitSymE v -> return $ LitSymE v

    -- A case expression is eventually transformed into a ReadTag + switch statement.
    -- We first retrieve the cursor referred to by the scrutinee, and unpack
    -- the first bound variable 1 byte after that cursor. Thats all we need to do here,
    -- because we've already computed other locations in InferLocations and RouteEnds
    CaseE (L _ (VarE v)) brs ->
      case M.lookup v venv of
        Just lrm -> do
          {- generate a ReadTag here, instead of doing it in Lower ...

          cursorAfterTag <- gensym (toVar "cursor_after_tag")
          traceShow lrm (return __)
          maintain all the same properties as the cursor for v
          let cursorAfterTagLrm = cursorizeLRM "_" lrm
          (LetE (cursorAfterTag,[],
                 CursorTy cursorAfterTagLrm,
                 l$ Ext $ L3.ReadTag loc)) <$>
            (l<$> CaseE (l$ VarE cursorAfterTag)) <$>
                    mapM (unpackDataCon cursorAfterTagLrm) brs

          -}
          CaseE (l$ VarE $ lrmLoc lrm) <$>
            mapM (unpackDataCon lrm) brs
        Nothing -> error $ "cursorizeExp: CaseE encountered a var without a cursor: " ++ sdoc v

    -- Trivial case
    PrimAppE pr args -> PrimAppE pr <$> mapM go args

    -- Here we switch to a convention where functions accept (output,input) cursors
    -- and return end-witnesses. Since we already know the abstract locations to which
    -- these values flow to, the actual fn arguments are unused!
    -- TODO: AUTIT ME
    AppE f locs _args ->
      case locs of
        (iploc:oploc:[]) -> do
          return $ AppE f [] (l$ MkProdE [l$ VarE oploc, l$ VarE iploc])
        _ -> error $ "cursorizing AppE: unexpected number of locations: " ++ show locs

    -- TODO: AUTIT ME
    -- Right now, we return a (Cursor,Cursor) pair i.e (start, end)
    -- this should probably return (end_input, end_output) ?
    DataConE sloc dcon args -> do
      let
          -- Return (start,end) cursors
          -- The final return value lives at the position of the out cursors:
          go2 d [] = return $ MkProdE [l$ VarE sloc, l$ VarE d]

          go2 _d ((rnd, ty):rst) | isPackedTy ty = do
            d' <- gensym $ toVar "writepackedcur"
            let (L _ (VarE v)) = rnd
            LetE (d',[], CursorTy, l$ VarE v) <$>
              l <$> (go2 d' rst)

          -- (_ty == IntTy) : Int fields are currently our only "scalar" fields
          go2 d ((rnd,_ty):rst) = do
            d' <- gensym $ toVar "writeintcur"
            rnd' <- go rnd
            LetE (d',[], CursorTy, l$ Ext $ L3.WriteInt d rnd') <$>
              l <$> (go2 d' rst)

      writetag <- gensym "writetag"
      (LetE (writetag,[], CursorTy,
             l$ Ext $ L3.WriteTag dcon sloc)
        <$> l <$> (go2 writetag (zip args (lookupDataCon ddfs dcon))))


    -- This is a simple case where the RHS is not packed
    LetE (v,_locs,ty,rhs) bod | not (isPackedTy ty) -> do
      rhs' <- go rhs
      LetE (v,[], L3.stripTyLocs ty,rhs') <$> go bod

    -- Here, we assume a convention that all packed values are changed to be (start,end) cursors.
    -- We bind the end of the input cursor to the end location if specified,
    -- and end of write cursor to the variable
    LetE (v,locs,_ty,rhs) bod -> do
      -- would return a (start,end) cursor tuple
      -- we bind v to the end cursor, and start cursor to the location in locs
      rhs' <- go rhs
      fresh <- gensym "packed_tpl"

      case locs of
        [] -> do
          (LetE (fresh,[],ProdTy [CursorTy, CursorTy], rhs') <$>
            (l <$> LetE (v,[],CursorTy, l$ ProjE 1 (l$ VarE fresh)) <$>
              cursorizeExp ddfs venv bod))

        _ -> do
          (LetE (fresh,[],ProdTy [CursorTy, CursorTy], rhs') <$>
            (l <$> LetE (head locs,[],CursorTy, l$ ProjE 0 (l$ VarE fresh)) <$>
              (l <$> LetE (v,[],CursorTy, l$ ProjE 1 (l$ VarE fresh)) <$>
               cursorizeExp ddfs venv bod)))

    -- TODO
    -- IfE EXP EXP EXP
    -- MkProdE [EXP]
    -- ProjE Int EXP
    -- TimeIt EXP dec Bool

    -- All locations are transformed into cursors here. All the location expressions
    -- are expressed in terms of corresponding cursor operations. See `cursorizeLocExp`
    Ext (LetLocE loc rhs bod) -> do
      let rhs' = cursorizeLocExp rhs
      LetE (loc,[],CursorTy,rhs') <$>
        cursorizeExp ddfs venv bod

    -- Just convert the implicit location return into a ProdE
    Ext (RetE locs v) ->
      case locs of
        [] -> return $ VarE v
        [loc] -> return $ MkProdE [l$ VarE loc, l$ VarE v]
        _     -> error $ "cursorize: RetE with more than 1 locs not allowed! " ++ show locs

    -- Some expressions are not handled yet ...
    oth -> trace ("TODO:\n" ++ sdoc oth) $ return (VarE "FIXME")

  where
    go = cursorizeExp ddfs venv

    -- | Take a cursor pointing to the start of the tag, and advance it by 1 byte
    -- If the first bound varaible is a scalar (IntTy), read it using the newly returned cursor.
    -- Otherwise, just process the body. it'll have the correct instructions to process
    -- other bound locations
    unpackDataCon :: LRM -> (DataCon, [(Var,LocVar)], L Exp2) ->
                     SyM (DataCon, [(Var,())], L L3.Exp3)
    unpackDataCon scrtLrm (dcon,vlocs,rhs) =
      let (vars,locs) = unzip vlocs
          tys  = lookupDataCon ddfs dcon
      in
      case tys of
        [] -> (dcon, [],) <$> go rhs
        (ty:_) -> do
          let scrtLoc = lrmLoc (scrtLrm)     -- location of the scrutinee
              floc    = head locs            -- location of the first field

          -- TODO: check if we can conditionally add things to the fmap computation
          bod <-
            if ty == IntTy
            then do
              -- the first field is an int, create a let binding for "v" by performing a
              -- readint
              let v = head vars -- name of the first bound variable
              tmp <- gensym (toVar "readint_tpl")
              LetE (floc,[],CursorTy, l$ Ext$ L3.AddCursor scrtLoc 1) <$>
                -- the tmp cursor doesn't have a correct type. flrm should be modified
                -- with the location of the next field, if it has any
                l<$> (LetE (tmp,[],ProdTy [IntTy, CursorTy],
                            l$ Ext $ L3.ReadInt scrtLoc) <$>
                       (l<$> LetE (v,[],IntTy, l$ ProjE 0 (l$ VarE tmp)) <$>
                         cursorizeExp ddfs venv rhs))
            else do
              LetE (floc,[],CursorTy, l$ Ext$ L3.AddCursor scrtLoc 1) <$>
                cursorizeExp ddfs venv rhs

          return (dcon,[],l$ bod)

    -- would this always have a valid LRM ? should this be a Maybe ?
    cursorizeLocExp :: LocExp -> L L3.Exp3
    cursorizeLocExp locExp =
      case locExp of
        AfterConstantLE i loc -> l$ Ext $ L3.AddCursor loc i
        AfterVariableLE v loc -> l$ VarE (toVar $ "AfterVariableLE" ++ fromVar v ++ fromVar loc)
        FromEndLE loc -> l$ VarE loc
        oth -> error $ "cursorizeLocExp: todo " ++ sdoc oth

-- | Change the location to _
cursorizeLRM :: LRM -> LRM
cursorizeLRM lrm = lrm {lrmLoc = "_"}
