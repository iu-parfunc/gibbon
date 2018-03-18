{-# LANGUAGE OverloadedStrings #-}

module Packed.FirstOrder.Passes.BoundsCheck
  ( boundsCheck, followRedirects ) where

import Data.Loc
import Data.Maybe (fromJust, isJust)
import Data.Graph
import qualified Data.Map as M
import qualified Data.Set as S

import Packed.FirstOrder.GenericOps
import Packed.FirstOrder.Common hiding (FunDef(..))
import Packed.FirstOrder.L1.Syntax hiding (Prog(..), FunDef(..))
import Packed.FirstOrder.L2.Syntax as L2

{- Note [Infinite regions]
~~~~~~~~~~~~~~~~~~~~~~~~~~

Instead of allocating a single global region (4GB right now), the compiler would
use a linked list of small regions which are allocated as required. Such regions
are referred to as "Infinite" in Gibbon. And the old regions are now "BigInfinite".
The size of the first region in the chain would typically be small. And subsequent
allocations double the size everytime (upto a certain limit).
For more details, refer https://github.com/iu-parfunc/gibbon/issues/79.

Since every region now has a boundary (an end), we have to ensure that we don't
cross that. This "bounds-check" has to be done before every write op (almost).


Bounds-check: When?
~~~~~~~~~~~~~~~~~~~~

When checking a DataConE, a bounds check is required only when it has 1
scalar value. Eg. `(Leaf 10)` or `(A 10 20 (B ...))`. But not for datatypes like
`(Node (Leaf ..) (Node ..))`. The reason is that constructing a `Node` just involves
writing a tag *before* it's children. So it's OK to assume that this write will
be safe.

This policy of inserting BoundsChecks is not perfect. It needs to be
inserted before *any* location arithmetic is performed on the output
cursor. For example, for buildSTreeProg this doesn't always work out.
But this comes with it's own problems. It's difficult to reliably infer
the exact number of bytes needed for a write op when we're just
performing some location arithmetic on the write cursor. The program
analysis needs to be modified to return this information. That is,
when we're looking at a location, we should be able to look-ahead
and return the `DataConE` (if any) that it's used in. That's a
TODO for now.

Current policy for adding a bounds check node:

(1) If the return type is "complex" i.e it has some nodes which have both
    scalar and packed fields, the bounds check is inserted before
    doing anything with the write cursor. Even before doing any location
    arithmetic involving such a cursor.

(2) Otherwise, do a bounds check just before the write op i.e DataConE.


Bounds-check: How?
~~~~~~~~~~~~~~~~~~

To implement this, we have to tweak the cursor passing style a bit.


    -- char*
    type Cursor = Ptr Char
    type Region = Cursor

    add1 :: Region -> Region -> Cursor -> Cursor
         -> (Region, Region, Cursor, (Cursor, Cursor))
    add1 reg end_reg lout lin =
      let tag = readTag lin
      in case tag of
           Leaf -> let n  = readInt tag
                       (reg1, end_reg1, lout1) = bounds_check reg end_reg lout 9
                       wt = writeTag lout1 Leaf
                       wi = writeInt wt   (n+1)
                   in (reg1, end_reg1, lin + 8, (lout1, wi))
           Node -> ...

    bounds_check :: Region -> Region -> Cursor -> Int -> (Region, Region, Cursor)
    bounds_check reg end_reg cur size =
      if (end_reg - cur) > size
      then (reg, end_reg, cur)
      else
        do new_reg <- allocate new_reg_size
           redirect cur new_reg
           return (new_reg, new_reg + new_reg_size, new_reg)


Every function takes in an additional (reg_start, reg_end) for every packed type
in the return value (i.e 1 pair per output cursor). The reg_end is used to
check if the current write cursor is within the region boundary. If it is,
we just continue using the current region. Otherwise, we allocate a new region,
write a "redirection" at the current cursor location and use new fresh region
for subsequent writes. The reg_start is used to calculate the size of the current
region, where `size_region = end_reg - start_reg`. Most of the work will be done
during codegen. This pass just inserts the `BoundsCheck` with correct args.


To thread through the region arguments:

(1) Region vars are prepended to the locations that AppE forms accept,
    and the corresponding let bindings are updated to accept region return values.

        LetE (x,[endof_lin], Packed, AppE "add1" [lin,lout] arg)

    becomes

        LetE (x,[reg2, end_reg2, endof_lin], Packed, AppE "add1" [reg1, reg2, lin, lout] arg)

(2) RetE forms returning packed values are modified to return region arguments,
    in addition to the "endof" locations.

        RetE [endof1] arg

    becomes

        RetE [reg2, end_reg2, endof1] arg

-}

-- Maps a location to a region
type RegEnv = M.Map LocVar Var

type TypeEnv = M.Map Var Ty2

type Deps = [(Var, Var, [Var])]

boundsCheck :: L2.Prog -> SyM L2.Prog
boundsCheck Prog{ddefs,fundefs,mainExp} = do
  fds' <- mapM (boundsCheckFn ddefs fundefs) $ M.elems fundefs
  let fundefs' = M.fromList $ map (\f -> (funname f,f)) fds'
      env2 = Env2 M.empty (initFunEnv fundefs)
  -- mainExp' <- case mainExp of
  --               Nothing -> return Nothing
  --               Just (mn, ty) -> Just . (,ty) <$>
  --                 boundsCheckExp ddefs fundefs M.empty env2 (depList mn) S.empty mn
  return $ Prog (ddefsWithRedir ddefs) fundefs' mainExp

boundsCheckFn :: DDefs Ty2 -> NewFuns -> L2.FunDef -> SyM L2.FunDef
boundsCheckFn ddefs fundefs f@FunDef{funarg,funty,funbod} = do
  let initRegEnv = M.fromList $ map (\(LRM lc r _) -> (lc, regionVar r)) (locVars funty)
      initTyEnv  = M.singleton funarg (arrIn funty)
      env2 = Env2 initTyEnv (initFunEnv fundefs)
      deps = [(lc, lc, []) | lc <- outLocVars funty] ++ depList funbod
  bod' <- boundsCheckExp ddefs fundefs initRegEnv env2 deps S.empty funbod
  return $ f {funbod = bod'}

boundsCheckExp :: DDefs Ty2 -> NewFuns -> RegEnv -> Env2 Ty2 -> Deps -> S.Set Var
               -> L L2.Exp2 -> SyM (L L2.Exp2)
boundsCheckExp ddfs fundefs renv env2 deps checked (L p ex) = L p <$>
  case ex of
    LetE (v, locs, ty, rhs@(L _ (DataConE lc dcon _))) bod -> do
      let PackedTy tycon _ = ty
          reg = renv # lc
      if needsBoundsCheck ddfs dcon && reg `S.notMember` checked
      then do
        let sz  = sizeRedirection + sizeOfScalars ddfs dcon
        -- IMPORTANT: Mutates the region/cursor bindings
        -- IntTy is a placeholder. BoundsCheck is a side-effect
        unLoc <$>
          mkLets [ ("_", []  , IntTy, l$ Ext$ BoundsCheck tycon sz reg lc)
                 , (v  , locs, ty   , rhs)] <$>
          boundsCheckExp ddfs fundefs renv (extendVEnv v ty env2) deps checked bod
      else
        LetE (v, locs, ty, rhs) <$>
          boundsCheckExp ddfs fundefs renv (extendVEnv v ty env2) deps checked bod

    LetE (v,locs,ty,rhs) bod ->
      LetE <$> (v,locs,ty,) <$> go rhs <*>
        boundsCheckExp ddfs fundefs renv (extendVEnv v ty env2) deps checked bod

    Ext ext ->
      case ext of
        LetLocE loc rhs bod -> do
          let reg = case rhs of
                      StartOfLE r  -> regionVar r
                      InRegionLE r -> regionVar r
                      AfterConstantLE _ lc -> renv # lc
                      AfterVariableLE _ lc -> renv # lc
                      -- HACK: LetE form doesn't extend the RegEnv with the
                      -- the endof locations returned by the RHS.
                      FromEndLE _         -> renv # loc
              (outloc, tycon, needsCheck) =
                if reg `S.member` checked
                then ("DUMMY","DUMMY",False)
                else
                  case deps of
                    [] -> ("DUMMY","DUMMY",False)
                    _  ->
                      let (g,nodeF,vtxF) = graphFromEdges deps
                          -- Vertex of the location variable
                          locVertex = case vtxF loc of
                                        Just x  -> x
                                        Nothing -> error $ "No vertex for:" ++ sdoc loc
                          retType = gTypeExp ddfs env2 bod
                      in if hasPacked retType
                          then
                            let  retLocs = getTyLocs retType
                                 retVertices = map (fromJust . vtxF) retLocs
                                 paths = map (\ret -> (ret, path g locVertex ret)) retVertices
                                 connected = filter snd paths
                            in case connected of
                                 [] -> ("DUMMY","DUMMY", False)
                                 (vert,_):_ ->
                                   let fst3 (a,_,_) = a
                                       outloc' = fst3 (nodeF vert)
                                       tycon'  = fromJust $ getTyconLoc outloc' retType
                                       iscomplex = hasComplexDataCon ddfs tycon'
                                   in (outloc', tycon', iscomplex)
                          else ("DUMMY","DUMMY", False)
          if needsCheck
          then
            unLoc <$>
              mkLets [ ("_", []  , IntTy, l$ Ext$ BoundsCheck tycon conservativeSizeScalars reg outloc) ] <$> l <$>
              Ext <$> LetLocE loc rhs <$>
              boundsCheckExp ddfs fundefs (M.insert loc reg renv) env2 deps (S.insert reg checked) bod
          else
            Ext <$> LetLocE loc rhs <$>
              boundsCheckExp ddfs fundefs (M.insert loc reg renv) env2 deps checked bod

        LetRegionE r bod -> Ext <$> LetRegionE r <$> go bod
        FromEndE{} -> return ex
        RetE{}     -> return ex
        BoundsCheck{} -> error "Shouldn't encounter BoundsCheck before this pass."


    VarE{}     -> return ex
    LitE{}     -> return ex
    LitSymE{}  -> return ex
    AppE{}     -> return ex
    PrimAppE{} -> return ex
    DataConE{} -> return ex
    ProjE i e  -> ProjE i <$> go e
    IfE a b c  -> IfE <$> go a <*> go b <*> go c
    MkProdE ls -> MkProdE <$> mapM go ls
    CaseE scrt mp -> do
      let L _ (VarE v) = scrt
          PackedTy _ tyloc = lookupVEnv v env2
          reg = renv # tyloc
      CaseE scrt <$> mapM (docase reg renv env2) mp
    TimeIt e ty b -> do
      e' <- go e
      return $ TimeIt e' ty b
    MapE{}  -> error $ "go: TODO MapE"
    FoldE{} -> error $ "go: TODO FoldE"

  where go = boundsCheckExp ddfs fundefs renv env2 deps checked
        docase reg lenv1 env2' (dcon,vlocs,bod) = do
          -- Update the envs with bindings for pattern matched variables and locations.
          -- The locations point to the same region as the scrutinee.
          let (vars,locs) = unzip vlocs
              lenv1' = foldr (\lc acc -> M.insert lc reg acc) lenv1 locs
              tys = lookupDataCon ddfs dcon
              env2'' = extendsVEnv (M.fromList $ zip vars tys) env2'
          (dcon,vlocs,) <$> (boundsCheckExp ddfs fundefs lenv1' env2'' deps checked bod)


        getTyconLoc :: LocVar -> L2.Ty2 -> Maybe TyCon
        getTyconLoc lc ty =
          case ty of
            PackedTy tycon lc1 -> if lc == lc1
                                  then Just tycon
                                  else Nothing
            ProdTy tys -> case filter isJust $ map (getTyconLoc lc) tys of
                            [] -> Nothing
                            ls -> head ls
            _ -> Nothing


-- | Return true if writing a DataCon needs a bounds check
needsBoundsCheck :: DDefs Ty2 -> DataCon -> Bool
needsBoundsCheck ddfs dcon = sizeOfScalars ddfs dcon > 0


-- | Minimum #bytes required to write DataCon
sizeOfScalars :: DDefs Ty2 -> DataCon -> Int
sizeOfScalars ddfs dcon =
  case (scalars,packed) of
    ([], _ ) -> 0
    (_ , []) -> 1 + sizes
    (_ , _ ) -> 1 + sizes + sizeRedirection
  where
    tys =  lookupDataCon ddfs dcon
    scalars = filter (not . isPackedTy) tys
    packed  = filter isPackedTy tys
    sizes = sum $ map (fromJust . sizeOf) scalars -- or just assume 8 for now ?


-- | Add redirection nodes to the ddefs
ddefsWithRedir :: DDefs Ty2 -> DDefs Ty2
ddefsWithRedir ddfs = M.map (\d@DDef{dataCons} -> d {dataCons = dataCons ++ [redirCon] } ) ddfs
  where
    redirCon :: (DataCon,[(IsBoxed,Ty2)])
    redirCon = (tagRedirection,[(False, CursorTy)])

-- |
hasComplexDataCon :: DDefs Ty2 -> TyCon -> Bool
hasComplexDataCon ddfs tycon =
  let dcons = getConOrdering ddfs tycon
      tys = map (lookupDataCon ddfs) dcons
  in hasComplex tys

hasComplex :: [[Ty2]] -> Bool
hasComplex tys = any id $ map (\t -> any hasPacked t && any (not . hasPacked) t) tys

-- The modified program analysis can't figure out the exact #bytes required
-- for the current write-op (Since it won't always be inserted before a write-op).
-- This is a reasonable default.
conservativeSizeScalars :: Int
conservativeSizeScalars = 64

--------------------------------------------------------------------------------

{- [Modifying functions to use redirection nodes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Bounds checking adds redirection nodes at the end of regions, and other functions
have to be modified so that they can use them i.e fns should know what to do when
they encounter a redirection. Consider the modifications to the add1 function:


    add1 :: Tree -> Tree
    add1 tr =
      case tr of
        Leaf n   -> ...
        Node l r -> ...
        Redirection cur ->
          let tr' = readCursor cur
          in add1 tr'


We just recursively call the parent function with the address stored after the
redirection tag. If the function takes more arguments, we only modify the argument
represented by the case scrutinee and then make the recursive call.


    addTrees :: Tree -> Tree -> Tree
    addTrees tr1 tr2 =
      case tr1 of
        Leaf n1 ->
          case tr2 of
            Leaf n2 -> ...
            Redirection cur ->
              let tr2' = readCursor cur
              in addTrees tr1 tr2'
         Node l r -> ...
         Redirection cur ->
           let tr1' = readCursor cur
           in addTrees tr1' tr2


`followRedirects` implements this idea.

-}


-- Modify functions to use the REDIRECTION tag. For all case expressions,
-- add an extra case, and recurse.
followRedirects :: L2.Prog -> SyM L2.Prog
followRedirects Prog{ddefs,fundefs,mainExp} = do
  fds' <- mapM (followRedirectsFn ddefs fundefs) $ M.elems fundefs
  let fundefs' = M.fromList $ map (\f -> (funname f,f)) fds'
  return $ Prog ddefs fundefs' mainExp

followRedirectsFn :: DDefs Ty2 -> NewFuns -> L2.FunDef -> SyM L2.FunDef
followRedirectsFn ddefs fundefs f@FunDef{funname,funarg,funty,funbod} = do
  let initTyEnv  = M.singleton funarg (arrIn funty)
  bod' <- followRedirectsExp ddefs fundefs funname initTyEnv funbod
  return $ f {funbod = bod'}

followRedirectsExp :: DDefs Ty2 -> NewFuns -> Var -> TypeEnv -> L L2.Exp2 -> SyM (L L2.Exp2)
followRedirectsExp ddfs fundefs funname tenv (L p ex) = L p <$>
  case ex of
    CaseE scrt mp -> do
      -- ASSUMPTION: scrutinee is flat.
      -- Replace tyloc with redirection and recurse
      -- TODO: Write more documentation about this pass.
      let L _ (VarE v) = scrt
          PackedTy _ tyloc = tenv # v
          fundef  = fundefs # funname
          fnarg   = funarg fundef
          fnty    = funty fundef
          inT     = arrIn fnty
          outT    = arrOut fnty
          inLocs  = inLocVars fnty
          outLocs = outLocVars fnty
          lrets   = locRets fnty

      -- Functions that don't take any packed inputs don't need to
      -- worry about processing the redirection tag.
      if not (hasPacked inT)
      then return ex
      else do
        rvar   <- gensym "rdr_var"
        rloc   <- gensym "rdr_loc"
        bvar   <- gensym "tmp_bind"
        fltarg <- gensym "fltarg"
        endofs <- mapM (\_ -> gensym "endof") lrets

        let
            -- The new function argument and it's type
            newarg    = replaceInArg inT (l$ VarE fnarg) tyloc (l$ VarE rvar)
            newargty  = replaceInTy inT tyloc

            -- tyloc is replaced with the location of the redirection
            newinLocs = map (\x -> if x == tyloc then rloc else x) inLocs

            -- Recursively call the same fn we're processing now
            recurse   = l$ AppE funname (newinLocs ++ outLocs) (l$ VarE fltarg)

            -- Just return the result of the recursion
            tail1     = l$ Ext (RetE endofs bvar)

            -- RHS of the new case expression
            newcaseexp = mkLets [ (fltarg, []    , newargty, newarg),
                                  (bvar  , endofs, outT    , recurse) ]
                         tail1
            -- Final case triple
            newcase = (tagRedirection,[(rvar,rloc)],newcaseexp)

        mp' <- mapM (docase tenv) mp
        return $ CaseE scrt (mp' ++ [newcase])

    VarE{}     -> return ex
    LitE{}     -> return ex
    LitSymE{}  -> return ex
    AppE{}     -> return ex
    PrimAppE{} -> return ex
    DataConE{} -> return ex
    LetE (v,locs,ty,rhs) bod ->
      LetE <$> (v,locs,ty,) <$> go rhs
        <*> followRedirectsExp ddfs fundefs funname (M.insert v ty tenv) bod
    ProjE i e  -> ProjE i <$> go e
    IfE a b c  -> IfE <$> go a <*> go b <*> go c
    MkProdE ls -> MkProdE <$> mapM go ls
    TimeIt e ty b -> do
      e' <- go e
      return $ TimeIt e' ty b
    Ext ext -> Ext <$>
      case ext of
        LetRegionE reg bod  -> LetRegionE reg <$> go bod
        LetLocE loc rhs bod -> LetLocE loc rhs <$> go bod
        RetE{}        -> return ext
        FromEndE{}    -> return ext
        BoundsCheck{} -> return ext

    MapE{}  -> error $ "go: TODO MapE"
    FoldE{} -> error $ "go: TODO FoldE"

  where
    go = followRedirectsExp ddfs fundefs funname tenv
    docase tenv1 (dcon,vlocs,bod) = do
          -- Update the envs with bindings for pattern matched variables and locations.
          -- The locations point to the same region as the scrutinee.
          let (vars,_) = unzip vlocs
              tys = lookupDataCon ddfs dcon
              tenv1' = foldr (\(x,ty) acc -> M.insert x ty acc) tenv1 (zip vars tys)
          (dcon,vlocs,) <$> (followRedirectsExp ddfs fundefs funname tenv1' bod)


-- | Break apart the argument and reconstruct it with relevant parts replaced.
--
-- It replaces the argument having the type `(PackedTy _ fromloc)` with `tovar`.
-- Looks under arbitrary tuples.
--
-- >>> replaceInArg (PackedTy "Tree" "lin2") (l$ VarE "arg") "lin2" (l$ VarE "redir")
-- VarE (Var "redir")
--
-- >>> replaceInArg (ProdTy [PackedTy "Tree" "lin351", PackedTy "Tree" "lin352"]) (l$ VarE "arg") "lin351" (l$ VarE "redir")
-- MkProdE [VarE (Var "redir"),ProjE 1 VarE (Var "arg")]
--
-- >>> replaceInArg (ProdTy [PackedTy "Tree" "lin351", PackedTy "Tree" "lin352"]) (l$ VarE "arg") "lin352" (l$ VarE "redir")
-- MkProdE [ProjE 0 VarE (Var "arg"),VarE (Var "redir")]
--
-- >>> replaceInArg (ProdTy [PackedTy "Tree" "lin1",
--                           PackedTy "Tree" "lin2",
--                           ProdTy [PackedTy "Tree" "lin3"]])
--                  (l$ VarE "arg") "lin3" (l$ VarE "redir")
-- MkProdE [ProjE 0 VarE (Var "arg"),ProjE 1 VarE (Var "arg"),MkProdE [VarE (Var "redir")]]
-- >>> replaceInArg (ProdTy [PackedTy "Tree" "lin1",
--                           PackedTy "Tree" "lin2",
--                           ProdTy [PackedTy "Tree" "lin3",
--                                   PackedTy "Tree" "lin4"]])
--                  (l$ VarE "arg") "lin4" (l$ VarE "redir")
-- MkProdE [ProjE 0 VarE (Var "arg"),ProjE 1 VarE (Var "arg"),MkProdE [ProjE 0 ProjE 2 VarE (Var "arg"),VarE (Var "redir")]]
replaceInArg :: L2.Ty2 -> L L2.Exp2 -> LocVar -> L L2.Exp2 -> L L2.Exp2
replaceInArg ty arg fromloc tovar =
  case ty of
    PackedTy _ tyloc -> if tyloc == fromloc
                        then tovar
                        else arg
    ProdTy tys -> l$ MkProdE (map ((\(t,n) -> replaceInArg t (l$ ProjE n arg) fromloc tovar)) $ zip tys [0..])
    _ -> arg


-- | Replace (PackedTy _ fromloc) with a CursorTy
replaceInTy :: L2.Ty2 -> LocVar -> L2.Ty2
replaceInTy ty fromloc =
  case ty of
    PackedTy _ tyloc -> if tyloc == fromloc
                        then CursorTy
                        else ty
    ProdTy tys -> ProdTy $ map (\x -> replaceInTy x fromloc) tys
    _ -> ty
