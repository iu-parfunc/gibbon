{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

-- | The final pass of the compiler: generate C code.

module Packed.FirstOrder.Passes.Codegen
    ( codegenProg ) where

import           Control.Monad
import           Data.Bifunctor (first)
import           Data.Int
import           Data.Loc -- For SrcLoc
import           Data.Maybe
import qualified Data.Set as S
import           Language.C.Quote.C (cdecl, cedecl, cexp, cfun, cparam, csdecl, cstm, cty, cunit)
import qualified Language.C.Quote.C as C
import qualified Language.C.Syntax as C
import           Packed.FirstOrder.Common hiding (funBody)
import           Prelude hiding (init)
import           System.Directory
import           System.Environment
import           Text.PrettyPrint.Mainland

import           Packed.FirstOrder.L3_Target
--------------------------------------------------------------------------------
        

-- | Harvest all struct tys.  All product types used anywhere in the program.
harvestStructTys :: Prog -> S.Set [Ty]
harvestStructTys (Prog funs mtal) =
    -- if S.null (S.difference tys0 tys1)
    S.union tys0 tys1
  where
  tys0 :: S.Set [Ty]
  tys0 = findAllProds $ concatMap allTypes allTails

  tys1 :: S.Set [Ty]
  -- All types mentioned in function arguments and returns:
  tys1 = S.fromList [ tys | fn <- funs, ProdTy tys <- funTys fn ]
  -- structs f = makeStructs $ S.toList $ harvestStructTys prg

  funTys :: FunDecl -> [Ty]
  funTys (FunDecl _ args ty _) = ty : (map snd args)

  allTails = (case mtal of
                Just (PrintExp t) -> [t]
                Just (RunWithRacketFile{}) -> []
                Just (RunRacketCorePass{}) -> []
                Nothing -> []) ++
             map funBody funs

  -- We may have nested products; this finds everything:
  findAllProds :: [Ty] -> S.Set [Ty]
  findAllProds = go
    where
      go []     = S.empty
      go (t:ts) =
       case t of
         ProdTy ls -> S.insert ls $ S.union (go ls) (go ts)
         _ -> go ts -- S.empty

  -- This finds all types that maybe grouped together as a ProdTy:
  allTypes :: Tail -> [Ty]
  allTypes = go
   where
    go tl = 
     case tl of
       (RetValsT _)  -> []
       (AssnValsT ls)-> [ProdTy (map (\(_,x,_) -> x) ls)]
       -- This creates a demand for a struct return, but it is covered
       -- by the fun signatures already:
       (LetCallT binds _ _  bod)   -> ProdTy (map snd binds) : go bod
       -- INVARIANT: This does not create a struct:
       -- But just in case it does in the future, we add it:
       (LetPrimCallT binds _ _ bod)-> ProdTy (map snd binds) : go bod
       (LetTrivT (_,ty,_) bod)     -> ty : go bod
       -- This should not create a struct.  Again, we add it just for the heck of it:
       (LetIfT binds (_,a,b) bod)  -> ProdTy (map snd binds) : go a ++ go b ++ go bod
       (LetTimedT _ binds rhs bod) -> ProdTy (map snd binds) : go rhs ++ go bod

       -- These are precisely for operating on structs:
       (LetUnpackT binds _ bod)    -> ProdTy (map snd binds) : go bod
       (LetAllocT _ vals bod)      -> ProdTy (map fst vals) : go bod

       (IfT _ a b) -> go a ++ go b
       ErrT{} -> []
       (Switch _ (IntAlts ls) b) -> concatMap (go . snd) ls ++ concatMap go (maybeToList b)
       (Switch _ (TagAlts ls) b) -> concatMap (go . snd) ls ++ concatMap go (maybeToList b)
       (TailCall _ _)    -> []

--------------------------------------------------------------------------------
-- * C codegen

-- | Slightly different entrypoint than mkProgram that enables a
-- "main" expression.
codegenProg :: Prog -> IO String
codegenProg prg@(Prog funs mtal) = do
      env <- getEnvironment
      let rtsPath = case lookup "TREELANGDIR" env of
                      Just p -> p ++"/gibbon-compiler/rts.c"
                      Nothing -> "rts.c" -- Assume we're running from the compiler dir!
      e <- doesFileExist rtsPath
      unless e $ error$ "codegen: rts.c file not found at path: "++rtsPath
                       ++"\n Consider setting TREELANGDIR to repo root.\n"
      rts <- readFile rtsPath -- TODO (maybe): We can read this in in compile time using TH
      return (rts ++ '\n' : pretty 80 (stack (map ppr defs)))
    where
      defs = fst $ runSyM 0 $ do
        funs' <- mapM codegenFun funs
        prots <- mapM makeProt funs
        main_expr' <- main_expr
        return (makeStructs (S.toList $ harvestStructTys prg) ++ prots ++ funs' ++ main_expr' : bench_fn)

      bench_fn :: [C.Definition]
      bench_fn =
        case mtal of
          Just (RunWithRacketFile fun) ->
              [cunit|
                $ty:(codegenTy IntTy) __fn_with_file( $ty:(codegenTy PtrTy) in ) {
                  return $(cid fun)(in);
                } |]
          Just (RunRacketCorePass build_tree bench) ->
            [cunit|
              $ty:(codegenTy IntTy) __fn_with_file( $ty:(codegenTy PtrTy) in ) {
                fprintf(stderr, "Benchmark is not implemented for this program.\n");
                exit(1);
              }              
              void __fn_to_bench( $ty:(codegenTy PtrTy) in, $ty:(codegenTy PtrTy) out) {
                  $(cid bench)(in, out);
              }
              void __build_tree( $ty:(codegenTy IntTy) tree_size, char* buffer) {
                  $(cid build_tree)(tree_size, buffer);
              } |]
          Just (PrintExp _) -> def 
          Nothing -> def
         where              
          def = [cunit|
              $ty:(codegenTy IntTy) __fn_with_file( $ty:(codegenTy PtrTy) in ) {
                fprintf(stderr, "Benchmark is not implemented for this program.\n");
                exit(1);
              }              
              void __fn_to_bench( $ty:(codegenTy PtrTy) in, $ty:(codegenTy PtrTy) out) {
                fprintf(stderr, "Benchmark is not implemented for this program.\n");
                exit(1);
              }
              void __build_tree( $ty:(codegenTy IntTy) tree_size, char* buffer) {
                fprintf(stderr, "Benchmark is not implemented for this program.\n");
                exit(1);
              } |]

      main_expr :: SyM C.Definition
      main_expr =
        case mtal of
          Just (PrintExp t) -> do
            t' <- codegenTail t (codegenTy IntTy)
            return $ C.FuncDef [cfun| void __main_expr() { $items:t' } |] noLoc
          _ ->
            return $ C.FuncDef [cfun| void __main_expr() { return 0; } |] noLoc

makeProt :: FunDecl -> SyM C.Definition
makeProt fd = do fn <- codegenFun' fd  -- This is bad and I apologize
                 return $ C.DecDef (C.funcProto fn) noLoc

codegenFun' :: FunDecl -> SyM C.Func
codegenFun' (FunDecl nam args ty tal) =
    do let retTy   = codegenTy ty
           params  = map (\(v,t) -> [cparam| $ty:(codegenTy t) $id:v |]) args
       body <- codegenTail tal retTy
       let fun     = [cfun| $ty:retTy $id:nam ($params:params) {
                            $items:body
                     } |]
       return fun

codegenFun :: FunDecl -> SyM C.Definition
codegenFun fd =
    do fun <- codegenFun' fd
       return $ C.FuncDef fun noLoc

makeStructs :: [[Ty]] -> [C.Definition]
makeStructs [] = []
makeStructs (ts : ts') = d : makeStructs ts'
    where strName = makeName ts
          d = [cedecl| typedef struct $id:(strName ++ "_struct") { $sdecls:decls } $id:strName; |]
          decls = zipWith (\t n -> [csdecl| $ty:(codegenTy t) $id:("field"++(show n)); |])
                  ts [0 :: Int ..]

-- | Replace returns with assignments to a given set of destinations.
rewriteReturns :: Tail -> [(Var,Ty)] -> Tail
rewriteReturns tl bnds =
 let go x = rewriteReturns x bnds in
 case tl of
   (RetValsT ls) -> AssnValsT [ (v,t,e) | (v,t) <- bnds | e <- ls ]

   -- Here we've already rewritten the tail to assign values
   -- somewhere.. and now we want to REREWRITE it?
   (AssnValsT _) -> error$ "rewriteReturns: Internal invariant broken:\n "++sdoc tl
   (e@LetCallT{bod})     -> e{bod = go bod }
   (e@LetPrimCallT{bod}) -> e{bod = go bod }
   (e@LetTrivT{bod})     -> e{bod = go bod }
   -- We don't recur on the "tails" under the if, because they're not
   -- tail with respect to our redex:
   (LetIfT bnd (a,b,c) bod) -> LetIfT bnd (a,b,c) (go bod)
   (LetTimedT flg bnd rhs bod) -> LetTimedT flg bnd rhs (go bod)
   (LetUnpackT bs scrt body) -> LetUnpackT bs scrt (go body)
   (LetAllocT lhs vals body) -> LetAllocT lhs vals (go body)
   (IfT a b c) -> IfT a (go b) (go c)
   (ErrT s) -> (ErrT s)
   (Switch tr alts def) -> Switch tr (mapAlts go alts) (fmap go def)
   -- Oops, this is not REALLY a tail call.  Hoist it and go under:
   (TailCall f rnds) -> let (vs,ts) = unzip bnds
                            vs' = map (++"hack") vs -- FIXME: Gensym
                        in LetCallT (zip vs' ts) f rnds
                            (rewriteReturns (RetValsT (map VarTriv vs')) bnds)
 where
   mapAlts f (TagAlts ls) = TagAlts $ zip (map fst ls) (map (f . snd) ls)
   mapAlts f (IntAlts ls) = IntAlts $ zip (map fst ls) (map (f . snd) ls)

                            
-- dummyLoc :: SrcLoc
-- dummyLoc = (SrcLoc (Loc (Pos "" 0 0 0) (Pos "" 0 0 0)))
                            
codegenTriv :: Triv -> C.Exp
codegenTriv (VarTriv v) = C.Var (C.toIdent v noLoc) noLoc
codegenTriv (IntTriv i) = [cexp| $llint:i |]  -- Must be consistent with codegenTy IntTy
codegenTriv (TagTriv i) = [cexp| (char)$i |]  -- Must be consistent with codegenTy TagTyPacked

codegenTail :: Tail -> C.Type -> SyM [C.BlockItem]

-- Void type:
codegenTail (RetValsT []) _ty   = return [ C.BlockStm [cstm| return; |] ] 
-- Single return:
codegenTail (RetValsT [tr]) _ty = return [ C.BlockStm [cstm| return $(codegenTriv tr); |] ]
-- Multiple return:
codegenTail (RetValsT ts) ty =
    return $ [ C.BlockStm [cstm| return $(C.CompoundLit ty args noLoc); |] ]
    where args = map (\a -> (Nothing,C.ExpInitializer (codegenTriv a) noLoc)) ts

codegenTail (AssnValsT ls) _ty =
    return $ [ mut (codegenTy ty) vr (codegenTriv triv) | (vr,ty,triv) <- ls ]

-- FIXME : This needs to actually generate a SWITCH!
codegenTail (Switch tr alts def) ty =
    do let trE = codegenTriv tr
           mk_tag_lhs lhs = C.Const (C.IntConst (show lhs) C.Unsigned (fromIntegral lhs) noLoc) noLoc
           mk_int_lhs lhs = C.Const (C.IntConst (show lhs) C.Signed   (fromIntegral lhs) noLoc) noLoc
           alts' = case alts of
                     TagAlts as -> map (first mk_tag_lhs) as
                     IntAlts as -> map (first mk_int_lhs) as
       altPairs <- forM alts' $ \(lhs, rhs) ->
                   do tal <- codegenTail rhs ty
                      return (C.BinOp C.Eq trE lhs noLoc, mkBlock tal)
       let mkIf [] = case def of
                       Nothing -> return $ Nothing
                       Just def' -> do tal <- codegenTail def' ty
                                       return $ Just $ mkBlock tal
           mkIf ((cond,rhs) : rest) = do rest' <- mkIf rest
                                         return $ Just $ C.If cond rhs rest' noLoc
       ifExpr <- mkIf altPairs
       return $ [ C.BlockStm (fromJust ifExpr) ]

codegenTail (TailCall v ts) _ty =
    return $ [ C.BlockStm [cstm| return $( C.FnCall (cid v) (map codegenTriv ts) noLoc ); |] ]

codegenTail (IfT e0 e1 e2) ty = do
    e1' <- codegenTail e1 ty
    e2' <- codegenTail e2 ty
    return $ [ C.BlockStm [cstm| if ($(codegenTriv e0)) { $items:e1' } else { $items:e2' } |] ]

codegenTail (ErrT s) _ty = return $ [ C.BlockStm [cstm| printf("%s\n", $s); |]
                                    , C.BlockStm [cstm| exit(1); |] ]


-- We could eliminate these earlier
codegenTail (LetTrivT (vr,rty,rhs) body) ty =
    do tal <- codegenTail body ty
       return $ [ C.BlockDecl [cdecl| $ty:(codegenTy rty) $id:vr = $(codegenTriv rhs); |] ]
                ++ tal 

codegenTail (LetAllocT lhs vals body) ty =
    do let structTy = codegenTy (ProdTy (map fst vals))
           size = [cexp| sizeof($ty:structTy) |]
       tal <- codegenTail body ty
       return$ assn (codegenTy PtrTy) lhs [cexp| ( $ty:structTy *)ALLOC( $size ) |] :
               [ C.BlockStm [cstm| (($ty:structTy *)  $id:lhs)->$id:fld = $(codegenTriv trv); |]
               | (ix,(_ty,trv)) <- zip [0 :: Int ..] vals
               , let fld = "field"++show ix] ++ tal

codegenTail (LetUnpackT bs scrt body) ty =
    do let mkFld :: Int -> C.Id
           mkFld i = C.toIdent ("field" ++ show i) noLoc
        
           fldTys = map snd bs
           struct_ty = codegenTy (ProdTy fldTys)

           mk_bind i (v, t) = [cdecl|
             $ty:(codegenTy t) $id:v = ( ( $ty:struct_ty * ) $exp:(cid scrt) )->$id:(mkFld i);
           |]

           binds = zipWith mk_bind [0..] bs

       body' <- codegenTail body ty
       return (map C.BlockDecl binds ++ body')

-- Here we unzip the tuple into assignments to local variables.
codegenTail (LetIfT bnds (e0,e1,e2) body) ty =

    do let decls = [ C.BlockDecl [cdecl| $ty:(codegenTy ty0) $id:vr0; |]
                   | (vr0,ty0) <- bnds ]
       let e1' = rewriteReturns e1 bnds
           e2' = rewriteReturns e2 bnds
       e1'' <- codegenTail e1' ty
       e2'' <- codegenTail e2' ty
       -- Int 1 is Boolean true:
       let ifbod = [ C.BlockStm [cstm| if ($(codegenTriv e0)) { $items:e1'' } else { $items:e2'' } |] ]
       tal <- codegenTail body ty
       return $ decls ++ ifbod ++ tal

codegenTail (LetTimedT flg bnds rhs body) ty =

    do let decls = [ C.BlockDecl [cdecl| $ty:(codegenTy ty0) $id:vr0; |]
                   | (vr0,ty0) <- bnds ]
       let rhs' = rewriteReturns rhs bnds
       rhs'' <- codegenTail rhs' ty
       let ident = case bnds of
                     ((v,_):_) -> v
                     _ -> ""
           begn  = "begin_"++ident
           end   = "end_"++ident
           iters = "iters_"++ident
       let timebod = [ C.BlockDecl [cdecl| struct timespec $id:begn; |]
                     , C.BlockStm [cstm| clock_gettime(CLOCK_MONOTONIC_RAW, & $id:begn );  |]
                     , C.BlockStm (if flg
                                   then [cstm| for (long long $id:iters = 0; $id:iters < global_iters_param; $id:iters ++) { $items:rhs'' }  |]
                                   else [cstm| { $items:rhs'' } |])
                     , C.BlockDecl [cdecl| struct timespec $id:end; |]
                     , C.BlockStm [cstm| clock_gettime(CLOCK_MONOTONIC_RAW, &$(cid end)); |]
                     ]
           withPrnt = timebod ++
                       if flg
                       then [ C.BlockStm [cstm| printf("ITERS: %lld\n", global_iters_param); |]
                            , C.BlockStm [cstm| printf("SIZE: %lld\n", global_size_param); |]
                            , C.BlockStm [cstm| printf("BATCHTIME: %lf\n", difftimespecs(&$(cid begn), &$(cid end))); |]
                            ]
                       else [ C.BlockStm [cstm| printf("SELFTIMED: %lf\n", difftimespecs(&$(cid begn), &$(cid end))); |] ]
       tal <- codegenTail body ty
       return $ decls ++ withPrnt ++ tal

              

codegenTail (LetCallT bnds ratr rnds body) ty
    | (length bnds) > 1 = do nam <- gensym "tmp_struct"
                             let bind (v,t) f = assn (codegenTy t) v (C.Member (cid nam) (C.toIdent f noLoc) noLoc)
                                 fields = map (\i -> "field" ++ show i) [0 :: Int .. length bnds - 1]
                                 ty0 = ProdTy $ map snd bnds
                                 init = [ C.BlockDecl [cdecl| $ty:(codegenTy ty0) $id:nam = $(C.FnCall (cid ratr) (map codegenTriv rnds) noLoc); |] ]
                             tal <- codegenTail body ty
                             return $ init ++ zipWith bind bnds fields ++ tal
    | otherwise = do tal <- codegenTail body ty
                     let call = assn (codegenTy (snd $ bnds !! 0)) (fst $ bnds !! 0) (C.FnCall (cid ratr) (map codegenTriv rnds) noLoc)
                     return $ [call] ++ tal


codegenTail (LetPrimCallT bnds prm rnds body) ty =
    do bod' <- codegenTail body ty
       let pre  = case prm of
                    AddP -> let [(outV,outT)] = bnds
                                [pleft,pright] = rnds
                            in [ C.BlockDecl [cdecl| $ty:(codegenTy outT) $id:outV = $(codegenTriv pleft) + $(codegenTriv pright); |] ]
                    SubP -> let (outV,outT) = head bnds
                                [pleft,pright] = rnds
                            in [ C.BlockDecl [cdecl| $ty:(codegenTy outT) $id:outV = $(codegenTriv pleft) - $(codegenTriv pright); |] ]
                    MulP -> let [(outV,outT)] = bnds
                                [pleft,pright] = rnds
                            in [ C.BlockDecl [cdecl| $ty:(codegenTy outT) $id:outV = $(codegenTriv pleft) * $(codegenTriv pright); |]]
                    EqP -> let [(outV,outT)] = bnds
                               [pleft,pright] = rnds
                           in [ C.BlockDecl [cdecl| $ty:(codegenTy outT) $id:outV = ($(codegenTriv pleft) == $(codegenTriv pright)); |]]
                    DictInsertP IntTy -> let [(outV,ty)] = bnds
                                             [(VarTriv dict),keyTriv,valTriv] = rnds
                                         in [ C.BlockDecl [cdecl| $ty:(codegenTy ty) $id:outV = dict_insert_int($id:dict, $(codegenTriv keyTriv), $(codegenTriv valTriv)); |] ]
                    DictInsertP SymTy -> let [(outV,ty)] = bnds
                                             [(VarTriv dict),keyTriv,valTriv] = rnds
                                         in [ C.BlockDecl [cdecl| $ty:(codegenTy ty) $id:outV = dict_insert_int($id:dict, $(codegenTriv keyTriv), $(codegenTriv valTriv)); |] ]
                    DictInsertP ty -> error $ "DictInsertP not implemented for type " ++ (show ty)
                    DictLookupP IntTy -> let [(outV,IntTy)] = bnds
                                             [(VarTriv dict),keyTriv] = rnds
                                         in [ C.BlockDecl [cdecl| $ty:(codegenTy IntTy) $id:outV = dict_lookup_int($id:dict, $(codegenTriv keyTriv)); |] ]
                    DictLookupP SymTy -> let [(outV,IntTy)] = bnds
                                             [(VarTriv dict),keyTriv] = rnds
                                         in [ C.BlockDecl [cdecl| $ty:(codegenTy IntTy) $id:outV = dict_lookup_int($id:dict, $(codegenTriv keyTriv)); |] ]
                    DictLookupP ty -> error $ "DictLookupP not implemented for type " ++ (show ty)
                    DictEmptyP _ty -> let [(outV,ty)] = bnds
                                      in [ C.BlockDecl [cdecl| $ty:(codegenTy ty) $id:outV = 0; |] ]
                    NewBuf   -> let [(outV,CursorTy)] = bnds in
                                [ C.BlockDecl [cdecl| $ty:(codegenTy CursorTy) $id:outV = ( $ty:(codegenTy CursorTy) )ALLOC_PACKED(DEFAULT_BUF_SIZE); |] ]
                    ScopedBuf -> let [(outV,CursorTy)] = bnds in
                                [ C.BlockDecl [cdecl| $ty:(codegenTy CursorTy) $id:outV = ( $ty:(codegenTy CursorTy) )ALLOC_SCOPED(); |] ]
                    WriteTag -> let [(outV,CursorTy)] = bnds
                                    [(TagTriv tag),(VarTriv cur)] = rnds
                                in [ C.BlockStm [cstm| *($id:cur) = $tag; |]
                                   , C.BlockDecl [cdecl| char* $id:outV = $id:cur + 1; |] ]
                    WriteInt -> let [(outV,CursorTy)] = bnds
                                    [val,(VarTriv cur)] = rnds
                                in [ C.BlockStm [cstm| *( $ty:(codegenTy IntTy)  *)($id:cur) = $(codegenTriv val); |]
                                   , C.BlockDecl [cdecl| char* $id:outV = ($id:cur) + sizeof( $ty:(codegenTy IntTy) ); |] ]
                    ReadTag -> let [(tagV,TagTyPacked),(curV,CursorTy)] = bnds
                                   [(VarTriv cur)] = rnds
                               in [ C.BlockDecl [cdecl| $ty:(codegenTy TagTyPacked) $id:tagV = *($id:cur); |]
                                  , C.BlockDecl [cdecl| char* $id:curV = $id:cur + 1; |] ]
                    ReadInt -> let [(valV,IntTy),(curV,CursorTy)] = bnds
                                   [(VarTriv cur)] = rnds
                               in [ C.BlockDecl [cdecl| $ty:(codegenTy IntTy) $id:valV = *( $ty:(codegenTy IntTy) *)($id:cur); |]
                                  , C.BlockDecl [cdecl| char* $id:curV = ($id:cur) + sizeof( $ty:(codegenTy IntTy) ); |] ]

                    GetFirstWord ->
                     let [ptr] = rnds in
                     case bnds of
                       [(outV,outTy)] ->
                        [ C.BlockDecl [cdecl|
                            $ty:(codegenTy outTy) $id:outV =
                              * (( $ty:(codegenTy outTy) *) $(codegenTriv ptr));
                          |] ]
                       _ -> error $ "wrong number of return bindings from GetFirstWord: "++show bnds

                    SizeParam -> let [(outV,IntTy)] = bnds in
                                [ C.BlockDecl [cdecl| $ty:(codegenTy IntTy) $id:outV = global_size_param; |] ]

                    PrintInt | [] <- bnds -> let [arg] = rnds in
                                             [ C.BlockStm [cstm| printf("%lld", $(codegenTriv arg)); |] ]
                             | otherwise -> error$ "wrong number of return values expected from PrintInt prim: "++show bnds

                    PrintString str | [] <- bnds, [] <- rnds -> [ C.BlockStm [cstm| puts( $string:str ); |] ]
                                    | otherwise -> error$ "wrong number of args/return values expected from PrintString prim: "++show (rnds,bnds)

                    -- oth -> error$ "FIXME: codegen needs to handle primitive: "++show oth
       return $ pre ++ bod'

codegenTy :: Ty -> C.Type
                  -- ARGH: C-quote won't allow us to use a typedef here, e.g. "IntTy":
codegenTy IntTy = [cty|long long|] -- Must be consistent IntTy, rts.c
codegenTy TagTyPacked = [cty|char|]      -- Must be consistent TagTyPacked, rts.c
codegenTy TagTyBoxed  = codegenTy IntTy  -- Must be consistent TagTyBoxed, rts.c
codegenTy SymTy = [cty|long long|] -- Must be consistent SymTy, rts.c
codegenTy PtrTy = [cty|char*|] -- Hack, this could be void* if we have enough casts. [2016.11.06]
codegenTy CursorTy = [cty|char*|]
codegenTy (ProdTy []) = [cty|void*|]
codegenTy (ProdTy ts) = C.Type (C.DeclSpec [] [] (C.Tnamed (C.Id nam noLoc) [] noLoc) noLoc) (C.DeclRoot noLoc) noLoc
    where nam = makeName ts
codegenTy (SymDictTy _t) = C.Type (C.DeclSpec [] [] (C.Tnamed (C.Id "dict_item_t*" noLoc) [] noLoc) noLoc) (C.DeclRoot noLoc) noLoc

makeName :: [Ty] -> String
makeName tys = concatMap makeName' tys ++ "Prod"

makeName' :: Ty -> String
makeName' IntTy = "Int64"
makeName' CursorTy = "Cursor"
makeName' TagTyPacked = "Tag"
-- makeName' TagTyBoxed  = "Btag"
makeName' TagTyBoxed  = makeName' IntTy
makeName' PtrTy = "Ptr"
makeName' (SymDictTy _ty) = "Dict"
makeName' x = error $ "makeName', not handled: " ++ show x

mkBlock :: [C.BlockItem] -> C.Stm
mkBlock ss = C.Block ss noLoc

cid :: Var -> C.Exp
cid v = C.Var (C.toIdent v noLoc) noLoc

-- | Create a NEW lexical binding.
assn :: (C.ToIdent v, C.ToExp e) => C.Type -> v -> e -> C.BlockItem
assn t x y = C.BlockDecl [cdecl| $ty:t $id:x = $exp:y; |]

-- | Mutate an existing binding:
mut :: (C.ToIdent v, C.ToExp e) => C.Type -> v -> e -> C.BlockItem
mut _t x y = C.BlockStm [cstm| $id:x = $exp:y; |]


