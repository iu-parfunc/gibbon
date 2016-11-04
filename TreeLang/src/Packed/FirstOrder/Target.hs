{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Defines the target language for first-order L1 language with C code
-- generator for it.

module Packed.FirstOrder.Target
    ( Var, Tag, Tail(..), Triv(..), Ty(..), Prim(..), FunDecl(..),
      Alts(..), Prog(..), MainExp(..),
      codegenProg,
    ) where

--------------------------------------------------------------------------------

-- import Packed.FirstOrder.L1_Source ()

import Control.DeepSeq
import Control.Monad
import Data.List (nub)
import Data.Word (Word8)

import Data.Bifunctor (first)
import Data.Loc (noLoc)
import Data.Maybe (fromJust)
-- import Data.Traversable
import GHC.Generics (Generic)
import Language.C.Quote.C (cdecl, cedecl, cexp, cfun, cparam, csdecl, cstm, cty,
                           cunit)
import qualified Language.C.Quote.C as C
import qualified Language.C.Syntax as C
import Prelude hiding (init)
import System.Environment
import System.Directory
import Text.PrettyPrint.GenericPretty (Out (..))
import Text.PrettyPrint.Mainland

import Packed.FirstOrder.Common

--------------------------------------------------------------------------------
-- * AST definition

data Prog = Prog
  { fundefs :: [FunDecl]
  , mainExp :: Maybe MainExp
  } deriving (Show, Ord, Eq, Generic, NFData, Out)

data MainExp
  = PrintExp Tail
      -- ^ Evaluate the expression and print the result. Type of the expression
      -- must be `int`.
  | RunRacketCorePass Var Var
      -- ^ Run the pass. First `Var` is a function for building initial trees,
      -- second `Var` is the function to benchmark. Return value of benchmark
      -- function is ignored.
  deriving (Show, Ord, Eq, Generic, NFData, Out)

type Tag = Word8

data Triv
    = VarTriv Var
    | IntTriv Int
    | TagTriv Tag
  deriving (Show, Ord, Eq, Generic, NFData, Out)

-- | Switch alternatives.
data Alts
  = TagAlts [(Tag, Tail)]
      -- ^ Casing on tags.
  | IntAlts [(Int, Tail)]
      -- ^ Casing on integers.
  deriving (Show, Ord, Eq, Generic, NFData, Out)

instance Out Word8 where
  doc w = doc (fromIntegral w :: Int)
  docPrec n w = docPrec n (fromIntegral w :: Int)

data Tail
    = RetValsT [Triv] -- ^ Only in tail position, for returning from a function.
    | AssnValsT [(Var,Ty,Triv)] -- ^ INTERNAL ONLY: used for assigning instead of returning.

    | LetCallT { binds :: [(Var,Ty)],
                 rator :: Var,
                 rands :: [Triv],
                 bod   :: Tail }
    | LetPrimCallT { binds :: [(Var,Ty)],
                     prim  :: Prim,
                     rands :: [Triv],
                     bod   :: Tail }

    -- Ugh, we should not strictly need this if we simplify everything in the right way:
    | LetTrivT { bnd :: (Var,Ty,Triv)
               , bod :: Tail }

    -- A control-flow join point; an If on the RHS of LeT:
    | LetIfT { binds :: [(Var,Ty)]
             , ife :: (Triv,Tail,Tail)
             , bod :: Tail
             }

    | LetUnpackT { binds :: [(Var,Ty)]
                 , ptr :: (Var,[Ty])
                 , bod :: Tail }
    -- ^ Unpack a struct pointer (variable of PtrTy) into local fields.

    | LetAllocT { lhs  :: Var -- ^ Var to bind to PtrTy
                , vals :: [(Ty,Triv)] -- ^ Fields of allocated struct
                , bod  :: Tail }
    -- ^ Allocate storage for a struct of the given type,
    --   Initialize all fields Return PtrTy.


    | IfT { tst :: Triv,
            con  :: Tail,
            els  :: Tail }
    | ErrT String
    | StartTimerT Var Tail
    | EndTimerT   Var Tail
    | Switch Triv Alts (Maybe Tail)
    -- ^ For casing on numeric tags or integers.
    | TailCall Var [Triv]
  deriving (Show, Ord, Eq, Generic, NFData, Out)

data Ty
    = IntTy
    | TagTy -- ^ A single byte / Word8.  Used in PACKED mode.
    | SymTy -- ^ Symbols used in writing compiler passes.
            --   It's an alias for Int, an index into a symbol table.
    | CursorTy -- ^ A byte-indexing pointer.

    | PtrTy   -- ^ A machine word.  Same as IntTy.  Untyped.
    | StructPtrTy { fields :: [Ty] } -- ^ A pointer to a struct containing the given fields.

    | ProdTy [Ty]
    | SymDictTy Ty
      -- ^ We allow built-in dictionaries from symbols to a value type.
  deriving (Show, Ord, Eq, Generic, NFData, Out)

data Prim
    = AddP
    | SubP
    | MulP
    | EqP
    | DictInsertP -- ^ takes k,v,dict
    | DictLookupP -- ^ takes k,dict, errors if absent
    | NewBuf
    -- ^ Allocate a new buffer, return a cursor.
    | WriteTag
    -- ^ Write a static tag value, takes a cursor to target.
    | WriteInt
    -- ^ Write (leaf) data, takes cursor,int
    | ReadTag
    -- ^ Read one byte from the cursor and advance it.
    | ReadInt
      -- ^ Read an 8 byte Int from the cursor and advance.

    | GetFirstWord -- ^ takes a PtrTy, returns IntTy containing the (first) word pointed to.

  deriving (Show, Ord, Eq, Generic, NFData, Out)

data FunDecl = FunDecl
  { funName  :: Var
  , funArgs  :: [(Var,Ty)]
  , funRetTy :: Ty
  , funBody  :: Tail
  } deriving (Show, Ord, Eq, Generic, NFData, Out)

--------------------------------------------------------------------------------
-- * C codegen

-- | Slightly different entrypoint than mkProgram that enables a
-- "main" expression.
codegenProg :: Prog -> IO String
codegenProg (Prog funs mtal) = do
      env <- getEnvironment
      let rtsPath = case lookup "TREELANGDIR" env of
                      Just p -> p ++"/TreeLang/rts.c"
                      Nothing -> "rts.c" -- Assume we're running from the compiler dir!
      e <- doesFileExist rtsPath
      unless e $ error$ "codegen: rts.c file not found at path: "++rtsPath
                       ++"\n Consider setting TREELANGDIR to repo root.\n"
      rts <- readFile rtsPath -- TODO (maybe): We can read this in in compile time using TH
      return (rts ++ '\n' : pretty 80 (stack (map ppr defs)))
    where
      defs = fst $ runSyM 0 $ do
        funs' <- mapM codegenFun funs
        let structs f = makeStructs $ nub [ tys | ProdTy tys <- funTys f ]
        main_expr' <- main_expr
        return (concatMap structs funs ++ funs' ++ main_expr' : bench_fn)

      bench_fn :: [C.Definition]
      bench_fn =
        case mtal of
          Just (RunRacketCorePass build_tree bench) ->
            [cunit|
              void __fn_to_bench(char* in, char* out) {
                  $(cid bench)(in, out);
              }
              void __build_tree(int tree_size, char* buffer) {
                  $(cid build_tree)(tree_size, buffer);
              } |]
          _ ->
            [cunit|
              void __fn_to_bench(char* in, char* out) {
                fprintf(stderr, "Benchmark is not implemented for this program.\n");
                exit(1);
              }
              void __build_tree(int tree_size, char* buffer) {
                fprintf(stderr, "Benchmark is not implemented for this program.\n");
                exit(1);
              } |]

      main_expr :: SyM C.Definition
      main_expr =
        case mtal of
          Just (PrintExp t) -> do
            t' <- codegenTail t (codegenTy IntTy)
            return $ C.FuncDef [cfun| int __main_expr() { $items:t' } |] noLoc
          _ ->
            return $ C.FuncDef [cfun| int __main_expr() { return 0; } |] noLoc

funTys :: FunDecl -> [Ty]
funTys (FunDecl _ args ty _) = ty : (map snd args)

codegenFun :: FunDecl -> SyM C.Definition
codegenFun (FunDecl nam args ty tal) =
    do let retTy   = codegenTy ty
           params  = map (\(v,t) -> [cparam| $ty:(codegenTy t) $id:v |]) args
       body <- codegenTail tal retTy
       let fun     = [cfun| $ty:retTy $id:nam ($params:params) {
                            $items:body
                     } |]
       return $ C.FuncDef fun noLoc

makeStructs :: [[Ty]] -> [C.Definition]
makeStructs [] = []
makeStructs (ts : ts') = d : makeStructs ts'
    where strName = makeName ts
          d = [cedecl| typedef struct $id:(strName ++ "_struct") { $sdecls:decls } $id:strName; |]
          decls = zipWith (\t n -> [csdecl| $ty:(codegenTy t) $id:("field"++(show n)); |])
                  ts [0 :: Int ..]

mapAlts :: (Tail->Tail) -> Alts -> Alts
mapAlts f (TagAlts ls) = TagAlts $ zip (map fst ls) (map (f . snd) ls)
mapAlts f (IntAlts ls) = IntAlts $ zip (map fst ls) (map (f . snd) ls)

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
   (IfT a b c) -> IfT a (go b) (go c)
   (ErrT s) -> (ErrT s)
   (StartTimerT v x2) -> StartTimerT v $ go x2
   (EndTimerT v x2)   -> EndTimerT v $ go x2
   (Switch tr alts def) -> Switch tr (mapAlts go alts) (fmap go def)
   -- Oops, this is not REALLY a tail call.  Hoist it and go under:
   (TailCall f rnds) -> let (vs,ts) = unzip bnds
                            vs' = map (++"hack") vs -- FIXME: Gensym
                        in LetCallT (zip vs' ts) f rnds
                            (rewriteReturns (RetValsT (map VarTriv vs')) bnds)


codegenTriv :: Triv -> C.Exp
codegenTriv (VarTriv v) = C.Var (C.toIdent v noLoc) noLoc
codegenTriv (IntTriv i) = [cexp| $i |]
codegenTriv (TagTriv i) = [cexp| $i |]

codegenTail :: Tail -> C.Type -> SyM [C.BlockItem]

codegenTail (RetValsT [tr]) _ty = return $ [ C.BlockStm [cstm| return $(codegenTriv tr); |] ]

codegenTail (RetValsT ts) ty =
    return $ [ C.BlockStm [cstm| return $(C.CompoundLit ty args noLoc); |] ]
    where args = map (\a -> (Nothing,C.ExpInitializer (codegenTriv a) noLoc)) ts

codegenTail (AssnValsT ls) _ty =
    return $ [ mut (codegenTy ty) vr (codegenTriv triv) | (vr,ty,triv) <- ls ]

codegenTail (RetValsT ts) ty =
    return $ [ C.BlockStm [cstm| return $(C.CompoundLit ty args noLoc); |] ]
    where args = map (\a -> (Nothing,C.ExpInitializer (codegenTriv a) noLoc)) ts


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

codegenTail (ErrT s) _ty = return $ [ C.BlockStm [cstm| printf("$s\n"); |]
                                    , C.BlockStm [cstm| exit(1); |] ]

codegenTail (StartTimerT begin tal) ty =
    do tal' <- codegenTail tal ty
       return $ [ C.BlockDecl [cdecl| struct timespec $id:begin; |]
                , C.BlockStm [cstm| clock_gettime(CLOCK_MONOTONIC_RAW, &$(cid begin)); |]
                ] ++ tal'

codegenTail (EndTimerT begin tal) ty =
    do end <- gensym "endtmr"
       tal' <- codegenTail tal ty
       return $ [ C.BlockDecl [cdecl| struct timespec $id:end; |]
                , C.BlockStm [cstm| clock_gettime(CLOCK_MONOTONIC_RAW, &$(cid end)); |]
                , C.BlockStm [cstm| printf("SELFTIMED: %lf\n", difftimespecs(&$(cid begin), &$(cid end))); |]
                ] ++ tal'

-- We could eliminate these earlier
codegenTail (LetTrivT (vr,rty,rhs) body) ty =
    do tal <- codegenTail body ty
       return $ [ C.BlockDecl [cdecl| $ty:(codegenTy rty) $id:vr = $(codegenTriv rhs); |] ]
                ++ tal

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


codegenTail (LetCallT bnds ratr rnds body) ty
    | (length bnds) > 1 = do nam <- gensym "tmp_struct"
                             let init = [ C.BlockDecl [cdecl| $ty:(codegenTy ty0) $id:nam = $(C.FnCall (cid ratr) (map codegenTriv rnds) noLoc); |] ]
                                 bind (v,t) f = assn (codegenTy t) v (C.Member (cid nam) (C.toIdent f noLoc) noLoc)
                                 fields = map (\i -> "field" ++ show i) [0 :: Int .. length bnds - 1]
                                 ty0 = ProdTy $ map snd bnds
                             tal <- codegenTail body ty
                             return $ init ++ zipWith bind bnds fields ++ tal
    | otherwise = do tal <- codegenTail body ty
                     return $ assn (codegenTy (snd $ bnds !! 0)) (fst $ bnds !! 0) (C.FnCall (cid ratr) (map codegenTriv rnds) noLoc) : tal


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
                    DictInsertP -> unfinished 1
                    DictLookupP -> unfinished 2
                    NewBuf -> unfinished 3
                    WriteTag -> let [(outV,CursorTy)] = bnds
                                    [(TagTriv tag),(VarTriv cur)] = rnds
                                in [ C.BlockStm [cstm| *($id:cur) = $tag; |]
                                   , C.BlockDecl [cdecl| char* $id:outV = $id:cur + 1; |] ]
                    WriteInt -> let [(outV,CursorTy)] = bnds
                                    [val,(VarTriv cur)] = rnds
                                in [ C.BlockStm [cstm| *(int*)($id:cur) = $(codegenTriv val); |]
                                   , C.BlockDecl [cdecl| char* $id:outV = ($id:cur) + sizeof(int); |] ]
                    ReadTag -> let [(tagV,TagTy),(curV,CursorTy)] = bnds
                                   [(VarTriv cur)] = rnds
                               in [ C.BlockDecl [cdecl| $ty:(codegenTy TagTy) $id:tagV = *($id:cur); |]
                                  , C.BlockDecl [cdecl| char* $id:curV = $id:cur + 1; |] ]
                    ReadInt -> let [(valV,IntTy),(curV,CursorTy)] = bnds
                                   [(VarTriv cur)] = rnds
                               in [ C.BlockDecl [cdecl| int $id:valV = *(int*)($id:cur); |]
                                  , C.BlockDecl [cdecl| char* $id:curV = ($id:cur) + sizeof(int); |] ]
       return $ pre ++ bod'

codegenTy :: Ty -> C.Type
codegenTy IntTy = [cty|int|]
codegenTy TagTy = [cty|int|]
codegenTy SymTy = [cty|int|]
codegenTy CursorTy = [cty|char*|]
codegenTy (ProdTy ts) = C.Type (C.DeclSpec [] [] (C.Tnamed (C.Id nam noLoc) [] noLoc) noLoc) (C.DeclRoot noLoc) noLoc
    where nam = makeName ts
codegenTy (SymDictTy _t) = unfinished 4

makeName :: [Ty] -> String
makeName []            = "Prod"
makeName (IntTy:ts)    = "Int" ++ makeName ts
makeName (CursorTy:ts) = "Cursor" ++ makeName ts
makeName _             = unfinished 5

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

unfinished :: Int -> a
unfinished n = error $ "Target.hs: unfinished hole #"++show n
