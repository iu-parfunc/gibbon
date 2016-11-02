{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Defines the target language for first-order L1 language with C code
-- generator for it.

module Packed.FirstOrder.Target
    ( Var, Tag, Tail(..), Triv(..), Ty(..), Prim(..), FunDecl(..),
      Alts(..), Prog(..),
      codegenProg, codegenFun, mkProgram, writeProgram,
      -- Examples, temporary:
      exadd1, exadd1Tail, add1C, buildTreeC
    ) where

--------------------------------------------------------------------------------

-- import Packed.FirstOrder.L1_Source ()

import Control.DeepSeq
import Data.List (nub)
import Data.Word (Word8)

import Data.Bifunctor (first)
import Data.Loc (noLoc)
import Data.Maybe (fromJust)
import Data.Traversable
import GHC.Generics (Generic)
import Language.C.Quote.C (cdecl, cedecl, cexp, cfun, cparam, csdecl, cstm, cty,
                           cunit)
import qualified Language.C.Quote.C as C
import qualified Language.C.Syntax as C
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.GenericPretty (Out(..))
import Prelude hiding (init)
    
import Packed.FirstOrder.Common
--------------------------------------------------------------------------------
-- * AST definition

data Prog = Prog { fundefs :: [FunDecl]
                 , mainExp :: (Maybe Tail) }
  deriving (Show, Read, Ord, Eq, Generic, NFData) 

instance Out Prog
          
type Tag = Word8

data Triv
    = VarTriv Var
    | IntTriv Int
    | TagTriv Tag
  deriving (Show, Read, Ord, Eq, Generic, NFData)
instance Out Triv

-- | Switch alternatives.
data Alts
  = TagAlts [(Tag, Tail)]
      -- ^ Casing on tags.
  | IntAlts [(Int, Tail)]
      -- ^ Casing on integers.
  deriving (Show, Read, Ord, Eq, Generic, NFData)
instance Out Alts

instance Out Word8 where
  doc w = doc (fromIntegral w :: Int)
  docPrec n w = docPrec n (fromIntegral w :: Int)
    
data Tail
    = RetValsT [Triv] -- ^ Only in tail position, for returning from a function.
    | LetCallT { binds  :: [(Var,Ty)],
                 rator  :: Var,
                 rands  :: [Triv],
                 bod    :: Tail }
    | LetPrimCallT { binds :: [(Var,Ty)],
                     prim  :: Prim,
                     rands :: [Triv],
                     bod   :: Tail }
    | IfEqT { val1 :: Var,
              val2 :: Var,
              con  :: Tail,
              els  :: Tail }
    | ErrT
    | TimeT Tail
    | Switch Triv Alts (Maybe Tail)
    -- ^ For casing on numeric tags or integers.
    | TailCall Var [Triv]
  deriving (Show, Read, Ord, Eq, Generic, NFData)
instance Out Tail

data Ty
    = IntTy
    | TagTy -- ^ A single byte / Word8
    | SymTy -- ^ Symbols used in writing compiler passes.
            --   It's an alias for Int, an index into a symbol table.
    | CursorTy -- ^ A byte-indexing pointer.
    | ProdTy [Ty]
    | SymDictTy Ty
      -- ^ We allow built-in dictionaries from symbols to a value type.
  deriving (Show, Read, Ord, Eq, Generic, NFData)

instance Out Ty

data Prim
    = AddP
    | SubP
    | MulP
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
  deriving (Show, Read, Ord, Eq, Generic, NFData)
instance Out Prim
           
data FunDecl = FunDecl
  { funName  :: Var
  , funArgs  :: [(Var,Ty)]
  , funRetTy :: Ty
  , funBody  :: Tail
  } deriving (Show, Read, Ord, Eq, Generic, NFData)

instance Out FunDecl

--------------------------------------------------------------------------------
-- * C codegen

-- | Slightly different entrypoint than mkProgram that enables a
-- "main" expression.
codegenProg :: Prog -> String
codegenProg (Prog funs mtal) = pretty 80 (stack (map ppr defs))
    where defs = fst $ runSyM 0 $ do funs' <- mapM codegenFun funs
                                     let structs f = makeStructs $ nub [ tys | ProdTy tys <- funTys f ]
                                     case mtal of
                                       Just _x -> error "main expr: Not handled yet" -- TODO: main function
                                       Nothing -> return $ (concatMap structs funs) ++ funs'

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

codegenTriv :: Triv -> C.Exp
codegenTriv (VarTriv v) = C.Var (C.toIdent v noLoc) noLoc
codegenTriv (IntTriv i) = [cexp| $i |]
codegenTriv (TagTriv i) = [cexp| $i |]

codegenTail :: Tail -> C.Type -> SyM [C.BlockItem]
               
codegenTail (RetValsT [tr]) _ty = return $ [ C.BlockStm [cstm| return $(codegenTriv tr); |] ]
                                  
codegenTail (RetValsT ts) ty =
    return $ [ C.BlockStm [cstm| return $(C.CompoundLit ty args noLoc); |] ]
    where args = map (\a -> (Nothing,C.ExpInitializer (codegenTriv a) noLoc)) ts
                 
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

codegenTail (IfEqT v1 v2 e1 e2) ty = do
    e1' <- codegenTail e1 ty
    e2' <- codegenTail e2 ty
    return $ [ C.BlockStm [cstm| if ($(cid v1) == $(cid v2)) { $items:e1' } else { $items:e2' } |] ]

codegenTail ErrT _ty = return $ [ C.BlockStm [cstm| printf("error\n"); |]
                                , C.BlockStm [cstm| exit(1); |] ]

codegenTail (TimeT tal) ty =
    do begin <- gensym "tmp_begin"
       end <- gensym "tmp_end"
       tal' <- codegenTail tal ty
       return $ [ C.BlockDecl [cdecl| struct timespec $id:begin; |]
                , C.BlockDecl [cdecl| struct timespec $id:end; |]
                , C.BlockStm [cstm| clock_gettime(CLOCK_MONOTONIC_RAW, &$(cid begin)); |]
                ] ++ tal' ++ [ C.BlockStm [cstm| clock_gettime(CLOCK_MONOTONIC_RAW, &$(cid end)); |]
                             , C.BlockStm [cstm| printf("TIMEIT: %lf\n", difftimespecs(&$(cid begin), &$(cid end))); |] ]

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

assn :: (C.ToIdent v, C.ToExp e) => C.Type -> v -> e -> C.BlockItem
assn t x y = C.BlockDecl [cdecl| $ty:t $id:x = $exp:y; |]

--------------------------------------------------------------------------------
-- * Runtime functions / entry point for C programs

includes :: String
includes = unlines
  [ "#include <assert.h>"
  , "#include <stdio.h>"
  , "#include <stdlib.h>"
  , "#include <string.h>"
  , "#include <time.h>"
  , ""
  ]

-- | Given function name to benchmark, generates `main()` that benchmarks.
mkRuntimeFuns :: String -> [C.Definition]
mkRuntimeFuns f = [cunit|
void show_usage()
{
    // TODO
    return;
}

double avg(const double* arr, int n)
{
    double sum = 0.0;
    for(int i=0; i<n; i++) sum += arr[i];
    return sum / (double)n;
}

double difftimespecs(struct timespec* t0, struct timespec* t1)
{
    return (double)(t1->tv_sec - t0->tv_sec)
      + ((double)(t1->tv_nsec - t0->tv_nsec) / 1000000000.0);
}

int compare_doubles (const void *a, const void *b)
{
    const double *da = (const double *) a;
    const double *db = (const double *) b;
    return (*da > *db) - (*da < *db);
}

void run(int num_iterations, int tree_size, int buffer_size)
{
    printf("Generating initial tree...\n");
    char* initial_buffer = (char*)malloc(buffer_size);
    assert(initial_buffer);
    build_tree(tree_size, initial_buffer);

    printf("Benchmarking. Iteration count: %d\n", num_iterations);
    char* bench_buffer = (char*)malloc(buffer_size);
    assert(bench_buffer);

    double trials[num_iterations];
    struct timespec begin, end;

    for (int i = 0; i < num_iterations; ++i)
    {
        clock_gettime(CLOCK_MONOTONIC_RAW, &begin);
        $(cid f)(initial_buffer, bench_buffer);
        clock_gettime(CLOCK_MONOTONIC_RAW, &end);
        trials[i] = difftimespecs(&begin, &end);
    }

    qsort(trials, num_iterations, sizeof(double), compare_doubles);
    printf("\nMINTIME: %lf\n",  trials[0]);
    printf("MEDIANTIME: %lf\n", trials[num_iterations / 2]);
    printf("MAXTIME: %lf\n",    trials[num_iterations - 1]);
    printf("AVGTIME: %lf\n",    avg(trials, num_iterations));
}

int main(int argc, char** argv)
{
    // parameters to parse:
    //
    //   num iterations: How many times to repeat a benchmark. Default: 10.
    //   tree size: An integer passes to `build_tree()`. Default: 10.
    //   buffer size: Default 10M.

    int num_iterations = 10;
    int tree_size = 10;
    int buffer_size = 10 * 1000 * 1000; // 10M

    // TODO: atoi() error checking

    for (int i = 1; i < argc; ++i)
    {
        if (strcmp(argv[i], "-num-iterations") == 0 && i < argc - 1)
        {
            num_iterations = atoi(argv[i + 1]);
            ++i;
        }
        else if (strcmp(argv[i], "-tree-size") == 0 && i < argc - 1)
        {
            tree_size = atoi(argv[i + 1]);
            ++i;
        }
        else if (strcmp(argv[i], "-buffer-size") == 0 && i < argc - 1)
        {
            buffer_size = atoi(argv[i + 1]);
            ++i;
        }
        else
        {
            fprintf(stderr, "Can't parse argument: \"%s\"\n", argv[i]);
            show_usage();
            exit(1);
        }
    }

    run(num_iterations, tree_size, buffer_size);

    return 0;
}
|]

mkProgram
  :: [FunDecl]
       -- ^ function declarations to put in the compilation unit
  -> String
       -- ^ name of the function to benchmark
  -> String
mkProgram fs fname =
    fst $ runSyM 0 $
        do fs' <- mapM codegenFun fs
           return $ concat
                      [ includes
                      , pretty 80 (stack (map ppr fs'))
                      , pretty 80 (ppr (mkRuntimeFuns fname))
                      ]



writeProgram
  :: [FunDecl]
       -- ^ function declarations to put in the compilation unit
  -> String
       -- ^ name of the function to benchmark
  -> FilePath
       -- ^ path of the file to generate
  -> IO ()
writeProgram fs fname path = writeFile path (mkProgram fs fname)

-- Examples:
--------------------------------------------------------------------------------

leafTag :: Word8
leafTag = 0

nodeTag :: Word8
nodeTag = 1

exadd1Prog :: Prog
exadd1Prog = Prog [exadd1] Nothing

exadd1 :: FunDecl
exadd1 = FunDecl "add1" [("t",CursorTy),("tout",CursorTy)] (ProdTy [CursorTy,CursorTy]) exadd1Tail

buildTree :: FunDecl
buildTree = FunDecl "build_tree" [("n",IntTy),("tout",CursorTy)] CursorTy buildTree_tail

buildTree_tail :: Tail
buildTree_tail =
    Switch (VarTriv "n") (IntAlts [(0, base_case)]) (Just recursive_case)
  where
    base_case, recursive_case :: Tail

    base_case =
      LetPrimCallT [("tout1", CursorTy)] WriteInt [IntTriv 0, VarTriv "tout"] $
      RetValsT [VarTriv "tout1"]

    recursive_case =
      LetPrimCallT [("n1",IntTy)] SubP [VarTriv "n", IntTriv 1] $
      LetCallT [("tout1",CursorTy)] "build_tree" [VarTriv "n1", VarTriv "tout"] $
      LetCallT [("tout2",CursorTy)] "build_tree" [VarTriv "n1", VarTriv "tout1"] $
      RetValsT [VarTriv "tout2"]

exadd1Tail :: Tail
exadd1Tail =
    LetPrimCallT [("ttag",TagTy),("t2",CursorTy)] ReadTag [VarTriv "t"]
  $ Switch (VarTriv "ttag")
           (TagAlts [(leafTag,leafCase),
                     (nodeTag,nodeCase)])
           Nothing
    where leafCase =
              LetPrimCallT [("tout2",CursorTy)] WriteTag [TagTriv leafTag, VarTriv "tout"]
            $ LetPrimCallT [("n",IntTy),("t3",CursorTy)] ReadInt [VarTriv "t2"]
            $ LetPrimCallT [("n1",IntTy)] AddP [VarTriv "n", IntTriv 1]
            $ LetPrimCallT [("tout3",CursorTy)] WriteInt [VarTriv "n1", VarTriv "tout2"]
            $ RetValsT [VarTriv "t3", VarTriv "tout3"]
          nodeCase =
              LetPrimCallT [("tout2",CursorTy)] WriteTag [TagTriv nodeTag, VarTriv "tout"]
            $ LetCallT [("t3",CursorTy),("tout3",CursorTy)] "add1" [VarTriv "t2", VarTriv "tout2"]
            $ TailCall "add1" [VarTriv "t3", VarTriv "tout3"]

-- | Compile the example and print it
-- add1C :: IO ()
add1C = codegenProg exadd1Prog -- mapM_ (\c -> putDocLn $ ppr c) (codegenFun exadd1)

buildTreeC :: IO ()
buildTreeC = unfinished 6 --  mapM_ (\c -> putDocLn (ppr c)) (codegenFun buildTree)

unfinished :: Int -> a
unfinished n = error $ "Target.hs: unfinished hole #"++show n
