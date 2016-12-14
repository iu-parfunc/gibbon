{-# LANGUAGE DeriveGeneric #-}

-- | Interpreter for the source language (L1) 
--
-- UNFINISHED / PLACEHOLDER

module Packed.FirstOrder.SourceInterp (execAndPrint, main) where

import Data.List as L 
import Data.Map as M
import Packed.FirstOrder.L1_Source
import Packed.FirstOrder.Common
import GHC.Generics    
import Text.PrettyPrint.GenericPretty

-- TODO:
-- It's a SUPERSET, but use the Value type from TargetInterp anyway:
-- Actually, we should merge these into one type with a simple extension story.
-- import Packed.FirstOrder.TargetInterp (Val(..), applyPrim)

------------------------------------------------------------
    
-- | It's a first order language with simple values.
data Value = VInt Int
           | VBool Bool
           | VDict (M.Map Value Value)
-- FINISH:       | VList
           | VProd [Value]
           | VPacked Constr [Value]
  deriving (Read,Eq,Ord,Generic)

instance Out Value
           
instance Show Value where                      
 show v =
  case v of
   VInt n   -> show n
   VBool b  -> if b then "true" else "false"
   VProd ls -> "("++ concat(intersperse ", " (L.map show ls)) ++")"
   VPacked k ls -> k ++ show (VProd ls)
   VDict m      -> show (M.toList m)
                   
-- type ValEnv a = Map Var Value

------------------------------------------------------------
{-    
-- | Promote a value to a term that evaluates to it.
l1FromValue :: Value -> Exp
l1FromValue x =
  case x of
    (VInt y) -> __
    (VProd ls) -> __
    (VPacked y1 y2) -> __
-}

execAndPrint :: RunConfig -> Prog -> IO ()
execAndPrint rc prg = print =<< interpProg rc prg
   
                                                  
interpProg :: RunConfig -> Prog -> IO Value
interpProg _ Prog {mainExp=Nothing} =
    error "SourceInterp: cannot interpret program with no main expression"
interpProg rc Prog {ddefs,mainExp=Just e} = interp e

 where
  applyPrim :: Prim -> [Value] -> Value
  applyPrim p ls =
   case (p,ls) of
     (MkTrue,[])             -> VBool True
     (MkFalse,[])            -> VBool False
     (AddP,[VInt x, VInt y]) -> VInt (x+y)
     (SubP,[VInt x, VInt y]) -> VInt (x-y)
     (MulP,[VInt x, VInt y]) -> VInt (x*y)
     (EqSymP,[VInt x, VInt y]) -> VBool (x==y)
     (EqIntP,[VInt x, VInt y]) -> VBool (x==y)
     ((DictInsertP _ty),[VDict mp, key, val]) -> VDict (M.insert key val mp)
     ((DictLookupP _),[VDict mp, key])        -> mp # key
     ((DictEmptyP _),[])                      -> VDict M.empty
     ((ErrorP msg _ty),[]) -> error msg
     (SizeParam,[]) -> VInt (rcSize rc)
     oth -> error $ "unhandled prim or wrong number of arguments: "++show oth

  interp :: Exp -> IO Value
  interp = go M.empty
    where
      go env x =
          case x of
            LitE c         -> return $ VInt c
            VarE v         -> return $ env ! v
            PrimAppE p ls  -> do args <- mapM (go env) ls
                                 return $ applyPrim p args
            ProjE ix e -> do VProd ls <- go env e
                             return $ ls !! ix

            AppE v b -> do rand <- go env b
                           let bod  = __ -- funenv # v
                           go (M.insert v rand env) bod

            (CaseE x1 ls1) -> do
                   v <- go env x1
                   case v of
                     VPacked k ls2 ->
                         let (_,vs,rhs) = lookup3 k ls1
                             env' = M.union (M.fromList (zip vs ls2))
                                    env
                         in go env' rhs
                     _ -> error "L1 interp: type error"

            (LetE (v,_ty,rhs) bod) -> do
              rhs' <- go env rhs
              let env' = M.insert v rhs' env
              go env' bod

            (MkProdE ls) -> VProd <$> mapM (go env) ls
            -- TODO: Should check this against the ddefs.
            (MkPackedE k ls) -> VPacked k <$> mapM (go env) ls

            -- (Add a b) -> case (go env a, go env b) of
            --                (VInt c, VInt d) -> VInt $ c+d
            --                _ -> error "L1 interp: type error"

            TimeIt bod _ isIter ->
              go env bod -- FINISHME
              -- if isIter then
              --   __ --rcIters 
              --  else
              --   __ -- (go env bod) 
                                
            IfE a b c -> do v <- go env a
                            case v of
                             VBool flg -> if flg
                                          then go env b
                                          else go env c
                             oth -> error$ "interp: expected bool, got: "++show oth

--            MapE (v,t,rhs) bod -> MapE (v,t, rhs) (go bod)
--            FoldE (v1,t1,r1) (v2,t2,r2) bod -> __

                              

-- Misc Helpers
--------------------------------------------------------------------------------

lookup3 :: (Eq k, Show k, Show a, Show b) =>
           k -> [(k,a,b)] -> (k,a,b)
lookup3 k ls = go ls
  where
   go [] = error$ "lookup3: key "++show k++" not found in list:\n  "++take 80 (show ls)
   go ((k1,a1,b1):r)
      | k1 == k   = (k1,a1,b1)
      | otherwise = go r
                    
--------------------------------------------------------------------------------

p1 :: Prog
p1 = Prog emptyDD  M.empty
          (Just (LetE ("x", IntTy, LitE 3) (VarE "x")))
         -- IntTy

main :: IO ()
main = print =<< interpProg (RunConfig 1 1 dbgLvl) p1



       
