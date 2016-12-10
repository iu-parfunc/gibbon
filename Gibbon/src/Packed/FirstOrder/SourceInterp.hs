{-# LANGUAGE DeriveGeneric #-}

-- | Interpreter for the source language (L1) 
--
-- UNFINISHED / PLACEHOLDER

module Packed.FirstOrder.SourceInterp where

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
-- FINISH:       | VDict
-- FINISH:       | VList
           | VProd [Value]
           | VPacked Constr [Value]
  deriving (Read,Show,Eq,Ord,Generic)

type ValEnv a = Map Var Value

------------------------------------------------------------
    
-- | Promote a value to a term that evaluates to it.
l1FromValue :: Value -> Exp
l1FromValue x =
  case x of
    (VInt y) -> __
    (VProd ls) -> __
    (VPacked y1 y2) -> __


execAndPrint :: RunConfig -> Prog -> IO ()
execAndPrint rc prg =
  case interpProg rc prg of
   VInt n -> print n
                                                    
interpProg :: RunConfig -> Prog -> Value
interpProg rc Prog {ddefs,mainExp=Just e} = interp e

 where
  applyPrim :: Prim -> [Value] -> Value
  applyPrim p ls =
   case (p,ls) of
     (MkTrue,[])  -> VBool True
     (MkFalse,[]) -> VBool False
     (AddP,[VInt x, VInt y]) -> VInt (x+y)
     (SubP,[VInt x, VInt y]) -> VInt (x-y)
     (MulP,[VInt x, VInt y]) -> VInt (x*y)
     (EqSymP,[VInt x, VInt y]) -> VBool (x==y)
     (EqIntP,[VInt x, VInt y]) -> VBool (x==y)
     ((DictInsertP x1),x2) -> __finishDict
     ((DictLookupP x1),x2) -> __finishDict
     ((DictEmptyP x1),x2) -> __finishDict
     ((ErrorP msg _ty),[]) -> error msg
     (SizeParam,x2) -> VInt (rcSize rc)
     oth -> error $ "unhandled prim or wrong number of arguments: "++show oth

  interp :: Exp -> Value
  interp = go M.empty
    where
      go env x =
          case x of
            LitE c    -> VInt c
            VarE v -> env ! v

            PrimAppE p ls  ->
                let val = applyPrim p (L.map (go env) ls)
                in val
--                error $ "unhandled: "++show x
            ProjE i e -> __proj

            AppE v b ->               
              let rand = go env b
                  bod  = __ -- funenv # v
              in go (M.insert v rand env) bod
            (CaseE x1 ls1) -> case go env x1 of
                               VPacked k ls2 ->
                                   let (_,vs,rhs) = lookup3 k ls1
                                       env' = M.union (M.fromList (zip vs ls2))
                                                env
                                   in go env' rhs
                               _ -> error "L1 interp: type error"
            (LetE (v,_ty,rhs) bod) ->
              let env' = M.insert v rhs' env
                  rhs' = go env' rhs
              in go env' bod

            (MkProdE ls) -> VProd (L.map (go env) ls) 
            -- TODO: Should check this against the ddefs.
            (MkPackedE k ls) -> VPacked k $ L.map (go env) ls

            -- (Add a b) -> case (go env a, go env b) of
            --                (VInt c, VInt d) -> VInt $ c+d
            --                _ -> error "L1 interp: type error"

            -- TimeIt e t b -> TimeIt (go e) t b
                                
            IfE a b c -> case go env a of
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
main = print (interpProg (RunConfig 1 1 dbgLvl) p1)



       
