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

-- | It's a first order language with simple values.
data Value a = VInt Int
             | VBool Bool
-- FINISH:       | VDict
-- FINISH:       | VList
             | VProd [Value a]
             | VPacked Constr [Value a]
  deriving (Read,Show,Eq,Ord,Generic)

type ValEnv a = Map Var (Value a)

------------------------------------------------------------
    
-- | Promote a value to a term that evaluates to it.
l1FromValue :: Value Exp -> Exp
l1FromValue x =
  case x of
    (VInt y) -> __
    (VProd ls) -> __
    (VPacked y1 y2) -> __


execAndPrint :: Prog -> IO ()
execAndPrint prg =
  case interpProg prg of
   VInt n -> print n
                                                    
interpProg :: Prog -> Value Exp
interpProg Prog {ddefs,mainExp=Just e} = interp e

 where
  interp :: Exp -> Value Exp
  interp = go M.empty
    where
      go env x =
          case x of
            LitE c    -> VInt c
            VarE v -> env ! v
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
main = print (interpProg p1)
