{-# LANGUAGE DeriveGeneric #-}

-- | Interpreter for the source language (L1) 
--
-- UNFINISHED / PLACEHOLDER

module Packed.FirstOrder.SourceInterp where

import Data.Map
import Packed.FirstOrder.L1_Source
import Packed.FirstOrder.Common
import GHC.Generics    

data Value a = VInt Int
--             | VLam (ValEnv a) Var a
             | VProd [Value a]
--             | VLeft (Value a)
--             | VRight (Value a)
             | VPacked Constr [Value a]
  deriving (Read,Show,Eq,Ord,Generic)

type ValEnv a = Map Var (Value a)

------------------------------------------------------------
    
-- | Promote a value to a term that evaluates to it.
l1FromValue :: Value Exp -> Exp
l1FromValue x =
  case x of
--    VLeft x -> InL $ l1FromValue x
    (VInt y) -> __
--    (VLam y1 y2 y3) -> __
    (VProd ls) -> __
--    (VLeft y) -> __
--    (VRight y) -> __
    (VPacked y1 y2) -> __

{-
                       
-- | To keep things simple we evaluate directly to a string.
interp :: DDefs Ty -> Exp -> Value Exp
interp _ddefs = go M.empty
    where
      go env x =
          case x of
            Lit c    -> VInt c
            VarE v -> env ! v
            App a b ->
              let rand = go env b
                  (VLam env' v bod) = go env a
              in go (M.insert v rand env') bod
            Lam (v,_ty) bod -> VLam env v bod
            CaseEither a b c -> case go env a of
                                  VLeft  v -> go env $ App b $ l1FromValue v
                                  VRight v -> go env $ App c $ l1FromValue v
                                  _ -> error "L1 interp: type error"
            (CasePacked x1 mp) -> case go env x1 of
                                    VPacked k ls ->
                                      let Just (vs,rhs) = M.lookup k mp
                                          env' = M.union (M.fromList (zip vs ls))
                                                         env
                                      in go env' rhs
                                    _ -> error "L1 interp: type error"
            (Add a b) -> case (go env a, go env b) of
                           (VInt c, VInt d) -> VInt $ c+d
                           _ -> error "L1 interp: type error"
            (Letrec (v,ty,rhs) bod) ->
              let env' = M.insert v rhs' env
                  rhs' = go env' rhs
              in go env' bod

            (InL x) -> VLeft  $ go env x
            (InR x) -> VRight $ go env x
            (MkProdE a b) -> VProd (go env a) (go env b)
            -- TODO: Should check this against the ddefs.
            (MkPacked k ls) -> VPacked k $ L.map (go env) ls

interpProg :: Prog -> Value Exp
interpProg Prog {defs,mainProg} = interp defs mainProg

tyc :: TEnv -> Exp -> Ty
tyc = __

--------------------------------------------------------------------------------

p1 :: Prog
p1 = Prog emptyDD (Letrec ("x",TInt,Lit 3) (Varref "x")) TInt

main :: IO ()
main = print (interpProg p1)
-}
