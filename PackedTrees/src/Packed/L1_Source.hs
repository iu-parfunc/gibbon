{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wall #-}

-- | 

module Packed.L1_Source where

import Packed.Common
import Data.Map as M
import Data.List as L
----------------------------------------
        
-- | The source language.  It has pointer based sums and products, as
-- well as packed algebraic datatypes.
data L1 = Varref Var | Lit Int
        | App L1 L1
        | Lam (Var,T1) L1
        | CaseEither L1 L1 L1
        | CasePacked L1 (M.Map Constr ([Var], L1))
        | Add L1 L1 -- One primitive.          
        | Letrec (Var,T1,L1) L1
          -- ^ One binding at a time, but could bind a tuple for
          -- mutual recursion.
        | InL L1 | InR L1 | MkProd L1 L1
        | MkPacked Constr [L1]
  deriving (Read,Show,Eq,Ord)
  
-- | Types include both (boxed, indirect) sums and products as well as
-- unboxed/unpacked ones.
data T1 = TInt | TArr T1 T1 | TyVar Var
        | Prod T1 T1 | Sum T1 T1
        | Packed Constr [T1]
  deriving (Read,Show,Eq,Ord)
  
-- | Complete programs include datatype definitions:
data P1 = P1 { defs :: [DDef]
             , mainProg :: L1
             , mainTy   :: T1 }
  deriving (Read,Show,Eq,Ord)
  
data DDef = DDef Var [(Constr,[T1])]
  deriving (Read,Show,Eq,Ord)
  
--------------------------------------------------------------------------------

-- | Promote a value to a term that evaluates to it.
l1FromValue :: Value L1 -> L1
l1FromValue x =
  case x of
    VLeft x -> InL $ l1FromValue x
    (VInt y) -> undefined
    (VLam y1 y2 y3) -> undefined
    (VProd y1 y2) -> undefined
    (VLeft y) -> undefined
    (VRight y) -> undefined
    (VPacked y1 y2) -> undefined

-- | To keep things simple we evaluate directly to a string.
interp :: [DDef] -> L1 -> Value L1
interp _ddefs = go M.empty
    where
      go env x = 
          case x of
            Lit c    -> VInt c
            Varref v -> env ! v
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
            (MkProd a b) -> VProd (go env a) (go env b)
            -- TODO: Should check this against the ddefs.
            (MkPacked k ls) -> VPacked k $ L.map (go env) ls

interpProg :: P1 -> Value L1
interpProg P1 {defs,mainProg,mainTy} = interp defs mainProg

--------------------------------------------------------------------------------

p1 :: P1
p1 = P1 [] (Letrec ("x",TInt,Lit 3) (Varref "x")) TInt

main :: IO ()
main = print (interpProg p1)
