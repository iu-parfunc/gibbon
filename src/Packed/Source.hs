-- | 

module Packed.Source where

import Packed.Common
import Data.Map as M
    
----------------------------------------
        
-- | The source language.  It has pointer based sums and products, as
-- well as packed algebraic datatypes.
data L1 = Varref Var
        | App L1 L1
        | Lam (Var,T1) L1
        | CaseEither L1 L1 L1
        | CasePacked L1 [([Var], L1)]
        | Add L1 L1 -- One primitive.
        | Letrec [(Var,T1,L1)] L1
        | InL L1 | InR L1 | MkProd L1 L1
        | MkPacked Constr [L1]
          
data T1 = TInt | TArr T1 T1 | TyVar Var
        | Prod T1 T1 | Sum T1 T1
        | Packed Constr [T1]

-- | Complete programs include datatype definitions:
data P1 = P1 { defs :: [(Var,[(Constr,[T1])])]
             , mainProg :: (L1,T1) }
        
--------------------------------------------------------------------------------

-- | To keep things simple we evaluate directly to a string.
interp :: L1 -> Value L1
interp = go M.empty
    where
      go env x = 
          case x of
            Varref v -> env ! v
            App a b ->
              let rand = go env b 
                  (VLam env' v bod) = go env a 
              in go (M.insert v rand env') bod
