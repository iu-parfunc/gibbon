{-# LANGUAGE RecordWildCards #-}

-- | A compiler pass that translates between the source and target
-- language, implementing the cursor-passing implementation of Packed
-- tree ADTs.

module Packed.Translate where

import           Packed.L1_Source       as S
import qualified Packed.L2_Intermediate as T

insertCursors :: P1 -> T.P2
insertCursors P1{..} = undefined (go mainProg)
 where
   go :: L1 -> T.L2
   go (Varref x) = undefined
   go (Lit x) = undefined
   go (App x1 x2) = undefined
   go (Lam x1 x2) = undefined
   go (CaseEither x1 x2 x3) = undefined
   go (CasePacked x1 x2) = undefined
   go (Add x1 x2) = undefined
   go (Letrec x1 x2) = undefined
   go (InL x) = undefined
   go (InR x) = undefined
   go (MkProd x1 x2) = undefined
   go (MkPacked x1 x2) = undefined
