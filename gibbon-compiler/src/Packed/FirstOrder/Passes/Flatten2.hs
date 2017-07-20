{-# LANGUAGE FlexibleInstances #-}

-- | Flatten ported to L2.
module Packed.FirstOrder.Passes.Flatten2 (flatten2) where

-- import Control.Monad.State
import Packed.FirstOrder.Common (SyM, Env2(Env2,vEnv,fEnv), LocVar, Var)
import Packed.FirstOrder.L1.Syntax () -- Instances
import Packed.FirstOrder.L2.Syntax as L2
import Text.PrettyPrint.GenericPretty (Out)
import Packed.FirstOrder.GenericOps (Flattenable, gFlattenExp)
import Packed.FirstOrder.Passes.Flatten (TEnv)
-- import qualified Data.Map as M

import Prelude hiding (exp)

-- | Just like Flatten.hs, but on L2.
flatten2 :: L2.Prog -> SyM L2.Prog
flatten2 prg@(L2.Prog ddefs _ _) = L2.mapMExprs fn prg
  where
   fn :: Env2 (UrTy LocVar) -> L2.Exp2 -> SyM L2.Exp2
   fn env2 ex = gFlattenExp ddefs env2 ex

type Binds l e = (Var,[l],UrTy l, PreExp l e (UrTy l))

-- instance Flattenable Exp2 where

-- instance (Show l, Show d, Out l, Out d) =>
--          Flattenable (PreExp l (E2Ext l d) d) where
--  gFlattenExp = _             
--   where
--    exp :: TEnv l -> PreExp l e (UrTy l) -> SyM ([Binds l e],PreExp l e (UrTy l))
--    exp tenv ex =
--      case ex of
--        Ext ext -> case ext of
--                    LetRegionE r bod -> _       
--        oth -> gFlattenExp 

instance (Show l, Show d, Out l, Out d) =>
         Flattenable (E2Ext l d) where
 gFlattenExp = _             
  where
   exp :: TEnv l -> E2Ext l d -> SyM ([Binds l e],E2Ext l d)
   exp tenv ex =
     case ex of
       LetRegionE r bod -> _       


