{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- |  This is really a hack for benchmarking.  Lift global allocations outside of timing loop.s

-- WARNING: seeded with DUPLICATED code from Unariser

module Packed.FirstOrder.Passes.HoistNewBuf
    (hoistNewBuf) where

-- import qualified Data.Map as M    
import qualified Data.List as L
import Packed.FirstOrder.Common 
import qualified Packed.FirstOrder.L1_Source as L1
import Packed.FirstOrder.L2_Traverse as L2
import Packed.FirstOrder.Passes.Cursorize as C 
import Prelude hiding (exp)

-- | Strip out the NewBuf bindings up until the point control flow diverges.
hoistNewBuf :: L2.Prog -> SyM L2.Prog
hoistNewBuf = mapMExprs hoistExp 
         
hoistExp :: ignored -> L1.Exp -> SyM L1.Exp
hoistExp _ ex0 = return $ gocap ex0
  where

  gocap ex = let (lets,ex') = go ex in
             mklets lets ex'

  go :: L1.Exp -> ([(Var,L1.Ty,Exp)], L1.Exp)
  go e0 =
   case e0 of
    (LitE _)   -> ([], e0)
    (VarE _)   -> ([], e0)
    (AppE _ _) -> ([], e0)
    (PrimAppE{}) -> ([], e0)

    -- Here's where we lift outside timings!!                        
    (TimeIt e t b) -> let (lts,e') = go e in
                      (lts, TimeIt e' t b)
     
    (LetE (v,t, NewBuffer) bod) ->
        let (lts1,bod') = go bod in
        ((v,t, NewBuffer):lts1, bod')

    -- boilerplate
    (LetE (v,t,rhs) bod) ->
        let (lts1, rhs') = go rhs
            (lts2, bod') = go bod
        in (lts1++lts2, LetE (v,t,rhs') bod')

    (IfE e1 e2 e3) ->
         let (lts1, e1') = go e1 in 
         (lts1, IfE e1' (gocap e2) (gocap e3))

    (ProjE i e)  -> let (lts,e') = go e in
                    (lts, ProjE i e')
    (MkProdE es) -> let (ltss,es') = unzip $ L.map go es in
                    (concat ltss, MkProdE es')

    (CaseE e ls) -> let (lts,e') = go e 
                    in (lts, CaseE e' [ (k,vs, gocap e)
                                      | (k,vs,e) <- ls ])
    (MkPackedE c es) -> let (ltss,es') = unzip $ L.map go es in
                        (concat ltss, MkPackedE c es')

    -- (MapE (v,t,e') e) -> 
    -- (FoldE (v1,t1,e1) (v2,t2,e2) e3) ->


mklets :: [(Var, L1.Ty, Exp)] -> Exp -> Exp
mklets [] bod = bod
mklets (bnd:rst) bod = LetE bnd $ mklets rst bod
