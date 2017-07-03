{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

--- Sketching out a new lower-level language with explicit tags and cursors.
--- Not sure if this will be necessary.
--- Currently in the existing compiler machinery this is a bit messy.
---
--- Language uses monadic notation to express effectful statements.
--- WriteTag, etc, are actions. Could use this to write a type checker
--- by tracking type state in monad.

module Packed.FirstOrder.CurExp where

import Packed.FirstOrder.Common
import Packed.FirstOrder.LocExp
import Data.Map as M
import Data.Set as S
import Data.List as L
import GHC.Generics
import Text.PrettyPrint.GenericPretty
import Control.DeepSeq (NFData)


type Cur = Var

data CurProgram = CurProgram (Map Var ProcDef) (Maybe Stm)
                  deriving (Read,Show,Eq,Ord, Generic, NFData)

data ProcDef = ProcDef Var CType CType [Var] Stm
               deriving (Read,Show,Eq,Ord, Generic, NFData)

data Stm = StmBind StmAction [Var] Stm
         | StmReturn Var
           deriving (Read,Show,Eq,Ord, Generic, NFData)

data StmAction = PrimAction Lexp
               | WriteTag DataCon Cur
               | ReadTag Cur
               | WriteData Ptype Cur
               | ReadData Ptype Cur
               | Increment Int Cur
               | AllocReg
               | DeleteReg Var
               | FunCall Var [Var]
                 deriving (Read,Show,Eq,Ord, Generic, NFData)
