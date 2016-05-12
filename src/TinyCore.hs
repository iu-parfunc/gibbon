{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

-- | A baby, baby version of Core.

module TinyCore where

import Data.Typeable (Typeable)
import Data.Data (Data)

--------------------------------------------------------------------------------

-- This could be an index into a symtable:
newtype Id = Id Int

type Literal = Int

data Expr b
  = Var   Id
  | Lit   Literal
  | App   (Expr b) (Arg b)
  | Lam   b (Expr b)
  | Let   (Bind b) (Expr b)
  | Case  (Expr b) b Type [Alt b] 
--  | Cast  (Expr b) Coercion
--  | Tick  (Tickish Id) (Expr b)
--  | Type  Type
--  | Coercion Coercion
--  deriving (Data, Typeable)

-- | Type synonym for expressions that occur in function argument positions.
-- Only 'Arg' should contain a 'Type' at top level, general 'Expr' should not
type Arg b = Expr b

-- | Binding, used for top level bindings in a module and local bindings in a @let@.

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.hs
data Bind b = NonRec b (Expr b)
            | Rec [(b, (Expr b))]
--  deriving (Data, Typeable)


-- | A case split alternative. Consists of the constructor leading to the alternative,
-- the variables bound from the constructor, and the expression to be executed given that binding.
-- The default alternative is @(DEFAULT, [], rhs)@

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.hs
type Alt b = (AltCon, [b], Expr b)

-- | A case alternative constructor (i.e. pattern match)

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.hs
data AltCon
--  = DataAlt DataCon   --  ^ A plain data constructor: @case e of { Foo x -> ... }@.
                      -- Invariant: the 'DataCon' is always from a @data@ type, and never from a @newtype@

  = LitAlt  Literal   -- ^ A literal: @case e of { 1 -> ... }@
                      -- Invariant: always an *unlifted* literal
                      -- See Note [Literal alternatives]

  | DEFAULT           -- ^ Trivial alternative: @case e of { _ -> ... }@
--   deriving (Eq, Ord, Data, Typeable)


data Type
  = TInt 
  | TArrow Type Type
