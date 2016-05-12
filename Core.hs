{-# LANGUAGE DeriveDataTypeable #-}

-- | 

module Core where

import Data.Typeable (Typeable)
import Data.Data (Data)

-- TODO: Not sucked in yet:
--------------------------------------------------------------------------------

data Id
data Var

data Bind b
data Coercion
data Tickish a
-- Data
-- Typeable
data AltCon

data ByteString
data FastString

data FunctionOrData

data TyCon
data KindOrType
data TyLit
data KindCoercion
data TyBinder

--------------------------------------------------------------------------------


data Expr b
  = Var   Id
  | Lit   Literal
  | App   (Expr b) (Arg b)
  | Lam   b (Expr b)
  | Let   (Bind b) (Expr b)
  | Case  (Expr b) b Type [Alt b]       -- See #case_invariant#
  | Cast  (Expr b) Coercion
  | Tick  (Tickish Id) (Expr b)
  | Type  Type
  | Coercion Coercion
--  deriving (Data, Typeable)

-- | Type synonym for expressions that occur in function argument positions.
-- Only 'Arg' should contain a 'Type' at top level, general 'Expr' should not
type Arg b = Expr b

-- | A case split alternative. Consists of the constructor leading to the alternative,
-- the variables bound from the constructor, and the expression to be executed given that binding.
-- The default alternative is @(DEFAULT, [], rhs)@

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.hs
type Alt b = (AltCon, [b], Expr b)


data Literal
  =     ------------------
        -- First the primitive guys
    MachChar    Char            -- ^ @Char#@ - at least 31 bits. Create with 'mkMachChar'

  | MachStr     ByteString      -- ^ A string-literal: stored and emitted
                                -- UTF-8 encoded, we'll arrange to decode it
                                -- at runtime.  Also emitted with a @'\0'@
                                -- terminator. Create with 'mkMachString'

  | MachNullAddr                -- ^ The @NULL@ pointer, the only pointer value
                                -- that can be represented as a Literal. Create
                                -- with 'nullAddrLit'

  | MachInt     Integer         -- ^ @Int#@ - at least @WORD_SIZE_IN_BITS@ bits. Create with 'mkMachInt'
  | MachInt64   Integer         -- ^ @Int64#@ - at least 64 bits. Create with 'mkMachInt64'
  | MachWord    Integer         -- ^ @Word#@ - at least @WORD_SIZE_IN_BITS@ bits. Create with 'mkMachWord'
  | MachWord64  Integer         -- ^ @Word64#@ - at least 64 bits. Create with 'mkMachWord64'

  | MachFloat   Rational        -- ^ @Float#@. Create with 'mkMachFloat'
  | MachDouble  Rational        -- ^ @Double#@. Create with 'mkMachDouble'

  | MachLabel   FastString
                (Maybe Int)
        FunctionOrData
                -- ^ A label literal. Parameters:
                --
                -- 1) The name of the symbol mentioned in the declaration
                --
                -- 2) The size (in bytes) of the arguments
                --    the label expects. Only applicable with
                --    @stdcall@ labels. @Just x@ => @\<x\>@ will
                --    be appended to label name when emitting assembly.

  | LitInteger Integer Type --  ^ Integer literals


data Type
  -- See Note [Non-trivial definitional equality]
  = TyVarTy Var -- ^ Vanilla type or kind variable (*never* a coercion variable)

  | AppTy         -- See Note [AppTy rep]
        Type
        Type            -- ^ Type application to something other than a 'TyCon'. Parameters:
                        --
                        --  1) Function: must /not/ be a 'TyConApp',
                        --     must be another 'AppTy', or 'TyVarTy'
                        --
                        --  2) Argument type

  | TyConApp      -- See Note [AppTy rep]
        TyCon
        [KindOrType]    -- ^ Application of a 'TyCon', including newtypes /and/ synonyms.
                        -- Invariant: saturated applications of 'FunTyCon' must
                        -- use 'FunTy' and saturated synonyms must use their own
                        -- constructors. However, /unsaturated/ 'FunTyCon's
                        -- do appear as 'TyConApp's.
                        -- Parameters:
                        --
                        -- 1) Type constructor being applied to.
                        --
                        -- 2) Type arguments. Might not have enough type arguments
                        --    here to saturate the constructor.
                        --    Even type synonyms are not necessarily saturated;
                        --    for example unsaturated type synonyms
                        --    can appear as the right hand side of a type synonym.

  | ForAllTy
        TyBinder
        Type            -- ^ A Π type.
                        -- This includes arrow types, constructed with
                        -- @ForAllTy (Anon ...)@. See also Note [TyBinder].

  | LitTy TyLit     -- ^ Type literals are similar to type constructors.

  | CastTy
        Type
        KindCoercion  -- ^ A kind cast. The coercion is always nominal.
                      -- INVARIANT: The cast is never refl.
                      -- INVARIANT: The cast is "pushed down" as far as it
                      -- can go. See Note [Pushing down casts]

  | CoercionTy
        Coercion    -- ^ Injection of a Coercion into a type
                    -- This should only ever be used in the RHS of an AppTy,
                    -- in the list of a TyConApp, when applying a promoted
                    -- GADT data constructor
--  deriving (Data, Typeable)
