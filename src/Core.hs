{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

-- | Stand-alone Core datatype definitions.

module Core where

import Data.Typeable (Typeable)
import Data.Data (Data)

import Data.Aeson.TH
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as B

-- TEMP/TESTING -- external representations:
--------------------------------------------------------------------------------

data E 
  = V  String
  | L  Int
  | Ap E E
 deriving (Show,Read)

-- $(deriveJSON defaultOptions{constructorTagModifier = map toLower} ''E)
$(deriveJSON defaultOptions ''E)

x = Ap (V "x") (L 3)

y = B.putStrLn $ encode x
-- Object (fromList [("tag",String "Ap"),("contents",Array [Object (fromList [("tag",String "V"),("contents",String "x")]),Object (fromList [("tag",String "L"),("contents",Number 3.0)])])])

-- TODO: Not sucked in yet:
--------------------------------------------------------------------------------

data Id
data Var

data Coercion
data Tickish a

data ByteString
data FastString

data FunctionOrData

data TyCon
data KindOrType
data TyLit
data KindCoercion
data TyBinder

data Name
data Unique
data ConTag
data TyVar
data EqSpec
data ThetaType
data HsSrcBang
data FieldLabel
data DataConRep
data Arity

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
  = DataAlt DataCon   --  ^ A plain data constructor: @case e of { Foo x -> ... }@.
                      -- Invariant: the 'DataCon' is always from a @data@ type, and never from a @newtype@

  | LitAlt  Literal   -- ^ A literal: @case e of { 1 -> ... }@
                      -- Invariant: always an *unlifted* literal
                      -- See Note [Literal alternatives]

  | DEFAULT           -- ^ Trivial alternative: @case e of { _ -> ... }@
--   deriving (Eq, Ord, Data, Typeable)



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


data DataCon
  = MkData {
        dcName    :: Name,      -- This is the name of the *source data con*
                                -- (see "Note [Data Constructor Naming]" above)
        dcUnique :: Unique,     -- Cached from Name
        dcTag    :: ConTag,     -- ^ Tag, used for ordering 'DataCon's

        -- Running example:
        --
        --      *** As declared by the user
        --  data T a where
        --    MkT :: forall x y. (x~y,Ord x) => x -> y -> T (x,y)

        --      *** As represented internally
        --  data T a where
        --    MkT :: forall a. forall x y. (a~(x,y),x~y,Ord x) => x -> y -> T a
        --
        -- The next six fields express the type of the constructor, in pieces
        -- e.g.
        --
        --      dcUnivTyVars  = [a]
        --      dcExTyVars    = [x,y]
        --      dcEqSpec      = [a~(x,y)]
        --      dcOtherTheta  = [x~y, Ord x]
        --      dcOrigArgTys  = [x,y]
        --      dcRepTyCon       = T

        -- In general, the dcUnivTyVars are NOT NECESSARILY THE SAME AS THE TYVARS
        -- FOR THE PARENT TyCon. (This is a change (Oct05): previously, vanilla
        -- datacons guaranteed to have the same type variables as their parent TyCon,
        -- but that seems ugly.)

        dcVanilla :: Bool,      -- True <=> This is a vanilla Haskell 98 data constructor
                                --          Its type is of form
                                --              forall a1..an . t1 -> ... tm -> T a1..an
                                --          No existentials, no coercions, nothing.
                                -- That is: dcExTyVars = dcEqSpec = dcOtherTheta = []
                -- NB 1: newtypes always have a vanilla data con
                -- NB 2: a vanilla constructor can still be declared in GADT-style
                --       syntax, provided its type looks like the above.
                --       The declaration format is held in the TyCon (algTcGadtSyntax)

        -- Universally-quantified type vars [a,b,c]
        -- INVARIANT: length matches arity of the dcRepTyCon
        -- INVARIANT: result type of data con worker is exactly (T a b c)
        dcUnivTyVars    :: [TyVar],     -- Two linked fields
        dcUnivTyBinders :: [TyBinder],  -- see Note [TyBinders in DataCons]


        -- Existentially-quantified type vars [x,y]
        dcExTyVars     :: [TyVar],     -- Two linked fields
        dcExTyBinders  :: [TyBinder],  -- see Note [TyBinders in DataCons]


        -- INVARIANT: the UnivTyVars and ExTyVars all have distinct OccNames
        -- Reason: less confusing, and easier to generate IfaceSyn

        dcEqSpec :: [EqSpec],   -- Equalities derived from the result type,
                                -- _as written by the programmer_

                -- This field allows us to move conveniently between the two ways
                -- of representing a GADT constructor's type:
                --      MkT :: forall a b. (a ~ [b]) => b -> T a
                --      MkT :: forall b. b -> T [b]
                -- Each equality is of the form (a ~ ty), where 'a' is one of
                -- the universally quantified type variables

                -- The next two fields give the type context of the data constructor
                --      (aside from the GADT constraints,
                --       which are given by the dcExpSpec)
                -- In GADT form, this is *exactly* what the programmer writes, even if
                -- the context constrains only universally quantified variables
                --      MkT :: forall a b. (a ~ b, Ord b) => a -> T a b
        dcOtherTheta :: ThetaType,  -- The other constraints in the data con's type
                                    -- other than those in the dcEqSpec

        dcStupidTheta :: ThetaType,     -- The context of the data type declaration
                                        --      data Eq a => T a = ...
                                        -- or, rather, a "thinned" version thereof
                -- "Thinned", because the Report says
                -- to eliminate any constraints that don't mention
                -- tyvars free in the arg types for this constructor
                --
                -- INVARIANT: the free tyvars of dcStupidTheta are a subset of dcUnivTyVars
                -- Reason: dcStupidTeta is gotten by thinning the stupid theta from the tycon
                --
                -- "Stupid", because the dictionaries aren't used for anything.
                -- Indeed, [as of March 02] they are no longer in the type of
                -- the wrapper Id, because that makes it harder to use the wrap-id
                -- to rebuild values after record selection or in generics.

        dcOrigArgTys :: [Type],         -- Original argument types
                                        -- (before unboxing and flattening of strict fields)
        dcOrigResTy :: Type,            -- Original result type, as seen by the user
                -- NB: for a data instance, the original user result type may
                -- differ from the DataCon's representation TyCon.  Example
                --      data instance T [a] where MkT :: a -> T [a]
                -- The OrigResTy is T [a], but the dcRepTyCon might be :T123

        -- Now the strictness annotations and field labels of the constructor
        dcSrcBangs :: [HsSrcBang],
                -- See Note [Bangs on data constructor arguments]
                --
                -- The [HsSrcBang] as written by the programmer.
                --
                -- Matches 1-1 with dcOrigArgTys
                -- Hence length = dataConSourceArity dataCon

        dcFields  :: [FieldLabel],
                -- Field labels for this constructor, in the
                -- same order as the dcOrigArgTys;
                -- length = 0 (if not a record) or dataConSourceArity.

        -- The curried worker function that corresponds to the constructor:
        -- It doesn't have an unfolding; the code generator saturates these Ids
        -- and allocates a real constructor when it finds one.
        dcWorkId :: Id,

        -- Constructor representation
        dcRep      :: DataConRep,

        -- Cached
          -- dcRepArity == length dataConRepArgTys
        dcRepArity    :: Arity,
          -- dcSourceArity == length dcOrigArgTys
        dcSourceArity :: Arity,

        -- Result type of constructor is T t1..tn
        dcRepTyCon  :: TyCon,           -- Result tycon, T

        dcRepType   :: Type,    -- Type of the constructor
                                --      forall a x y. (a~(x,y), x~y, Ord x) =>
                                --        x -> y -> T a
                                -- (this is *not* of the constructor wrapper Id:
                                --  see Note [Data con representation] below)
        -- Notice that the existential type parameters come *second*.
        -- Reason: in a case expression we may find:
        --      case (e :: T t) of
        --        MkT x y co1 co2 (d:Ord x) (v:r) (w:F s) -> ...
        -- It's convenient to apply the rep-type of MkT to 't', to get
        --      forall x y. (t~(x,y), x~y, Ord x) => x -> y -> T t
        -- and use that to check the pattern.  Mind you, this is really only
        -- used in CoreLint.


        dcInfix :: Bool,        -- True <=> declared infix
                                -- Used for Template Haskell and 'deriving' only
                                -- The actual fixity is stored elsewhere

        dcPromoted :: TyCon    -- The promoted TyCon
                               -- See Note [Promoted data constructors] in TyCon
  }
--  deriving Data.Typeable.Typeable
