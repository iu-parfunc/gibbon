module Packed.FirstOrder.Passes.LLVM.Type where

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Type as T

class TypeOf x where
  typeOf :: x -> T.Type

instance TypeOf AST.Operand where
  typeOf op =
    case op of
      (AST.LocalReference ty _) -> ty
      (AST.ConstantOperand c)   -> typeOf c
      op -> error $ "typeOf Op: Not implemented " ++ show op

instance TypeOf C.Constant where
  typeOf c =
    case c of
      (C.Int bits _) -> T.IntegerType bits
      c -> error $ "typeOf Constant: Not implemented " ++ show c

data KindOf = IntegerK | PointerK | NamedK

kindOf :: T.Type -> KindOf
kindOf (T.IntegerType x) = IntegerK
kindOf (T.PointerType _ _) = PointerK
kindOf (T.NamedTypeReference (AST.Name _)) = NamedK
