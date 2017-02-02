-- | Export C functions

module Packed.FirstOrder.Passes.LLVM.Global where

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Type as T
import qualified LLVM.General.AST.AddrSpace as AS
import qualified LLVM.General.AST.Constant as C

puts :: G.Global
puts = G.functionDefaults
       { G.name        = AST.Name "__fputs"
       , G.parameters  = ([G.Parameter ty nm []], False)
       , G.returnType  = T.i32
       }
  where ty = T.PointerType T.i8 (AS.AddrSpace 0)
        nm = AST.UnName 0


mainFn :: [AST.BasicBlock] -> G.Global
mainFn instrs = G.functionDefaults
       { G.name        = AST.Name "__main_expr"
       , G.parameters  = ([], False)
       , G.returnType  = T.VoidType
       , G.basicBlocks = instrs
       }

-- | must match with lib.c
printIntType :: T.Type
printIntType = T.i64

printInt :: G.Global
printInt = G.functionDefaults
           { G.name        = AST.Name "__print_int"
           , G.parameters  = ([G.Parameter ty nm []], False)
           , G.returnType  = T.i32
           }
  where ty = printIntType
        nm = AST.UnName 0

globalSizeParam = G.globalVariableDefaults
                  { G.name  = AST.Name "global_size_param"
                  , G.type' = T.IntegerType 64
                  , G.initializer = Just $ C.Int 64 1
                  }
