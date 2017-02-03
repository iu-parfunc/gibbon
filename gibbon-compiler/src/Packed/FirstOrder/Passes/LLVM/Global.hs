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
       , G.returnType  = T.i64
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

printInt :: G.Global
printInt = G.functionDefaults
           { G.name        = AST.Name "__print_int"
           , G.parameters  = ([G.Parameter ty nm []], False)
           , G.returnType  = T.i64
           }
  where ty = T.i64
        nm = AST.UnName 0


globalSizeParam :: G.Global
globalSizeParam = G.globalVariableDefaults
                  { G.name  = AST.Name "global_size_param"
                  , G.type' = T.i64
                  , G.initializer = Just $ C.Int 64 1
                  }
