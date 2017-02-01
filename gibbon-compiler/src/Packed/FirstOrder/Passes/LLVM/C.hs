-- | Export C functions

module Packed.FirstOrder.Passes.LLVM.C where

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Type as T
import qualified LLVM.General.AST.AddrSpace as AS

puts :: G.Global
puts = G.functionDefaults
       { G.name        = AST.Name "puts"
       , G.parameters  = ([G.Parameter ty nm []], False)
       , G.returnType  = T.i32
       }
  where ty = T.PointerType T.i8 (AS.AddrSpace 0)
        nm = AST.UnName 0


mainFn :: [AST.BasicBlock] -> G.Global
mainFn instrs = G.functionDefaults
       { G.name        = AST.Name "main"
       , G.parameters  = ([], False)
       , G.returnType  = T.VoidType
       , G.basicBlocks = instrs
       }

-- | must match with lib.c
printIntType :: T.Type
printIntType = T.i32

printInt :: G.Global
printInt = G.functionDefaults
           { G.name        = AST.Name "print_int"
           , G.parameters  = ([G.Parameter ty nm []], False)
           , G.returnType  = T.i32
           }
  where ty = printIntType
        nm = AST.UnName 0
