{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Gibbon.TH where

import           Language.Haskell.TH     hiding ( ScopedTypeVariables )
import           Language.Haskell.TH.Quote
import           Control.Monad.State.Strict
import           Data.Set
import           Language.Haskell.Exts.Extension
import           Language.Haskell.Exts.Parser
import           Gibbon.HaskellFrontend
import           Gibbon.Common
import           Gibbon.Compiler
import           Gibbon.DynFlags
import           Gibbon.Passes.Freshen
import           Gibbon.Passes.Codegen
import qualified Gibbon.L0.Syntax              as L0
import qualified Gibbon.L0.Typecheck           as L0
import qualified Gibbon.L0.Specialize2         as L0
import qualified Gibbon.L1.Syntax              as L1
import           System.IO.Unsafe

gQuoter :: QuasiQuoter
gQuoter = QuasiQuoter compileL1 undefined undefined undefined

-- | same as Frontend's parseFile and parseInput function, but not in IO
parseL1 :: String -> (L1.Prog1, Int)
parseL1 code =
  let parse_mode = defaultParseMode
        { extensions = [EnableExtension ScopedTypeVariables]
                         ++ (extensions defaultParseMode)
        }
  in  case parseModuleWithMode parse_mode code of
        ParseOk hs ->
          runPassM defaultConfig 0 . mono_and_spec . desugarModule $ hs
        ParseFailed _ e -> error ("haskell-src-exts failed: " ++ e)
 where
  mono_and_spec :: PassM L0.Prog0 -> PassM L1.Prog1
  mono_and_spec pm_l0 = do
    l00 <- pm_l0
    l01 <- freshNames l00
    l02 <- L0.tcProg l01
    l1  <- L0.l0ToL1 l02
    pure l1

compileL1 :: String -> Q Exp
compileL1 fp1 =
  let (l1, _) = parseL1 fp1
      config' = Config Haskell
                       ToC
                       Nothing
                       Nothing
                       0
                       "gcc"
                       ""
                       Nothing
                       Nothing
                       C
                       (DynFlags empty empty)
                       Nothing
      stM = passes config' l1
      l4  = evalStateT stM (CompileState { cnt = 0, result = Nothing })
      str = (codegenProg config') =<< l4
  in  return . LitE . StringL . unsafePerformIO $ str
