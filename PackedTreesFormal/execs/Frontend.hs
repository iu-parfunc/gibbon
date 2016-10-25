{-# LANGUAGE LambdaCase #-}

module Main where

--------------------------------------------------------------------------------

import System.Environment (getArgs)
import System.Exit (exitFailure)

import Language.Haskell.Exts.Parser
import Packed.FirstOrder.HaskellFrontend

--------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
      [path] -> run path
      _      -> putStrLn "USAGE: packed-trees <FILE PATH>" >> exitFailure

-- run = error "Reenable me!!"                
run :: FilePath -> IO ()
run path =
    fmap parse (readFile path) >>= \case
      ParseOk hs -> do
        putStrLn "haskell-src-exts parsed OK. Desugaring..."
        case desugarModule hs of
          Right ast -> do
            putStrLn "Desugared AST:"
            print ast
          Left err -> do
            putStrLn ("Desugaring failed: " ++ err)
            exitFailure
      ParseFailed _ err -> do
        putStrLn ("haskell-src-exts failed: " ++ err)
        exitFailure
