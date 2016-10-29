{-# LANGUAGE LambdaCase #-}

module Main where

--------------------------------------------------------------------------------

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath
import Language.Haskell.Exts.Parser
import Packed.FirstOrder.HaskellFrontend

import qualified Packed.FirstOrder.SExpFrontend as S

--------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
      [path] ->
          if takeExtension path == ".hs"
          then run path
          else if elem (takeExtension path) [".sexp",".rkt"]
          then S.main
          else err
      _ -> err
 where
  err = do putStrLn "USAGE: packed-trees <FILE PATH>"
           putStrLn "  Takes either a .hs or a .sexp input file."
           exitFailure

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
