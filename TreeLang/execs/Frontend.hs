{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath
import Packed.FirstOrder.Compiler

main :: IO ()
main = do
    args <- getArgs
    case args of
      [path] ->
          if takeExtension path == ".hs"
          then compileHSFile path
          else if elem (takeExtension path) [".sexp",".rkt"]
          then compileSExpFile path
          else err
      _ -> err
 where
  err = do putStrLn "USAGE: packed-trees <FILE PATH>"
           putStrLn "  Takes either a .hs or a .sexp input file."
           exitFailure
