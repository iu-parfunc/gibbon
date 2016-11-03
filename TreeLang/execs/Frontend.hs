module Main where
import System.Environment (getArgs)
import Packed.FirstOrder.Compiler (compileCmd)

main :: IO ()
main = getArgs >>= compileCmd 
