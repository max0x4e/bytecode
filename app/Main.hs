module Main where

import System.Environment ( getArgs )
import System.Exit () 
import System.FilePath ((</>))
import Control.Exception( try, SomeException ) 
import Control.Monad ()
import Bytecode ( runInterpreter )

driver::[FilePath] -> IO ()
driver args
    | null args = runInterpreter ""
    | otherwise = do
          readResult <- try $ readFile (head args) :: IO (Either SomeException String)
          case readResult of
              Left _ -> putStrLn "Usage: bytecode [<a file containing valid sequence of bytecode mnemonic commands>]"
              Right initCode -> runInterpreter initCode

main :: IO () 
main = do
    args <- getArgs
    driver args
