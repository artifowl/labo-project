module Main (main) where

import System.Environment (getArgs)
import Parser (mainParser)
import Analyzer (analyzeBufferOverflow, analyzeUninitializedVars, printBufferOverflowRisks, printUninitWarnings)

main :: IO ()
main = do
  args <- getArgs
  case args of 
    [file] -> do 
      result <- mainParser file
      case result of
        Left err -> putStrLn $ "❌ " ++ err
        Right ast -> do
          let overflowRisks = analyzeBufferOverflow ast
          let uninitRisks   = analyzeUninitializedVars ast
          printBufferOverflowRisks overflowRisks
          printUninitWarnings uninitRisks
          
    _ -> putStrLn "❌ Usage: ./YourApp <source_file.c>"
