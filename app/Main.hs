module Main (main) where

import System.Environment(getArgs)
import Parser (mainParser)

main :: IO ()
main = do
  args <- getArgs
  case args of 
    [file] -> do 
      mainParser file
    _ -> putStrLn "wrong arguments"
