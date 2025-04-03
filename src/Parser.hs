module Parser(mainParser) where

import Language.C.System.GCC
import Language.C.System.Preprocess
import System.IO(withFile)

mainParser :: IO()
mainParser = do
    let gccPrepocessor = newGCC "gcc"
    let cppArgs = ["--help"]
    case parseCPPArgs gccPrepocessor cppArgs of 
        Left err -> putStrLn("Erreur : " ++ err)
        Right(args, remainArgs) -> do

            -- Utiliser un fichier pour y passer des arguments

            putStrLn ("Arguments remaining : " ++ show remainArgs)
            exitCode <- runCPP gccPrepocessor args
            putStrLn ("Exit code : " ++ show exitCode)





