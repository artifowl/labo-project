module Parser (mainParser) where

import qualified Data.Text.IO as T
import qualified Data.Text as Text
import Control.Exception (try, IOException)
import Language.C.System.GCC
import Language.C.System.Preprocess
import Language.C.Data.Position
import Language.C.Data.Node
import Language.C.Parser
import Language.C.Syntax.AST
import qualified Data.ByteString as BS
import System.Exit (exitFailure, exitSuccess)


filterFromSource :: FilePath -> CTranslUnit -> [CExtDecl]
filterFromSource myFile (CTranslUnit decls _) = 
    filter (\decl -> getfile (nodeInfo decl) == myFile) decls
  where 
    getfile :: NodeInfo -> FilePath
    getfile ni = posFile (posOfNode ni)
 

mainParser :: IO ()
mainParser = do
    let gccPreprocessor = newGCC "gcc"

    let myInputFile = "app/test.c"
    let myOutputFile = "app/preprocessed_output.i"
    let parseFile = "app/parse.txt"
    let cppArgs = ["-E", myInputFile, "-o", myOutputFile]

    case parseCPPArgs gccPreprocessor cppArgs of
        Left err -> putStrLn $ "Preprocessor argument error: " ++ err
        Right (args, remainingArgs) -> do
            putStrLn $ "Remaining arguments: " ++ show remainingArgs
            _ <- runCPP gccPreprocessor args

            if isPreprocessed myOutputFile
                then do
                    contents <- BS.readFile myOutputFile
                    let pos = position 0 myOutputFile 0 0 Nothing
                    let parseResult = parseC contents pos
                    case parseResult of
                        Left err -> do
                            putStrLn $ "Parsing error: " ++ show err
                            exitFailure
                        Right translUnit -> do
                            putStrLn "Parsing successful"
                            let parseFilter = filterFromSource myInputFile translUnit 
                            let parseFilterText = Text.pack (show parseFilter)
                            result <- try (T.writeFile parseFile parseFilterText) :: IO (Either IOException())
                            case result of 
                                Left err -> do 
                                    putStrLn $ "File write error: " ++ show err
                                    exitFailure
                                Right () -> do 
                                    putStrLn "Writing success"
                                    exitSuccess
                else do
                    putStrLn "The file was not preprocessed correctly."
                    exitFailure
