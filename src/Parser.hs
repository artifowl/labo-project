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

-- Function destinated to only keep our file's AST without the include
filterFromSource :: FilePath -> CTranslUnit -> [CExtDecl]
filterFromSource myFile (CTranslUnit decls _) = 
    filter (\decl -> getfile (nodeInfo decl) == myFile) decls
  where 
    getfile :: NodeInfo -> FilePath
    getfile ni = posFile (posOfNode ni)

-- Main function -> File -> Parsed File -> AST File
mainParser :: FilePath -> IO (Either String [CExtDecl])
mainParser file = do
    let gccPreprocessor = newGCC "gcc"
    let myInputFile = file
    let myOutputFile = "app/preprocessed_output.i"
    let parseFile = "app/AST.txt"

    let cppArgs = ["-Wall", myInputFile, "-o", myOutputFile]

    case parseCPPArgs gccPreprocessor cppArgs of
        Left err -> return $ Left $ "Preprocessor argument error: " ++ err
        Right (args, remainingArgs) -> do
            putStrLn $ "Remaining arguments: " ++ show remainingArgs
            _ <- runCPP gccPreprocessor args

            if isPreprocessed myOutputFile
                then do
                    contents <- BS.readFile myOutputFile
                    let pos = position 0 myOutputFile 0 0 Nothing
                    let parseResult = parseC contents pos
                    case parseResult of
                        Left err -> return $ Left $ "Parsing error: " ++ show err
                        Right translUnit -> do
                            putStrLn "Parsing successful" 
                            let parseFilter = filterFromSource myInputFile translUnit                           
                            -- Génère l'AST proprement dit en texte
                            let parseFilterText = Text.pack (show parseFilter)
                            -- Sauvegarde de l'AST textuel
                            result <- try (T.writeFile parseFile parseFilterText) :: IO (Either IOException ())
                            case result of 
                                Left err -> return $ Left $ "File write error: " ++ show err
                                Right () -> do 
                                    putStrLn "Writing success"
                                    return $ Right parseFilter
            else return $ Left "The file was not preprocessed correctly."