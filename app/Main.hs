{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Network.Wai.Parse (fileContent)
import System.IO.Temp (withSystemTempFile)
import System.IO (hClose)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Lazy as TL
import Control.Monad.IO.Class (liftIO)

import Parser (mainParser)
import Analyzer (analyzeBufferOverflow, analyzeUninitializedVars, formatBufferOverflowRisks, formatUninitWarnings)

-- Function to generate the HTML page with the results
pageHtml :: TL.Text -> TL.Text
pageHtml resultHtml = TL.pack $ unlines
  [ "<!DOCTYPE html>"
  , "<html lang='en'>"
  , "<head>"
  , "  <meta charset='UTF-8'>"
  , "  <meta name='viewport' content='width=device-width, initial-scale=1.0'>"
  , "  <title>Static C Analyzer</title>"
  , "  <style>"
  , "    body {"
  , "      font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;"
  , "      background-color: #f5f7fa;"
  , "      color: #333;"
  , "      margin: 2rem auto;"
  , "      max-width: 800px;"
  , "      padding: 1rem 2rem;"
  , "      box-shadow: 0 0 15px rgba(0,0,0,0.1);"
  , "      border-radius: 8px;"
  , "    }"
  , "    header {"
  , "      border-bottom: 1px solid #ddd;"
  , "      padding-bottom: 1rem;"
  , "      margin-bottom: 2rem;"
  , "      text-align: center;"
  , "    }"
  , "    header h1 {"
  , "      color: #2a6ebb;"
  , "      margin: 0;"
  , "      font-weight: 700;"
  , "    }"
  , "    header p {"
  , "      margin: 0.3rem 0 0.7rem 0;"
  , "      font-style: italic;"
  , "      color: #555;"
  , "      font-size: 1rem;"
  , "    }"
  , "    header a {"
  , "      color: #2a6ebb;"
  , "      font-weight: 600;"
  , "      text-decoration: none;"
  , "      font-size: 1rem;"
  , "    }"
  , "    header a:hover {"
  , "      text-decoration: underline;"
  , "    }"
  , "    footer {"
  , "      margin-top: 3rem;"
  , "      text-align: center;"
  , "      font-size: 0.9rem;"
  , "      color: #888;"
  , "      border-top: 1px solid #ddd;"
  , "      padding-top: 1rem;"
  , "    }"
  , "    form {"
  , "      margin-top: 2rem;"
  , "      text-align: center;"
  , "    }"
  , "    input[type='file'] {"
  , "      padding: 0.5rem;"
  , "      border: 1px solid #ccc;"
  , "      border-radius: 4px;"
  , "      margin-bottom: 1rem;"
  , "      width: 100%;"
  , "      max-width: 400px;"
  , "    }"
  , "    input[type='submit'] {"
  , "      background-color: #2a6ebb;"
  , "      color: white;"
  , "      border: none;"
  , "      padding: 0.7rem 1.5rem;"
  , "      font-size: 1rem;"
  , "      border-radius: 4px;"
  , "      cursor: pointer;"
  , "      transition: background-color 0.3s ease;"
  , "    }"
  , "    input[type='submit']:hover {"
  , "      background-color: #1a4d8c;"
  , "    }"
  , "    pre {"
  , "      background-color: #222;"
  , "      color: #eee;"
  , "      padding: 1rem;"
  , "      border-radius: 6px;"
  , "      overflow-x: auto;"
  , "      margin-top: 2rem;"
  , "      font-size: 0.9rem;"
  , "      line-height: 1.4;"
  , "      white-space: pre-wrap;"
  , "      word-wrap: break-word;"
  , "    }"
  , "    .success { color: #4CAF50; font-weight: bold; }"
  , "    .error { color: #f44336; font-weight: bold; }"
  , "    .warning { color: #ff9800; font-weight: bold; }"
  , "  </style>"
  , "</head>"
  , "<body>"
  , "  <header>"
  , "    <h1>🛡️ Static C Analyzer</h1>"
  , "    <p>Laboratory Project 2025</p>"
  , "    <a href='https://github.com/artifowl/labo-project' target='_blank' rel='noopener noreferrer'>GitHub Repository</a>"
  , "  </header>"
  , "  <form method='post' enctype='multipart/form-data'>"
  , "    Upload your .c file:<br>"
  , "    <input type='file' name='file' accept='.c'/><br>"
  , "    <input type='submit' value='Analyze'/>"
  , "  </form>"
  , ""
  , "  <!-- Result will show here -->"
  , "  " ++ TL.unpack resultHtml
  , ""
  , "  <footer>"
  , "    Created by Elouan Le Marrec"
  , "  </footer>"
  , "</body>"
  , "</html>"
  ]



-- Main function to run the web server
main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    html $ pageHtml "" 

  post "/" $ do
    files <- files
    case lookup "file" files of
      Just file -> do
        let content = fileContent file
        resultText <- liftIO $ withSystemTempFile "upload.c" $ \path handle -> do
          BL.hPut handle content
          hClose handle
          result <- mainParser path
          case result of
            Left err -> return $ "<pre class='error'>❌ Parsing failed:\n" ++ err ++ "</pre>"
            Right ast -> do
              let overflowOutput = formatBufferOverflowRisks (analyzeBufferOverflow ast)
              let uninitOutput   = formatUninitWarnings (analyzeUninitializedVars ast)
              return $ "<pre class='success'>✅ Parsing OK\n\n" ++ overflowOutput ++ uninitOutput ++ "</pre>"
        html $ pageHtml (TL.pack resultText)
      Nothing -> html $ pageHtml "<pre class='error'>❌ No file uploaded.</pre>"
