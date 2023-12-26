{-# LANGUAGE LambdaCase #-}

module Main where

import Convert (convert)
import qualified Html
import qualified Markup
import System.Directory (doesFileExist)
import System.Environment (getArgs)

main :: IO ()
main =
  getArgs >>= \case
    [inputFile, outputFile] -> fileIO inputFile outputFile
    [] -> stdIO
    _ -> invalidInput

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse

confirm :: IO Bool
confirm =
  getLine
    >>= \case
      "y" -> pure True
      "n" -> pure False
      _ -> putStrLn "Invalid response. Use y or n." *> confirm

whenIO :: IO Bool -> IO () -> IO ()
whenIO condIO action =
  condIO >>= \cond ->
    if cond
      then action
      else pure ()

fileIO :: String -> String -> IO ()
fileIO iFile oFile = do
  iFileExists <- doesFileExist iFile
  if iFileExists
    then do
      oFileExists <- doesFileExist oFile
      let readProcessWrite = do
            content <- readFile iFile
            writeFile oFile (process oFile content)
      if oFileExists
        then do
          putStr "Overwrite out file? (y/n): "
          whenIO confirm readProcessWrite
        else readProcessWrite
    else putStrLn "There is no input file"

stdIO :: IO ()
stdIO = do
  putStrLn "Please enter title:"
  title <- getLine
  putStrLn "and content:"
  content <- getContents
  putStrLn (process title content)

invalidInput :: IO ()
invalidInput = putStrLn "Either call the program with 2 arguments that represent the files to read and write, or no arguments to work with standard I/O."
