{-# LANGUAGE LambdaCase #-}

module Main where

import qualified HsBlog
import           OptParse
import           System.Directory               ( doesFileExist )
import           System.Exit                    ( exitFailure )
import           System.IO

main :: IO ()
main = do
  options <- parse
  case options of
    ConvertSingle input output -> do
      (title, inputHandle) <- case input of
        Stdin          -> pure ("", stdin)
        InputFile path -> (,) path <$> openFile path ReadMode

      outputHandle <- case output of
        Stdout                       -> pure stdout
        OutputFile path forceReplace -> do
          exists         <- doesFileExist path
          shouldOpenFile <- if exists && not forceReplace
            then confirm
            else pure True
          if shouldOpenFile then openFile path WriteMode else exitFailure

      HsBlog.convertSingle title inputHandle outputHandle
      hClose inputHandle
      hClose outputHandle

    ConvertDir input output _ -> HsBlog.convertDirectory input output

------------------------------------------------
-- * Utilities

-- | Confirm user action
confirm :: IO Bool
confirm = putStrLn "Are you sure? (y/n)" *> getLine >>= \case
  "y" -> pure True
  "n" -> pure False
  _   -> putStrLn "Invalid response. use y or n" *> confirm
