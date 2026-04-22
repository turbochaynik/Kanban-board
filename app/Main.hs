{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}

module Main
  ( main
  ) where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BS
import Monomer
import System.Directory (doesFileExist)
import Model
import Update
import View

-- | File path used for storing board state.
saveFilePath :: FilePath
saveFilePath = "kanban.json"

-- | Load saved model from file if it exists and can be decoded.
loadModel :: IO AppModel
loadModel = do
  fileExists <- doesFileExist saveFilePath
  if fileExists
    then do
      content <- BS.readFile saveFilePath
      case decode content of
        Just model -> pure model
        Nothing -> pure initialModel
    else pure initialModel

-- | Application entry point.
main :: IO ()
main = do
  model <- loadModel
  startApp
    model
    handleEvent
    buildUI
    config

-- | Monomer application configuration.
config :: [AppConfig AppModel AppEvent]
config =
  [ appWindowTitle "Kanban Board"
  , appFontDef "Regular" "/System/Library/Fonts/Helvetica.ttc"
  , appTheme darkTheme
  , appInitEvent AppInit
  , appWindowResizable True
  , appWindowState MainWindowMaximized
  ]
