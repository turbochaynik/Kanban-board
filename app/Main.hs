{-# LANGUAGE FlexibleContexts #-}
module Main where

import Model
import Update
import View

import Monomer
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BS
import System.Directory (doesFileExist)

saveFilePath :: FilePath
saveFilePath = "kanban.json"

loadModel :: IO AppModel
loadModel = do
  exists <- doesFileExist saveFilePath
  if exists
    then do
      content <- BS.readFile saveFilePath
      case decode content of
        Just model -> return model
        Nothing    -> return initialModel
    else return initialModel

main :: IO ()
main = do
  model <- loadModel
  startApp
    model
    handleEvent
    buildUI
    config

config :: [AppConfig AppModel AppEvent]
config =
  [ appWindowTitle "Kanban Board"
  , appWindowIcon ""
  , appTheme darkTheme
  , appFontDef "Regular" "/System/Library/Fonts/Helvetica.ttc"
  , appInitEvent AppInit
  , appWindowResizable True
  , appWindowState MainWindowMaximized
  ]
