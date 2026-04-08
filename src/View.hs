{-# LANGUAGE FlexibleContexts #-}
module View where

import Model
import Update

import Monomer
import Monomer.Widgets.Singles.Button
import Monomer.Widgets.Singles.Label
import Monomer.Widgets.Singles.TextField
import Monomer.Widgets.Containers.Stack (hstack, vstack)
import Monomer.Widgets.Containers.Scroll
import Monomer.Widgets.Singles.Spacer (spacer)
import Data.Text (Text, pack)
import Control.Lens ((^.))

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model =
  let board = model ^. appBoard
      showDialog = model ^. showNewTaskDialog
      form = model ^. newTaskForm
      headerRow = hstack
        [ label "Kanban Board" `styleBasic` [textSize 24, textColor black]
        , spacer
        , button "+ New Task" OpenNewTaskDialog `styleBasic` [padding 5]
        , spacer_ [width 10]
        , button "Save" SaveToFile `styleBasic` [padding 5]
        ] `styleBasic` [padding 10]
      columnsRow = hstack
        [ boardColumn "To Do" (boardTodos board)
        , boardColumn "In Progress" (boardInProgress board)
        , boardColumn "Done" (boardDones board)
        ] `styleBasic` [padding 10]
  in if showDialog
       then vstack [headerRow, newTaskPanel form, columnsRow]
       else vstack [headerRow, columnsRow]
  where
    black = Color 0 0 0 255

boardColumn :: Text -> [Task] -> WidgetNode AppModel AppEvent
boardColumn title tasks =
  vstack
    [ label title `styleBasic` [textSize 18, textCenter, textColor black, bgColor lightGray, padding 5]
    , scroll (vstack (map taskWidget tasks)) `styleBasic` [width 220, height 350, padding 5]
    ] `styleBasic` [border 1 gray, padding 5]
  where
    lightGray = Color 240 240 240 255
    gray      = Color 200 200 200 255
    black     = Color 0 0 0 255

taskWidget :: Task -> WidgetNode AppModel AppEvent
taskWidget task =
  let
    tid = taskId task
    title = taskTitle task
  in
    vstack
      [ hstack
          [ label title `styleBasic` [textSize 14, textColor black]
          , spacer
          , button "✕" (DeleteTask tid)
              `styleBasic` [ width 30, height 30
                           , textSize 16, textColor black
                           , bgColor lightCoral, border 1 black
                           ]
          ]
      , hstack
          [ label (pack $ show $ taskType task) `styleBasic` [textSize 10, textColor black]
          , spacer
          , button "T" (MoveTask tid ToDo)
              `styleBasic` [width 30, height 24, textSize 10, textColor black]
          , button "P" (MoveTask tid InProgress)
              `styleBasic` [width 30, height 24, textSize 10, textColor black]
          , button "D" (MoveTask tid Done)
              `styleBasic` [width 30, height 24, textSize 10, textColor black]
          ]
      ] `styleBasic` [border 1 gray, padding 5, bgColor lightGray]
  where
    lightGray  = Color 240 240 240 255
    gray       = Color 200 200 200 255
    black      = Color 0 0 0 255
    lightCoral = Color 240 128 128 255

taskTypeToText :: TaskType -> Text
taskTypeToText Bug      = "Bug"
taskTypeToText Feature  = "Feature"
taskTypeToText Question = "Question"
taskTypeToText Testing  = "Testing"

newTaskPanel :: NewTaskData -> WidgetNode AppModel AppEvent
newTaskPanel nd =
  vstack
    [ label "Create New Task" `styleBasic` [textSize 14, textColor black, padding 5]
    , hstack
        [ labeled "Title:" $
            textField (newTaskForm . newTitle) `styleBasic` [width 200]
        , labeled "Desc:" $
            textField (newTaskForm . newDesc) `styleBasic` [width 200]
        , vstack
            [ label "Type:" `styleBasic` [textSize 10, textColor black]
            , hstack
                [ typeButton Bug      (nd ^. newType == Bug)
                , typeButton Feature  (nd ^. newType == Feature)
                , typeButton Question (nd ^. newType == Question)
                , typeButton Testing  (nd ^. newType == Testing)
                ]
            ] `styleBasic` [padding 2]
        , button "Save" SaveNewTask `styleBasic` [padding 5]
        , button "Cancel" CloseDialog `styleBasic` [padding 5]
        ] `styleBasic` [padding 5]
    ] `styleBasic` [border 1 black, padding 5, bgColor white]
  where
    black = Color 0 0 0 255
    white = Color 255 255 255 255

typeButton :: TaskType -> Bool -> WidgetNode AppModel AppEvent
typeButton t active =
  button (taskTypeToText t) (UpdateNewType t)
    `styleBasic` ([padding 2] ++ if active then [bgColor lightBlue] else [])
  where
    lightBlue = Color 173 216 230 255

labeled :: Text -> WidgetNode AppModel AppEvent -> WidgetNode AppModel AppEvent
labeled lbl widget =
  vstack
    [ label lbl `styleBasic` [textSize 10, textColor black]
    , widget
    ] `styleBasic` [padding 2]
  where
    black = Color 0 0 0 255

