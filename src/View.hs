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
  let
    board = model ^. appBoard
    showDialog = model ^. showNewTaskDialog
    selected = model ^. selectedTask
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

  in case (showDialog, selected) of
       (True, _) ->
         vstack [headerRow, newTaskPanel form selected, columnsRow]

       (_, Just task) ->
         vstack [headerRow, taskDetailsPanel task, columnsRow]

       _ ->
         vstack [headerRow, columnsRow]

  where
    black = Color 0 0 0 255



boardColumn :: Text -> [Task] -> WidgetNode AppModel AppEvent
boardColumn title tasks =
  vstack
    [ label title `styleBasic`
        [textSize 18, textCenter, textColor black, bgColor lightGray, padding 5]
    , scroll (vstack (map taskWidget tasks))
        `styleBasic` [width 220, height 350, padding 5]
    ]
    `styleBasic` [border 1 gray, padding 5]
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
          , button "i" (SelectTask task)
              `styleBasic` [width 30, height 30]
          , button "Edit" (EditTask task)
              `styleBasic` [width 30, height 30]
          , button "✕" (DeleteTask tid)
              `styleBasic`
                [ width 30, height 30
                , textSize 16
                , bgColor lightCoral
                ]
          ]
      , hstack
          [ label (pack $ show $ taskType task)
              `styleBasic` [textSize 10]
          , spacer
          , button "T" (MoveTask tid ToDo)
          , button "P" (MoveTask tid InProgress)
          , button "D" (MoveTask tid Done)
          ]
      ]
      `styleBasic` [border 1 gray, padding 5, bgColor lightGray]

  where
    lightGray  = Color 240 240 240 255
    gray       = Color 200 200 200 255
    black      = Color 0 0 0 255
    lightCoral = Color 240 128 128 255



taskDetailsPanel :: Task -> WidgetNode AppModel AppEvent
taskDetailsPanel task =
  vstack
    [ label "Task Details" `styleBasic` [textSize 16, padding 5, textColor black]

    , label ("Title: " <> taskTitle task) `styleBasic` [textColor black]
    , label ("Description: " <> taskDesc task) `styleBasic` [textColor black]
    , label ("Type: " <> pack (show $ taskType task)) `styleBasic` [textColor black]

    , label ("Tags: " <> tagsText) `styleBasic` [textColor black]
    , label ("Assignee: " <> assigneeText) `styleBasic` [textColor black]

    , button "Close" CloseTaskDetails
        `styleBasic` [padding 5]
    ]
    `styleBasic` [border 1 black, padding 10, bgColor white]
  where
    tagsText =
      if null (taskTags task)
        then "None"
        else pack $ show $ map tagName (taskTags task)

    assigneeText =
      case taskAssignee task of
        Nothing -> "None"
        Just u  -> userName u

    black = Color 0 0 0 255
    white = Color 255 255 255 255


newTaskPanel
  :: NewTaskData
  -> Maybe Task
  -> WidgetNode AppModel AppEvent
newTaskPanel nd selected =
  vstack
    [ label "Create / Edit Task"
        `styleBasic` [textSize 14, padding 5]

    , hstack
        [ labeled "Title:"
            (textField (newTaskForm . newTitle)
              `styleBasic` [width 200])

        , labeled "Desc:"
            (textField (newTaskForm . newDesc)
              `styleBasic` [width 200])

        , labeled "Tags (comma):"
            (textField (newTaskForm . newTags)
              `styleBasic` [width 200])

        , labeled "Assignee:"
            (textField (newTaskForm . newAssignee)
              `styleBasic` [width 200])

        , vstack
            [ label "Type:"
            , hstack
                [ typeButton Bug      (nd ^. newType == Bug)
                , typeButton Feature  (nd ^. newType == Feature)
                , typeButton Question (nd ^. newType == Question)
                , typeButton Testing  (nd ^. newType == Testing)
                ]
            ]

        , button
            (maybe "Save" (const "Update") selected)
            SaveNewTask

        , button "Cancel" CloseDialog
        ]
    ]
    `styleBasic` [border 1 black, padding 5]
  where
    black = Color 0 0 0 255


typeButton :: TaskType -> Bool -> WidgetNode AppModel AppEvent
typeButton t active =
  button (taskTypeToText t) (UpdateNewType t)
    `styleBasic`
      ([padding 2] ++ if active then [bgColor lightBlue] else [])
  where
    lightBlue = Color 173 216 230 255

taskTypeToText :: TaskType -> Text
taskTypeToText Bug      = "Bug"
taskTypeToText Feature  = "Feature"
taskTypeToText Question = "Question"
taskTypeToText Testing  = "Testing"

labeled :: Text -> WidgetNode AppModel AppEvent -> WidgetNode AppModel AppEvent
labeled lbl widget =
  vstack
    [ label lbl
    , widget
    ]
