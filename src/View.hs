{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module View
  ( buildUI
  ) where

import Control.Lens ((^.))
import Data.Text (Text, pack)
import Monomer
import Model
import Update

-- | Build the full application interface.
buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI _ model =
  case (showDialog, selectedTaskValue) of
    (True, _) ->
      vstack [headerRow, errorPanel, newTaskPanel form, columnsRow]

    (_, Just task) ->
      vstack [headerRow, errorPanel, taskDetailsPanel task, columnsRow]

    _ ->
      vstack [headerRow, errorPanel, columnsRow]
  where
    board = model ^. appBoard
    showDialog = model ^. showNewTaskDialog
    selectedTaskValue = model ^. selectedTask
    form = model ^. newTaskForm
    errorMessage = model ^. appErrorMessage

    -- | Top control panel.
    headerRow =
      hstack
        [ label "Kanban Board"
            `styleBasic` [textSize 24, textColor blackColor]
        , spacer
        , button "+ New Task" OpenNewTaskDialog
            `styleBasic` [padding 5]
        , spacer_ [width 10]
        , button "Save" SaveToFile
            `styleBasic` [padding 5]
        ]
        `styleBasic` [padding 10]

    -- | Error message area.
    errorPanel =
      case errorMessage of
        Nothing ->
          spacer

        Just msg ->
          label msg
            `styleBasic`
              [ textColor errorColor
              , padding 10
              ]

    -- | Three Kanban columns.
    columnsRow =
      hstack
        [ boardColumn "To Do" (boardTodos board)
        , boardColumn "In Progress" (boardInProgress board)
        , boardColumn "Done" (boardDones board)
        ]
        `styleBasic` [padding 10]

-- | Render a single board column.
boardColumn :: Text -> [Task] -> WidgetNode AppModel AppEvent
boardColumn titleText tasks =
  vstack
    [ label titleText
        `styleBasic`
          [ textSize 18
          , textCenter
          , textColor blackColor
          , bgColor lightGrayColor
          , padding 5
          ]
    , scroll (vstack (map taskWidget tasks))
        `styleBasic` [width 220, height 350, padding 5]
    ]
    `styleBasic` [border 1 grayColor, padding 5]

-- | Render one task card.
taskWidget :: Task -> WidgetNode AppModel AppEvent
taskWidget task =
  vstack
    [ hstack
        [ label (taskTitle task)
            `styleBasic` [textSize 14, textColor blackColor]
        , spacer
        , button "i" (SelectTask task)
            `styleBasic` [width 30, height 30]
        , button "Edit" (EditTask task)
            `styleBasic` [width 50, height 30]
        , button "X" (DeleteTask (taskId task))
            `styleBasic`
              [ width 30
              , height 30
              , textSize 16
              , bgColor lightCoralColor
              ]
        ]
    , hstack
        [ label (pack (show (taskType task)))
            `styleBasic` [textSize 10]
        , spacer
        , button "T" (MoveTask (taskId task) ToDo)
        , button "P" (MoveTask (taskId task) InProgress)
        , button "D" (MoveTask (taskId task) Done)
        ]
    ]
    `styleBasic`
      [ border 1 grayColor
      , padding 5
      , bgColor lightGrayColor
      ]

-- | Render task details panel.
taskDetailsPanel :: Task -> WidgetNode AppModel AppEvent
taskDetailsPanel task =
  vstack
    [ label "Task Details"
        `styleBasic` [textSize 16, padding 5, textColor blackColor]
    , label ("Title: " <> taskTitle task)
        `styleBasic` [textColor blackColor]
    , label ("Description: " <> taskDesc task)
        `styleBasic` [textColor blackColor]
    , label ("Type: " <> pack (show (taskType task)))
        `styleBasic` [textColor blackColor]
    , label ("Tags: " <> renderTags (taskTags task))
        `styleBasic` [textColor blackColor]
    , label ("Assignee: " <> renderAssignee (taskAssignee task))
        `styleBasic` [textColor blackColor]
    , button "Close" CloseTaskDetails
        `styleBasic` [padding 5]
    ]
    `styleBasic`
      [ border 1 blackColor
      , padding 10
      , bgColor whiteColor
      ]

-- | Render create/edit task panel.
newTaskPanel :: NewTaskData -> WidgetNode AppModel AppEvent
newTaskPanel formData =
  vstack
    [ label "Create / Edit Task"
        `styleBasic` [textSize 14, padding 5]
    , hstack
        [ labeled "Title:"
            (textField (newTaskForm . newTitle)
              `styleBasic` [width 200])

        , labeled "Description:"
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
                [ typeButton Bug (formData ^. newType == Bug)
                , typeButton Feature (formData ^. newType == Feature)
                , typeButton Question (formData ^. newType == Question)
                , typeButton Testing (formData ^. newType == Testing)
                ]
            ]

        , button "Save" SaveNewTask
        , button "Cancel" CloseDialog
        ]
    ]
    `styleBasic` [border 1 blackColor, padding 5]

-- | Render a button for selecting task type.
typeButton :: TaskType -> Bool -> WidgetNode AppModel AppEvent
typeButton taskTypeValue isActive =
  button (taskTypeToText taskTypeValue) (UpdateNewType taskTypeValue)
    `styleBasic` styleList
  where
    styleList =
      [padding 2] ++ if isActive then [bgColor lightBlueColor] else []

-- | Convert task type to text.
taskTypeToText :: TaskType -> Text
taskTypeToText Bug = "Bug"
taskTypeToText Feature = "Feature"
taskTypeToText Question = "Question"
taskTypeToText Testing = "Testing"

-- | Combine a label and a widget into a small vertical block.
labeled
  :: Text
  -> WidgetNode AppModel AppEvent
  -> WidgetNode AppModel AppEvent
labeled fieldLabel widget =
  vstack
    [ label fieldLabel
    , widget
    ]

-- | Convert tag list into display text.
renderTags :: [Tag] -> Text
renderTags tags
  | null tags = "None"
  | otherwise = pack (show (map tagName tags))

-- | Convert assignee value into display text.
renderAssignee :: Maybe User -> Text
renderAssignee Nothing = "None"
renderAssignee (Just user) = userName user

-- | Standard black color.
blackColor :: Color
blackColor = Color 0 0 0 255

-- | Standard white color.
whiteColor :: Color
whiteColor = Color 255 255 255 255

-- | Standard light gray color.
lightGrayColor :: Color
lightGrayColor = Color 240 240 240 255

-- | Standard gray color.
grayColor :: Color
grayColor = Color 200 200 200 255

-- | Standard light blue color.
lightBlueColor :: Color
lightBlueColor = Color 173 216 230 255

-- | Standard light coral color.
lightCoralColor :: Color
lightCoralColor = Color 240 128 128 255

-- | Color used for validation errors.
errorColor :: Color
errorColor = Color 200 0 0 255
