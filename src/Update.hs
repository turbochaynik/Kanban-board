{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Update
  ( AppEvent(..)
  , handleEvent
  , saveModel
  ) where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import qualified Data.Text as T
import Monomer hiding (Task)
import qualified Monomer as M
import Model

-- | Save current model to a JSON file.
saveModel :: AppModel -> IO ()
saveModel model = BS.writeFile "kanban.json" (encode model)

-- | Application events triggered by the UI.
data AppEvent
  = AppInit
  | MoveTask TaskId TaskStatus
  | DeleteTask TaskId
  | OpenNewTaskDialog
  | CloseDialog
  | UpdateNewType TaskType
  | SaveNewTask
  | SaveToFile
  | SelectTask Task
  | CloseTaskDetails
  | EditTask Task
  deriving (Show)

-- | Main event handler of the application.
handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent _ _ model = \case
  -- | Do nothing on initialization.
  AppInit ->
    []

  -- | Move a task to another board column.
  MoveTask taskIdValue newStatus ->
    [ Model $
        model
          & appBoard .~ moveTaskToStatus
              taskIdValue
              newStatus
              (model ^. appBoard)
    ]

  -- | Delete a task from all columns.
  DeleteTask taskIdValue ->
    [ Model $
        model
          & appBoard .~ deleteTaskById
              taskIdValue
              (model ^. appBoard)
          & selectedTask .~ Nothing
    ]

  -- | Open dialog for creating a new task.
  OpenNewTaskDialog ->
    [ Model $
        model
          & newTaskForm .~ emptyNewTaskData
          & showNewTaskDialog .~ True
          & editingTaskId .~ Nothing
          & appErrorMessage .~ Nothing
    ]

  -- | Select a task and show its details.
  SelectTask task ->
    [ Model $ model & selectedTask .~ Just task ]

  -- | Close task details panel.
  CloseTaskDetails ->
    [ Model $ model & selectedTask .~ Nothing ]

  -- | Close task form dialog and clear temporary form state.
  CloseDialog ->
    [ Model $
        model
          & showNewTaskDialog .~ False
          & editingTaskId .~ Nothing
          & newTaskForm .~ emptyNewTaskData
          & appErrorMessage .~ Nothing
    ]

  -- | Change task type in the form.
  UpdateNewType taskTypeValue ->
    [ Model $ model & newTaskForm . newType .~ taskTypeValue ]

  -- | Fill the form with existing task data for editing.
  EditTask task ->
    [ Model $
        model
          & newTaskForm .~ NewTaskData
              { _newTitle = taskTitle task
              , _newDesc = taskDesc task
              , _newType = taskType task
              , _newTags = tagsToText (taskTags task)
              , _newAssignee = maybe "" userName (taskAssignee task)
              }
          & editingTaskId .~ Just (taskId task)
          & showNewTaskDialog .~ True
          & selectedTask .~ Nothing
          & appErrorMessage .~ Nothing
    ]

  -- | Validate and save form data either as a new or edited task.
  SaveNewTask ->
    case validateNewTaskData (model ^. newTaskForm) of
      Left err ->
        [ Model $ model & appErrorMessage .~ Just err ]

      Right validForm ->
        case model ^. editingTaskId of
          Just taskIdValue ->
            [ Model $
                model
                  & appBoard .~ updateTaskById
                      taskIdValue
                      validForm
                      (model ^. appBoard)
                  & showNewTaskDialog .~ False
                  & editingTaskId .~ Nothing
                  & newTaskForm .~ emptyNewTaskData
                  & appErrorMessage .~ Nothing
            ]

          Nothing ->
            [ Model $
                model
                  & appBoard .~ addTaskToBoard
                      validForm
                      (model ^. appBoard)
                  & showNewTaskDialog .~ False
                  & newTaskForm .~ emptyNewTaskData
                  & appErrorMessage .~ Nothing
            ]

  -- | Save the full application state to disk.
  SaveToFile ->
    [ M.Task $ saveModel model >> pure AppInit ]

-- | Validate and normalize form data before saving.
validateNewTaskData :: NewTaskData -> Either Text NewTaskData
validateNewTaskData formData
  | T.null trimmedTitle = Left "Title must not be empty."
  | otherwise =
      Right $
        formData
          { _newTitle = trimmedTitle
          , _newDesc = T.strip (_newDesc formData)
          , _newTags = T.strip (_newTags formData)
          , _newAssignee = T.strip (_newAssignee formData)
          }
  where
    trimmedTitle = T.strip (_newTitle formData)

-- | Add a new task to the To Do column.
addTaskToBoard :: NewTaskData -> KanbanBoard -> KanbanBoard
addTaskToBoard formData board =
  board { boardTodos = newTask : boardTodos board }
  where
    newTask = Task
      { taskId = nextTaskId board
      , taskTitle = formData ^. newTitle
      , taskDesc = formData ^. newDesc
      , taskDeadline = Nothing
      , taskType = formData ^. newType
      , taskTags = parseTags (formData ^. newTags)
      , taskAssignee = parseAssignee (formData ^. newAssignee)
      }

-- | Update an existing task in every board column.
updateTaskById :: TaskId -> NewTaskData -> KanbanBoard -> KanbanBoard
updateTaskById taskIdValue formData board =
  board
    { boardTodos = map updateTask (boardTodos board)
    , boardInProgress = map updateTask (boardInProgress board)
    , boardDones = map updateTask (boardDones board)
    }
  where
    updateTask :: Task -> Task
    updateTask task
      | taskId task == taskIdValue =
          task
            { taskTitle = formData ^. newTitle
            , taskDesc = formData ^. newDesc
            , taskType = formData ^. newType
            , taskTags = parseTags (formData ^. newTags)
            , taskAssignee = parseAssignee (formData ^. newAssignee)
            }
      | otherwise = task

-- | Generate the next available task identifier.
nextTaskId :: KanbanBoard -> TaskId
nextTaskId board = TaskId (maxId + 1)
  where
    allTasks =
      boardTodos board
        ++ boardInProgress board
        ++ boardDones board

    ids = map getTaskNumber allTasks

    maxId =
      case ids of
        [] -> 0
        xs -> maximum xs

-- | Extract numeric value from a task identifier.
getTaskNumber :: Task -> Int
getTaskNumber task =
  case taskId task of
    TaskId n -> n

-- | Move a task from one column to another.
moveTaskToStatus :: TaskId -> TaskStatus -> KanbanBoard -> KanbanBoard
moveTaskToStatus tid newStatus board =
  case extractTask tid board of
    (Nothing, _) ->
      board

    (Just task, boardWithoutTask) ->
      insertTask newStatus task boardWithoutTask

-- | Remove a task from the board and return it if found.
extractTask :: TaskId -> KanbanBoard -> (Maybe Task, KanbanBoard)
extractTask tid board =
  (foundTask, boardWithoutTask)
  where
    (foundTodo, restTodo) = extractFromList tid (boardTodos board)
    (foundProg, restProg) = extractFromList tid (boardInProgress board)
    (foundDone, restDone) = extractFromList tid (boardDones board)

    foundTask =
      case (foundTodo, foundProg, foundDone) of
        (Just task, _, _) -> Just task
        (_, Just task, _) -> Just task
        (_, _, Just task) -> Just task
        _ -> Nothing

    boardWithoutTask =
      board
        { boardTodos = restTodo
        , boardInProgress = restProg
        , boardDones = restDone
        }

-- | Extract a task by identifier from a list.
extractFromList :: TaskId -> [Task] -> (Maybe Task, [Task])
extractFromList tid = go []
  where
    go :: [Task] -> [Task] -> (Maybe Task, [Task])
    go acc [] = (Nothing, reverse acc)
    go acc (task : tasks)
      | taskId task == tid = (Just task, reverse acc ++ tasks)
      | otherwise = go (task : acc) tasks

-- | Insert a task into the selected board column.
insertTask :: TaskStatus -> Task -> KanbanBoard -> KanbanBoard
insertTask status task board =
  case status of
    ToDo ->
      board { boardTodos = task : boardTodos board }
    InProgress ->
      board { boardInProgress = task : boardInProgress board }
    Done ->
      board { boardDones = task : boardDones board }

-- | Delete a task from every board column.
deleteTaskById :: TaskId -> KanbanBoard -> KanbanBoard
deleteTaskById tid board =
  board
    { boardTodos = filter ((/= tid) . taskId) (boardTodos board)
    , boardInProgress =
        filter ((/= tid) . taskId) (boardInProgress board)
    , boardDones = filter ((/= tid) . taskId) (boardDones board)
    }

-- | Parse comma-separated tags from text.
parseTags :: Text -> [Tag]
parseTags rawTags =
  map Tag cleanedTags
  where
    splitTags = T.splitOn "," rawTags
    trimmedTags = map T.strip splitTags
    cleanedTags = filter (not . T.null) trimmedTags

-- | Parse assignee text into Maybe User.
parseAssignee :: Text -> Maybe User
parseAssignee rawAssignee
  | T.null cleanedAssignee = Nothing
  | otherwise = Just (User cleanedAssignee)
  where
    cleanedAssignee = T.strip rawAssignee

-- | Convert tags into comma-separated text.
tagsToText :: [Tag] -> Text
tagsToText tags = T.intercalate ", " (map tagName tags)
