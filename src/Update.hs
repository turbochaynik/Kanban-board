{-# LANGUAGE LambdaCase #-}
module Update where

import Model
import Monomer
import Control.Lens ((.~), (&), (^.))
import Data.Text (Text)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BS
import Control.Monad.IO.Class (liftIO)

saveModel :: AppModel -> IO ()
saveModel model = BS.writeFile "kanban.json" (encode model)

data AppEvent
  = AppInit
  | MoveTask TaskId TaskStatus
  | DeleteTask TaskId
  | OpenNewTaskDialog
  | CloseDialog
  | UpdateNewTitle Text
  | UpdateNewDesc Text
  | UpdateNewType TaskType
  | SaveNewTask
  | SaveToFile
  deriving (Show)

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent _ _ model = \case
  AppInit -> []

  MoveTask taskId newStatus ->
    [Model $ model & appBoard .~ moveTaskToStatus taskId newStatus (model ^. appBoard)]

  DeleteTask taskId ->
    [Model $ model & appBoard .~ deleteTaskById taskId (model ^. appBoard)]

  OpenNewTaskDialog ->
    [Model $ model & newTaskForm .~ emptyNewTaskData & showNewTaskDialog .~ True]

  CloseDialog ->
    [Model $ model & showNewTaskDialog .~ False]

  UpdateNewTitle t ->
    [Model $ model & newTaskForm . newTitle .~ t]

  UpdateNewDesc d ->
    [Model $ model & newTaskForm . newDesc .~ d]

  UpdateNewType tp ->
    [Model $ model & newTaskForm . newType .~ tp]

  SaveNewTask ->
    let form = model ^. newTaskForm
        board = model ^. appBoard
        newId = nextTaskId board
        newTask = Model.Task
          { taskId       = newId
          , taskTitle    = form ^. newTitle
          , taskDesc     = form ^. newDesc
          , taskDeadline = Nothing
          , taskType     = form ^. newType
          , taskTags     = []
          , taskAssignee = Nothing
          }
        newBoard = board { boardTodos = newTask : boardTodos board }
    in [Model $ model & appBoard .~ newBoard & showNewTaskDialog .~ False]

  SaveToFile ->
    [Monomer.Task $ liftIO (saveModel model) >> return AppInit]

nextTaskId :: KanbanBoard -> TaskId
nextTaskId board =
  let allTasks = boardTodos board ++ boardInProgress board ++ boardDones board
      ids = map (\(TaskId n) -> n) (map taskId allTasks)
      maxId = if null ids then 0 else maximum ids
  in TaskId (maxId + 1)

moveTaskToStatus :: TaskId -> TaskStatus -> KanbanBoard -> KanbanBoard
moveTaskToStatus tid newStatus board =
  let (mbTask, boardWithout) = extractTask tid board
  in case mbTask of
       Nothing -> board
       Just task -> insertTask newStatus task boardWithout

extractTask :: TaskId -> KanbanBoard -> (Maybe Task, KanbanBoard)
extractTask tid board =
  let (foundTodo, restTodo) = extractFromList tid (boardTodos board)
      (foundProg, restProg) = extractFromList tid (boardInProgress board)
      (foundDone, restDone) = extractFromList tid (boardDones board)
      mbTask = case (foundTodo, foundProg, foundDone) of
                 (Just t, _, _) -> Just t
                 (_, Just t, _) -> Just t
                 (_, _, Just t) -> Just t
                 _              -> Nothing
  in (mbTask, board { boardTodos = restTodo
                    , boardInProgress = restProg
                    , boardDones = restDone })

extractFromList :: TaskId -> [Task] -> (Maybe Task, [Task])
extractFromList tid = go []
  where
    go acc [] = (Nothing, reverse acc)
    go acc (t:ts)
      | taskId t == tid = (Just t, reverse acc ++ ts)
      | otherwise       = go (t:acc) ts

insertTask :: TaskStatus -> Task -> KanbanBoard -> KanbanBoard
insertTask status task board =
  case status of
    ToDo       -> board { boardTodos = task : boardTodos board }
    InProgress -> board { boardInProgress = task : boardInProgress board }
    Done       -> board { boardDones = task : boardDones board }

deleteTaskById :: TaskId -> KanbanBoard -> KanbanBoard
deleteTaskById tid board =
  board { boardTodos      = filter ((/= tid) . taskId) (boardTodos board)
        , boardInProgress = filter ((/= tid) . taskId) (boardInProgress board)
        , boardDones      = filter ((/= tid) . taskId) (boardDones board)
        }
