{-# LANGUAGE LambdaCase #-}
module Update where

import Model
import Monomer
import Control.Lens ((.~), (&), (^.))
import Data.Text (Text)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BS
import Control.Monad.IO.Class (liftIO)
import Data.Text (splitOn, strip)
import qualified Data.Text as T

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
  | UpdateNewTags Text
  | UpdateNewAssignee Text
  | SaveNewTask
  | SaveToFile
  | SelectTask Task
  | CloseTaskDetails
  | EditTask Task
  | SaveEditedTask
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
    
  SelectTask task ->
    [Model $ model & selectedTask .~ Just task]

  CloseTaskDetails ->
    [Model $ model & selectedTask .~ Nothing]

  CloseDialog ->
    [Model $ model & showNewTaskDialog .~ False]

  UpdateNewTitle t ->
    [Model $ model & newTaskForm . newTitle .~ t]

  UpdateNewDesc d ->
    [Model $ model & newTaskForm . newDesc .~ d]

  UpdateNewType tp ->
    [Model $ model & newTaskForm . newType .~ tp]
    
  UpdateNewTags t ->
    [Model $ model & newTaskForm . newTags .~ t]

  UpdateNewAssignee a ->
    [Model $ model & newTaskForm . newAssignee .~ a]
    
  EditTask task ->
    [ Model $
        model
          & newTaskForm .~ NewTaskData
              { _newTitle = taskTitle task
              , _newDesc  = taskDesc task
              , _newType  = taskType task
              , _newTags  = tagsToText (taskTags task)
              , _newAssignee =
                  maybe "" userName (taskAssignee task)
              }
          & editingTaskId .~ Just (taskId task)
          & showNewTaskDialog .~ True
    ]
  
  
  SaveNewTask ->
    case model ^. editingTaskId of

    -- =========================================
    -- ✏️ РЕДАКТИРОВАНИЕ
    -- =========================================
      Just tid ->
        let
          form  = model ^. newTaskForm
          board = model ^. appBoard

          tagsList =
            if form ^. newTags == ""
              then []
              else map (Tag . strip) (splitOnComma (form ^. newTags))

          assigneeVal =
            if form ^. newAssignee == ""
              then Nothing
              else Just (User (form ^. newAssignee))

          updateTask t =
            if taskId t == tid
              then t
                { taskTitle    = form ^. newTitle
                , taskDesc     = form ^. newDesc
                , taskType     = form ^. newType
                , taskTags     = tagsList
                , taskAssignee = assigneeVal
                }
              else t

          newBoard = board
            { boardTodos      = map updateTask (boardTodos board)
            , boardInProgress = map updateTask (boardInProgress board)
            , boardDones      = map updateTask (boardDones board)
            }

        in
          [ Model $
              model
                & appBoard .~ newBoard
                & showNewTaskDialog .~ False
                & editingTaskId .~ Nothing
          ]


    -- =========================================
    -- 🆕 СОЗДАНИЕ
    -- =========================================
      Nothing ->
        let
          form  = model ^. newTaskForm
          board = model ^. appBoard
          newId = nextTaskId board

          tagsList =
            if form ^. newTags == ""
              then []
              else map (Tag . strip) (splitOnComma (form ^. newTags))

          assigneeVal =
            if form ^. newAssignee == ""
              then Nothing
              else Just (User (form ^. newAssignee))
 
          newTask = Model.Task
            { taskId       = newId
            , taskTitle    = form ^. newTitle
            , taskDesc     = form ^. newDesc
            , taskDeadline = Nothing
            , taskType     = form ^. newType
            , taskTags     = tagsList
            , taskAssignee = assigneeVal
            }

          newBoard =
            board { boardTodos = newTask : boardTodos board }

        in
          [ Model $
              model
                & appBoard .~ newBoard
                & showNewTaskDialog .~ False
          ]

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

splitOnComma :: Text -> [Text]
splitOnComma = splitOn ","

tagsToText :: [Tag] -> Text
tagsToText tags =
  T.intercalate "," (map tagName tags)
