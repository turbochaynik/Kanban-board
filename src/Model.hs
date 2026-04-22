{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Model
  ( TaskId(..)
  , TaskType(..)
  , Tag(..)
  , User(..)
  , Task(..)
  , TaskStatus(..)
  , KanbanBoard(..)
  , NewTaskData(..)
  , AppModel(..)
  , newTitle
  , newDesc
  , newType
  , newTags
  , newAssignee
  , appBoard
  , newTaskForm
  , showNewTaskDialog
  , selectedTask
  , editingTaskId
  , appErrorMessage
  , emptyNewTaskData
  , initialModel
  ) where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)

-- | Unique identifier of a task.
newtype TaskId = TaskId Int
  deriving (Eq, Show, Read, Generic)

instance ToJSON TaskId
instance FromJSON TaskId

-- | Type of a task.
data TaskType
  = Bug
  | Feature
  | Question
  | Testing
  deriving (Eq, Show, Read, Generic)

instance ToJSON TaskType
instance FromJSON TaskType

-- | Task tag.
newtype Tag = Tag
  { tagName :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON Tag
instance FromJSON Tag

-- | Task assignee.
newtype User = User
  { userName :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User

-- | Main task entity stored on the board.
data Task = Task
  { taskId       :: TaskId
  , taskTitle    :: Text
  , taskDesc     :: Text
  , taskDeadline :: Maybe Day
  , taskType     :: TaskType
  , taskTags     :: [Tag]
  , taskAssignee :: Maybe User
  }
  deriving (Eq, Show, Generic)

instance ToJSON Task
instance FromJSON Task

-- | Status of a task on the Kanban board.
data TaskStatus
  = ToDo
  | InProgress
  | Done
  deriving (Eq, Show, Read, Generic)

instance ToJSON TaskStatus
instance FromJSON TaskStatus

-- | Kanban board with three columns.
data KanbanBoard = KanbanBoard
  { boardTodos      :: [Task]
  , boardInProgress :: [Task]
  , boardDones      :: [Task]
  }
  deriving (Eq, Show, Generic)

instance ToJSON KanbanBoard
instance FromJSON KanbanBoard

-- | Form state used for creating or editing a task.
data NewTaskData = NewTaskData
  { _newTitle    :: Text
  , _newDesc     :: Text
  , _newType     :: TaskType
  , _newTags     :: Text
  , _newAssignee :: Text
  }
  deriving (Eq, Show, Generic)

makeLenses ''NewTaskData

instance ToJSON NewTaskData
instance FromJSON NewTaskData

-- | Full application state.
data AppModel = AppModel
  { _appBoard          :: KanbanBoard
  , _newTaskForm       :: NewTaskData
  , _showNewTaskDialog :: Bool
  , _selectedTask      :: Maybe Task
  , _editingTaskId     :: Maybe TaskId
  , _appErrorMessage   :: Maybe Text
  }
  deriving (Eq, Show, Generic)

makeLenses ''AppModel

instance ToJSON AppModel
instance FromJSON AppModel

-- | Empty form value used when creating a new task.
emptyNewTaskData :: NewTaskData
emptyNewTaskData = NewTaskData "" "" Bug "" ""

-- | Initial application state.
initialModel :: AppModel
initialModel = AppModel
  { _appBoard = KanbanBoard
      { boardTodos = []
      , boardInProgress = []
      , boardDones = []
      }
  , _newTaskForm = emptyNewTaskData
  , _showNewTaskDialog = False
  , _selectedTask = Nothing
  , _editingTaskId = Nothing
  , _appErrorMessage = Nothing
  }
