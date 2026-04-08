{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Model where

import Data.Text (Text)
import Data.Time (Day)
import Control.Lens (makeLenses)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

newtype TaskId = TaskId Int
  deriving (Eq, Show, Read, Generic)

instance ToJSON TaskId
instance FromJSON TaskId

data TaskType
  = Bug
  | Feature
  | Question
  | Testing
  deriving (Eq, Show, Generic)

instance ToJSON TaskType
instance FromJSON TaskType

newtype Tag = Tag { tagName :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON Tag
instance FromJSON Tag

newtype User = User { userName :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User

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

data TaskStatus
  = ToDo
  | InProgress
  | Done
  deriving (Eq, Show, Read, Generic)

instance ToJSON TaskStatus
instance FromJSON TaskStatus

data KanbanBoard = KanbanBoard
  { boardTodos      :: [Task]
  , boardInProgress :: [Task]
  , boardDones      :: [Task]
  }
  deriving (Eq, Show, Generic)

instance ToJSON KanbanBoard
instance FromJSON KanbanBoard

data NewTaskData = NewTaskData
  { _newTitle :: Text
  , _newDesc  :: Text
  , _newType  :: TaskType
  } deriving (Eq, Show, Generic)

makeLenses ''NewTaskData

instance ToJSON NewTaskData
instance FromJSON NewTaskData

data AppModel = AppModel
  { _appBoard          :: KanbanBoard
  , _newTaskForm       :: NewTaskData
  , _showNewTaskDialog :: Bool
  } deriving (Eq, Show, Generic)

makeLenses ''AppModel

instance ToJSON AppModel
instance FromJSON AppModel

emptyNewTaskData :: NewTaskData
emptyNewTaskData = NewTaskData "" "" Bug

initialModel :: AppModel
initialModel = AppModel
  { _appBoard = KanbanBoard
      { boardTodos = []
      , boardInProgress = []
      , boardDones = []
      }
  , _newTaskForm = emptyNewTaskData
  , _showNewTaskDialog = False
  }
