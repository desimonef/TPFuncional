{-# LANGUAGE DeriveGeneric #-}

module Types (Workflow(..), Task(..), ExecutionState(..), TaskNode(..), TaskOutput(..), TaskGraph(..), IdResponse(..), WorkflowResponse(..), ExecutionResponse(..), ExecutionRecord(..), RetryPolicy(..), FailStrategy(..), ExecutionPlan(..)) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Control.Monad.State
import Control.Monad.Writer
import Data.Aeson (FromJSON, withObject, Value(..), (.:?), parseJSON)
import Data.Text (unpack)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime, UTCTime)

type ExecutionState = StateT [(String, String)] IO

-- Definición del workflow
data Workflow = Workflow {
    workflow_name :: String,
    tasks :: [Task]
} deriving (Show, Generic)

instance FromJSON Workflow
instance ToJSON Workflow

data TaskNode = TaskNode {
    task :: Task,
    dependencies :: [TaskNode]  -- Referencias directas en lugar de nombres
} deriving (Show)

data TaskGraph = TaskGraph { 
    taskMap :: [(String, TaskNode)]         
} deriving (Show)

-- Nuevo tipo para representar el output de una tarea
data TaskOutput
    = OutputFile String
    | OutputValue String
    deriving (Show, Eq, Generic)

data Task = Task {
    name :: String,
    command :: Maybe String,
    script :: Maybe String,
    input :: [Maybe String],
    output :: Maybe String,
    depends_on :: [String],
    retryPolicy :: Maybe RetryPolicy -- Nuevo campo
} deriving (Show, Generic)

instance FromJSON Task
instance ToJSON Task

instance FromJSON TaskOutput 
instance ToJSON TaskOutput

data IdResponse = IdResponse { id :: Int }
    deriving (Generic, Show)

instance FromJSON IdResponse
instance ToJSON IdResponse

data WorkflowResponse = WorkflowResponse
  { workflowId :: Int
  , workflowName :: String
  , definition :: T.Text
  } deriving (Generic, Show)

instance FromJSON WorkflowResponse 
instance ToJSON WorkflowResponse

data ExecutionResponse = ExecutionResponse
  { executionStatus :: T.Text
  } deriving (Generic, Show)

instance FromJSON ExecutionResponse
instance ToJSON ExecutionResponse

data ExecutionRecord = ExecutionRecord
  { executionId :: Int
  , workflow :: Int
  , timestamp :: UTCTime
  , status :: String
  } deriving (Generic, Show)

instance FromJSON ExecutionRecord
instance ToJSON ExecutionRecord

data RetryPolicy = RetryPolicy {
    maxRetries :: Int,         -- Número máximo de intentos
    failStrategy :: FailStrategy -- Estrategia en caso de fallo final
} deriving (Show, Generic)

instance FromJSON RetryPolicy
instance ToJSON RetryPolicy

data FailStrategy = FailWorkflow | ContinueWorkflow
    deriving (Show, Generic)

instance FromJSON FailStrategy
instance ToJSON FailStrategy

data ExecutionPlan = ExecutionPlan
    { execCommand :: String
    , execArgs    :: [String]
    , execOutput  :: TaskOutput
    }
