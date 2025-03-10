{-# LANGUAGE DeriveGeneric #-}

module Types (Workflow(..), Task(..), ExecutionState(..), TaskNode(..), TaskOutput(..), TaskGraph(..), IdResponse(..), WorkflowResponse(..), ExecutionResponse(..), ExecutionRecord(..)) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Control.Monad.State
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
    | OutputValue
    deriving (Show, Eq, Generic)

data Task = Task {
    name :: String,
    command :: Maybe String,  -- 🔹 Ahora `command` es opcional
    script :: Maybe String,   -- 🔹 Nuevo campo para scripts
    input :: [Maybe String],
    output :: Maybe TaskOutput,
    depends_on :: [String]
} deriving (Show, Generic)

instance FromJSON Task
instance ToJSON Task

instance FromJSON TaskOutput where
    parseJSON (String s) = return (OutputFile (unpack s))  -- 🔹 Convertimos `Text` a `String`
    parseJSON (Object _) = return OutputValue
    parseJSON _ = fail "Formato de output inválido"

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
  } deriving (Generic, Show)

instance FromJSON ExecutionRecord
instance ToJSON ExecutionRecord