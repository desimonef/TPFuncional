{-# LANGUAGE DeriveGeneric #-}

module Types 
  ( Workflow(..)
  , Task(..)
  , TaskNode(..)
  , TaskInput(..)
  , TaskOutput(..)
  , TaskGraph(..)
  , IdResponse(..)
  , WorkflowResponse(..)
  , ExecutionResponse(..)
  , ExecutionRecord(..)
  , RetryPolicy(..)
  , FailStrategy(..)
  , ExecutionPlan(..)
  ) where

import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

-- Definición del workflow
data Workflow = Workflow {
    workflow_name :: String,
    tasks :: [Task]
} deriving (Show, Generic)

data Task = Task {
    name :: String,
    command :: Maybe String,
    script :: Maybe String,
    input :: [TaskInput],
    output :: Maybe String,
    depends_on :: [String],
    retryPolicy :: Maybe RetryPolicy
} deriving (Show, Generic)

data TaskNode = TaskNode {
    task :: Task,
    dependencies :: [TaskNode]
} deriving (Show)

data TaskGraph = TaskGraph {
    taskMap :: [(String, TaskNode)]
} deriving (Show)

data TaskInput = FileInput String | VarInput String
    deriving (Show, Eq, Generic)

data TaskOutput = OutputFile String | OutputValue String
    deriving (Show, Eq, Generic)

data IdResponse = IdResponse { id :: Int }
    deriving (Generic, Show)

data WorkflowResponse = WorkflowResponse
  { workflowId :: Int
  , workflowName :: String
  , definition :: Text
  } deriving (Generic, Show)

data ExecutionResponse = ExecutionResponse
  { executionStatus :: Text
  } deriving (Generic, Show)

data ExecutionRecord = ExecutionRecord
  { executionId :: Int
  , workflow :: Int
  , timestamp :: UTCTime
  , status :: String
  } deriving (Generic, Show)

data RetryPolicy = RetryPolicy {
    maxRetries :: Int,
    failStrategy :: FailStrategy
} deriving (Show, Generic)

data FailStrategy = FailWorkflow | ContinueWorkflow
    deriving (Show, Generic)

data ExecutionPlan = ExecutionPlan
    { taskName :: String
    , execCommand :: String
    , execArgs    :: [TaskInput]
    , execOutput  :: TaskOutput
    }
