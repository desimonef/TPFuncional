{-# LANGUAGE DeriveGeneric #-}

module Types (Workflow(..), Task(..), TaskNode(..), TaskInput(..), TaskOutput(..), TaskGraph(..), IdResponse(..), WorkflowResponse(..), ExecutionResponse(..), ExecutionRecord(..), RetryPolicy(..), FailStrategy(..), ExecutionPlan(..)) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, (.:?), withObject, (.:), Value(..))
import qualified Data.Aeson.Key as Key
import Control.Monad.State
import Control.Monad.Writer
import Data.Aeson (FromJSON, ToJSON, (.:?), (.:), withObject, object, (.=), parseJSON, toJSON)
import Data.Text (unpack)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime, UTCTime)

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
    input :: [TaskInput],
    output :: Maybe String,
    depends_on :: [String],
    retryPolicy :: Maybe RetryPolicy -- Nuevo campo
} deriving (Show, Generic)

instance FromJSON Task
instance ToJSON Task

instance FromJSON TaskOutput 
instance ToJSON TaskOutput

data TaskInput
    = FileInput String  
    | VarInput String  
    deriving (Show, Eq, Generic)

instance FromJSON TaskInput where
    parseJSON = withObject "TaskInput" $ \o -> do
        mFile <- o .:? Key.fromString "file"
        mVar  <- o .:? Key.fromString "var"
        case (mFile, mVar) of
            (Just file, Nothing) -> return $ FileInput file
            (Nothing, Just var)  -> return $ VarInput var
            _ -> fail "TaskInput debe contener exactamente una clave 'file' o 'var'"

instance ToJSON TaskInput where
    toJSON (FileInput file) = object [Key.fromString "file" .= file]
    toJSON (VarInput var)   = object [Key.fromString "var" .= var]



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
    { taskName :: String
    , execCommand :: String
    , execArgs    :: [TaskInput]
    , execOutput  :: TaskOutput
    }
