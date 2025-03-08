{-# LANGUAGE DeriveGeneric #-}

module Types (Workflow(..), Task(..), ExecutionState(..), TaskNode(..), TaskOutput(..), TaskGraph(..)) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Control.Monad.State
import Data.Aeson (FromJSON, withObject, Value(..), (.:?), parseJSON)
import Data.Text (unpack)

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