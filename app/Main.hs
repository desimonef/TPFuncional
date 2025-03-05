{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy as B
import System.Process (callCommand)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import qualified Data.Map as M
import qualified Data.Graph as G
import Data.Maybe (fromJust)
import TaskExecutions (execute)
import System.FilePath (takeExtension)
import Database (DB, initDB, saveTaskStatus, getTaskStatus)


-- Definición de una tarea
data Task = Task {
    name :: String,
    command :: String,
    depends_on :: [String]
} deriving (Show, Generic)

instance FromJSON Task

-- Definición del workflow
data Workflow = Workflow {
    workflow_name :: String,
    tasks :: [Task]
} deriving (Show, Generic)

instance FromJSON Workflow

-- Construcción del grafo de tareas
type TaskGraph = (G.Graph, G.Vertex -> (Task, String, [String]), String -> Maybe G.Vertex)

buildTaskGraph :: [Task] -> TaskGraph
buildTaskGraph tasks =
    let taskMap = M.fromList [(name t, t) | t <- tasks]
        edges = [(name t, depends_on t) | t <- tasks]
        nodeInfo (tname, deps) = (fromJust $ M.lookup tname taskMap, tname, deps)
    in G.graphFromEdges (map nodeInfo edges)

-- Obtener el orden topológico del workflow
getExecutionOrder :: [Task] -> [Task]
getExecutionOrder tasks =
    let (graph, nodeFromVertex, _) = buildTaskGraph tasks
        sortedVertices = G.topSort graph  -- Orden topológico de tareas
    in map (\v -> let (task, _, _) = nodeFromVertex v in task) sortedVertices

-- Verificar si una tarea es un script basado en su extensión
isScript :: FilePath -> Bool
isScript path = takeExtension path `elem` [".py", ".js", ".sh", ".bat"]

-- Ejecutar una tarea
executeTask :: DB -> Task -> IO ()
executeTask db task = do
    let cmd = command task
    if isScript cmd
        then execute cmd
        else callCommand cmd
    saveTaskStatus db (name task) True

-- Ejecutar tareas en orden
executeTasks :: DB -> [Task] -> IO ()
executeTasks db [] = putStrLn "Workflow completado!"
executeTasks db (t:ts) = do
    executeTask db t
    executeTasks db ts

main :: IO ()
main = do
    db <- initDB
    
    contents <- B.readFile "workflow.json"
    case decode contents of
        Just wf -> do
            putStrLn $ "Ejecutando workflow: " ++ workflow_name wf
            let executionLevels = getExecutionOrder (tasks wf)
            executeTasks db executionLevels
        Nothing -> putStrLn "Error en la lectura"

