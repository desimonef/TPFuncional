{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy as B
import System.Process (callCommand)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_, when)
import qualified Data.Map as M
import TaskExecutions (execute)
import System.FilePath (takeExtension)

data Task = Task {
    name :: String,
    command :: String,
    depends_on :: [String]
} deriving (Show, Generic)

instance FromJSON Task

data Workflow = Workflow {
    workflow_name :: String,
    tasks :: [Task]
} deriving (Show, Generic)

instance FromJSON Workflow

type TaskStatus = M.Map String Bool  -- Diccionario de tareas ejecutadas

taskRunner :: Task -> TaskStatus -> IO TaskStatus
taskRunner task statusMap = do
    let depsCompleted = all (\dep -> M.findWithDefault False dep statusMap) (depends_on task)
    if depsCompleted then do
        putStrLn $ "Ejecutando tarea: " ++ name task
        let cmd = command task
        if isScript cmd
            then execute cmd  -- Ejecutar usando TaskExecutions si es un script
            else callCommand cmd  -- Ejecutar como comando normal
        return $ M.insert (name task) True statusMap  -- Marcar como completada
    else do
        putStrLn $ "Esperando dependencias para: " ++ name task
        threadDelay 2000000  -- Espera 2 segundos antes de reintentar
        return statusMap

-- Función para verificar si el comando es un script basado en su extensión
isScript :: FilePath -> Bool
isScript path = takeExtension path `elem` [".py", ".js", ".sh", ".bat"]


taskExecutor :: [Task] -> TaskStatus -> IO ()
taskExecutor [] _ = putStrLn "Workflow completado!"
taskExecutor tasks statusMap = do
    newStatus <- foldl (\acc task -> acc >>= taskRunner task) (return statusMap) tasks
    let pendingTasks = filter (\t -> not (M.findWithDefault False (name t) newStatus)) tasks
    if null pendingTasks then putStrLn "Todas las tareas completadas!"
    else taskExecutor pendingTasks newStatus


main :: IO ()
main = do
    contents <- B.readFile "workflow.json"
    case decode contents of
        Just wf -> do
            putStrLn $ "Ejecutando workflow: " ++ workflow_name wf
            let initialStatus = M.fromList [(name t, False) | t <- tasks wf]
            taskExecutor (tasks wf) initialStatus
        Nothing -> putStrLn "Error al leer el archivo JSON."