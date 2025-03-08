{-# LANGUAGE DeriveGeneric #-}

module Workflows (Workflow(..), Task(..), buildTaskGraph, executeFromGraph) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import System.FilePath (takeExtension)
import Control.Monad.State
import Database (DB)
import Execution (executeScript)

-- Estado de ejecución: mapea tarea → output generado
type ExecutionState = StateT [(String, String)] IO

-- Definición de una tarea
data Task = Task {
    name :: String,
    command :: String,
    input :: [Maybe String],
    output :: Maybe String,
    depends_on :: [String]
} deriving (Show, Generic)

instance FromJSON Task

-- Definición del workflow
data Workflow = Workflow {
    workflow_name :: String,
    tasks :: [Task]
} deriving (Show, Generic)

instance FromJSON Workflow

data TaskNode = TaskNode {
    task :: Task,
    dependencies :: [TaskNode]  -- Referencias directas en lugar de nombres
} deriving (Show)

data TaskGraph = TaskGraph { 
    taskMap :: [(String, TaskNode)]         
} deriving (Show)

buildTaskGraph :: [Task] -> TaskGraph
buildTaskGraph tasks =
    let adjacencyList = [(name t, depends_on t) | t <- tasks]
        taskNodes = [(name t, TaskNode t []) | t <- tasks]
        populatedNodes = map (\(n, node) -> (n, populateDependencies node adjacencyList taskNodes)) taskNodes
    in TaskGraph populatedNodes

populateDependencies :: TaskNode -> [(String, [String])] -> [(String, TaskNode)] -> TaskNode
populateDependencies node adjacency taskNodes =
    let taskMap = M.fromList taskNodes
        depNames = fromMaybe [] (lookup (name . task $ node) adjacency)
        depNodes = mapMaybe (`M.lookup` taskMap) depNames
    in node { dependencies = depNodes }


topologicalSort :: TaskGraph -> [TaskNode]
topologicalSort (TaskGraph taskMap) = reverse (dfsAll (map snd taskMap) [])

dfsAll :: [TaskNode] -> [TaskNode] -> [TaskNode]
dfsAll [] visited = visited
dfsAll (node:rest) visited
    | taskName node `elem` map taskName visited = dfsAll rest visited
    | otherwise = dfsAll rest (dfs (dependencies node) visited ++ [node])

dfs :: [TaskNode] -> [TaskNode] -> [TaskNode]
dfs [] visited = visited
dfs (x:xs) visited
    | taskName x `elem` map taskName visited = dfs xs visited
    | otherwise = dfs xs (x : visited)

taskName :: TaskNode -> String
taskName = name . task

executeFromGraph :: DB -> Workflow -> IO ()
executeFromGraph db (Workflow _ tasks) = do
    let graph = buildTaskGraph tasks
    let order = topologicalSort graph
    evalStateT (executeWithGraph db order) []

executeWithGraph :: DB -> [TaskNode] -> ExecutionState ()
executeWithGraph _ [] = liftIO $ putStrLn "Workflow completado!"
executeWithGraph db (node:rest) = do
    state <- get
    let t = task node
        cmd = command t
        args = resolveInputs t state
    result <- liftIO $ executeScript cmd args
    case result of
        Left err  -> liftIO $ putStrLn err
        Right out -> do
            put $ case output t of
                Just outFile -> (taskName node, outFile) : state
                Nothing -> (taskName node, out) : state
            executeWithGraph db rest

resolveInputs :: Task -> [(String, String)] -> [String]
resolveInputs task state = concatMap resolveInput (input task)
  where
    resolveInput Nothing = []
    resolveInput (Just ('@':taskName)) = maybe [] (:[]) (lookup taskName state)
    resolveInput (Just inp) = [inp]

