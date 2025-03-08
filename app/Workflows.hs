{-# LANGUAGE DeriveGeneric #-}

module Workflows (Workflow(..), Task(..), buildTaskGraph, executeWorkflow) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import qualified Data.Set as S
import qualified Data.Map as M
import System.FilePath (takeExtension)
import Control.Monad.State
import Data.Maybe (fromMaybe, mapMaybe)
import Database (DB)
import Execution (TaskOutput(..), Task(..), executeScript)

type ExecutionState = StateT [(String, String)] IO

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
topologicalSort (TaskGraph taskMap) = dfsAll (map snd taskMap) []

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

executeWorkflow :: DB -> Workflow -> IO ()
executeWorkflow db (Workflow _ tasks) = do
    let graph = buildTaskGraph tasks
    let order = topologicalSort graph
    putStrLn "Orden de ejecución de las tareas:"
    mapM_ (putStrLn . taskName) order  -- 🔹 Imprime cada tarea en el orden en que se ejecutará
    evalStateT (executeWithGraph db order) []

executeWithGraph :: DB -> [TaskNode] -> ExecutionState ()
executeWithGraph _ [] = liftIO $ putStrLn "Workflow completado!"
executeWithGraph db (node:rest) = do
    state <- get
    let t = task node
    result <- liftIO $ executeScript t (resolveInputs t state)
    case result of
        Left err  -> liftIO $ putStrLn err
        Right taskOutput -> do
            let outputValue = case taskOutput of
                    OutputFile outFile -> outFile
                    OutputValue -> "output_" ++ taskName node  -- 🔹 Nombre genérico para valores en memoria
            put ((taskName node, outputValue) : state)
            executeWithGraph db rest

resolveInputs :: Task -> [(String, String)] -> [String]
resolveInputs task state = concatMap resolveInput (input task)
  where
    resolveInput Nothing = []
    resolveInput (Just ('@':taskName)) = case lookup taskName state of
        Just value -> [value]
        Nothing -> []
    resolveInput (Just inp) = [inp]
