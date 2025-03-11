{-# LANGUAGE DeriveGeneric #-}

module Workflows (buildTaskGraph, executeWorkflow) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import qualified Data.Set as S
import qualified Data.Map as M
import System.FilePath (takeExtension)
import Control.Monad.State
import Data.Maybe (fromMaybe, mapMaybe)
import Database (DB)
import Execution (executeScript, executeWithRetries, checkCondition)
import Types (Workflow(..), Task(..), ExecutionState(..), TaskNode(..), TaskOutput(..), TaskGraph(..), RetryPolicy(..), FailStrategy(..))
import Debug.Trace (trace)


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

executeWorkflow :: DB -> Workflow -> IO Bool
executeWorkflow db (Workflow _ tasks) = do
    let graph = buildTaskGraph tasks
    let order = topologicalSort graph
    putStrLn "Orden de ejecución de las tareas:"
    mapM_ (putStrLn . taskName) order  -- 🔹 Imprime cada tarea en el orden en que se ejecutará
    
    -- Capturar el resultado de la ejecución
    evalStateT (executeWithGraph db order) []



executeWithGraph :: DB -> [TaskNode] -> ExecutionState Bool
executeWithGraph _ [] = do
    liftIO $ putStrLn "Workflow completado!"
    return True
executeWithGraph db (node:rest) = do
    state <- get
    let t = task node
    shouldRun <- liftIO $ checkCondition t state

    if shouldRun
        then do
            let retries = maybe 0 maxRetries (retryPolicy t)
            let strategy = maybe FailWorkflow failStrategy (retryPolicy t)
            result <- liftIO $ executeWithRetries t (resolveInputs t state) retries

            case result of
                Left err -> do
                    liftIO $ putStrLn err
                    case strategy of
                        FailWorkflow -> do
                            liftIO $ putStrLn "Workflow falló debido a una tarea no recuperable."
                            return False
                        ContinueWorkflow -> executeWithGraph db rest
                Right taskOutput -> do
                    let outputValue = case taskOutput of
                            OutputFile outFile -> outFile
                            OutputValue value -> value  -- 🔹 Guardamos el valor real
                    liftIO $ putStrLn $ "🔹 Output de " ++ taskName node ++ ": " ++ outputValue
                    put ((taskName node, outputValue) : state)
                    executeWithGraph db rest
        else executeWithGraph db rest


resolveInputs :: Task -> [(String, String)] -> [String]
resolveInputs task state = 
    let resolved = concatMap resolveInput (input task)
    in trace ("📌 State actual en resolveInputs: " ++ show state) resolved
  where
    resolveInput Nothing = []
    resolveInput (Just ('@':taskName)) = 
        case lookup taskName state of
            Just value -> [value]
            Nothing -> trace ("⚠️ No se encontró " ++ taskName ++ " en state") []
    resolveInput (Just inp) = [inp]
