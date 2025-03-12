{-# LANGUAGE DeriveGeneric #-}

module Workflows (executeWorkflow) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import qualified Data.Set as S
import qualified Data.Map as M
import System.FilePath (takeExtension)
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe, mapMaybe)
import Database (DB)
import Execution (executeScript, executeWithRetries)
import Graph(buildTaskGraph, topologicalSort, taskName)
import Types (Workflow(..), Task(..), TaskNode(..), TaskOutput(..), TaskGraph(..), RetryPolicy(..), FailStrategy(..), ExecutionState(..))
import Monad(ExecutionState, ExecutionMonad, logMsg, updateState, getState)


executeWorkflow :: DB -> Workflow -> IO Bool
executeWorkflow db (Workflow _ tasks) = do
    let graph = buildTaskGraph tasks
    let order = topologicalSort graph
    (result, logOutput) <- runWriterT (evalStateT (executeWithGraph db order) [])
    
    -- Imprimir el log de ejecución
    putStrLn "=== Execution Log ==="
    mapM_ putStrLn logOutput

    return result


executeWithGraph :: DB -> [TaskNode] -> ExecutionMonad Bool
executeWithGraph _ [] = do
    logMsg "Workflow completado!"
    return True
executeWithGraph db (node:rest) = do
    state <- getState
    let t = task node
    let retries = maybe 0 maxRetries (retryPolicy t)
    let strategy = maybe FailWorkflow failStrategy (retryPolicy t)

    result <- executeWithRetries t (resolveInputs t state) retries

    case result of
        Left err -> do
            logMsg $ "Fallo en la tarea: " ++ taskName node
            case strategy of
                FailWorkflow -> do
                    logMsg "Finalizando workflow debido a un fallo no recuperable."
                    return False
                ContinueWorkflow -> executeWithGraph db rest
        Right taskOutput -> do
            let outputValue = case taskOutput of
                    OutputFile outFile -> outFile
                    OutputValue outValue -> outValue
            updateState (taskName node) outputValue
            logMsg $ "Tarea " ++ taskName node ++ " finalizada con salida: " ++ outputValue
            executeWithGraph db rest





resolveInputs :: Task -> [(String, String)] -> [String]
resolveInputs task state = concatMap resolveInput (input task)
  where
    resolveInput Nothing = []
    resolveInput (Just ('@':taskName)) = case lookup taskName state of
        Just value -> [value]
        Nothing -> []
    resolveInput (Just inp) = [inp]
