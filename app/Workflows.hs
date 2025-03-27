{-# LANGUAGE DeriveGeneric #-}

module Workflows (executeWorkflow) where

import GHC.Generics (Generic)
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
import Types (Workflow(..), Task(..), TaskNode(..), TaskInput(..), TaskOutput(..), TaskGraph(..), RetryPolicy(..), FailStrategy(..))
import Monad(ExecutionState, ExecutionMonad, logMsg, updateState, getState)


executeWorkflow :: Workflow -> IO Bool
executeWorkflow (Workflow _ tasks) = do
    let graph = buildTaskGraph tasks
    let order = topologicalSort graph
    (result, logOutput) <- runWriterT (evalStateT (executeWithGraph order) [])
    
    -- Imprimir el log de ejecución
    putStrLn "=== Execution Log ==="
    mapM_ putStrLn logOutput

    return result


executeWithGraph :: [TaskNode] -> ExecutionMonad Bool
executeWithGraph [] = do
    logMsg "Workflow completado!"
    return True
executeWithGraph (node:rest) = do
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
                ContinueWorkflow -> executeWithGraph rest
        Right taskOutput -> do
            updateState (taskName node) taskOutput
            logMsg $ "Tarea " ++ taskName node ++ " finalizada con salida: " ++ show taskOutput
            executeWithGraph rest


resolveInputs :: Task -> ExecutionState -> [TaskInput]
resolveInputs task state = map resolveInput (input task)
  where
    resolveInput :: TaskInput -> TaskInput
    resolveInput (VarInput ('@':taskName)) = 
        case lookup taskName state of
            Just (OutputValue value) -> VarInput value  -- Si la tarea generó un valor, lo usamos como VarInput
            Just (OutputFile _)      -> error $ "Error: Se esperaba un valor, pero el output de " ++ taskName ++ " es un archivo."
            Nothing                  -> error $ "Referencia a tarea desconocida: " ++ taskName

    resolveInput (FileInput ('@':taskName)) =
        case lookup taskName state of
            Just (OutputFile filePath) -> FileInput filePath  -- Si la tarea generó un archivo, lo usamos como FileInput
            Just (OutputValue _)       -> error $ "Error: Se esperaba un archivo, pero el output de " ++ taskName ++ " es un valor."
            Nothing                    -> error $ "Referencia a tarea desconocida: " ++ taskName

    resolveInput ti = ti  -- Si no es una referencia a otra tarea, lo deja igual
