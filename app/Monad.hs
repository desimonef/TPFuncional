module Monad(ExecutionState, ExecutionMonad, DatabaseMonad, logMsg, updateState, getState, runDB, runExecutionMonad) where

import Control.Monad.State
import Control.Monad.Writer
import Types(TaskOutput)
import Control.Monad.Reader
import Database.SQLite.Simple

type DatabaseMonad = ReaderT Connection IO

runDB :: DatabaseMonad a -> IO a
runDB action = do
  conn <- open "workflows.db"
  result <- runReaderT action conn
  close conn
  return result

type ExecutionLog = [String]

type ExecutionState = [(String, TaskOutput)]

type ExecutionMonad a = StateT ExecutionState (WriterT ExecutionLog IO) a

-- Agregar un mensaje al log
logMsg :: String -> ExecutionMonad ()
logMsg msg = lift $ tell [msg]

-- Obtener el estado actual
getState :: ExecutionMonad ExecutionState
getState = get

-- Actualizar el estado de una tarea
updateState :: String -> TaskOutput -> ExecutionMonad ()
updateState task result = modify ((task, result) :)

runExecutionMonad :: ExecutionMonad a -> IO (a, [String])
runExecutionMonad action = runWriterT (evalStateT action [])