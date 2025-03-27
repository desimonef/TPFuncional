module Monad(ExecutionState, ExecutionMonad, DatabaseMonad, logMsg, updateState, getState, runDB) where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.IO.Class
import Types(TaskOutput)
import Control.Monad.Reader
import Database.SQLite.Simple
import Control.Monad.IO.Class (MonadIO)

-- | Tipo de la mónada de base de datos
type DatabaseMonad = ReaderT Connection IO

-- | Ejecuta una acción en DatabaseM manejando la conexión automáticamente
runDB :: DatabaseMonad a -> IO a
runDB action = do
  conn <- open "workflows.db"
  result <- runReaderT action conn
  close conn
  return result

-- Tipo para los logs de ejecución
type ExecutionLog = [String]

-- Estado: Guarda el estado de cada tarea ("success", "failed")
type ExecutionState = [(String, TaskOutput)]

-- Combinación de `StateT` y `WriterT` con IO
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