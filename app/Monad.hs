module Monad(ExecutionState, ExecutionMonad, logMsg, updateState, getState) where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.IO.Class

-- Tipo para los logs de ejecución
type ExecutionLog = [String]

-- Estado: Guarda el estado de cada tarea ("success", "failed")
type ExecutionState = [(String, String)]

-- Combinación de `StateT` y `WriterT` con IO
type ExecutionMonad a = StateT ExecutionState (WriterT ExecutionLog IO) a

-- Agregar un mensaje al log
logMsg :: String -> ExecutionMonad ()
logMsg msg = lift $ tell [msg]

-- Obtener el estado actual
getState :: ExecutionMonad ExecutionState
getState = get

-- Actualizar el estado de una tarea
updateState :: String -> String -> ExecutionMonad ()
updateState task result = modify ((task, result) :)