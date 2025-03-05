{-# LANGUAGE OverloadedStrings #-}

module Database (DB, initDB, saveTaskStatus, getTaskStatus) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Control.Exception (bracket)

-- Tipo de conexión a la base de datos
type DB = Connection

-- Estructura para mapear filas de la base de datos
data TaskRecord = TaskRecord String Bool deriving (Show)

instance FromRow TaskRecord where
    fromRow = TaskRecord <$> field <*> field

-- Inicializa la base de datos y crea la tabla si no existe
initDB :: IO DB
initDB = do
    conn <- open "workflow.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS tasks (name TEXT PRIMARY KEY, status BOOLEAN)"
    return conn

-- Guarda o actualiza el estado de una tarea en la base de datos
saveTaskStatus :: DB -> String -> Bool -> IO ()
saveTaskStatus conn taskName status = do
    execute conn "INSERT INTO tasks (name, status) VALUES (?, ?) ON CONFLICT(name) DO UPDATE SET status=excluded.status" (taskName, status)

-- Obtiene el estado de una tarea desde la base de datos
getTaskStatus :: DB -> String -> IO (Maybe Bool)
getTaskStatus conn taskName = do
    results <- query conn "SELECT status FROM tasks WHERE name = ?" (Only taskName) :: IO [Only Bool]
    return $ case results of
        [Only status] -> Just status
        _ -> Nothing
